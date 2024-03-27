//! Turn a JavaScript AST, coming from MD(X), into a component.
//!
//! Port of <https://github.com/mdx-js/mdx/blob/main/packages/mdx/lib/plugin/recma-document.js>,
//! by the same author.

use crate::error::ErrorKind;
use crate::hast_util_to_swc::Program;
use crate::swc_utils::{
    bytepos_to_point, create_call_expression, create_ident, create_ident_expression,
    create_null_expression, create_object_expression, create_str, position_opt_to_string,
    prefix_error_with_point, span_to_position,
};
use crate::Error;
use markdown::{
    unist::{Point, Position},
    Location,
};
use swc_core::ecma::ast::{
    AssignPat, BindingIdent, BlockStmt, Callee, CondExpr, Decl, DefaultDecl, ExportDefaultExpr,
    ExportSpecifier, Expr, ExprOrSpread, FnDecl, Function, ImportDecl, ImportDefaultSpecifier,
    ImportNamedSpecifier, ImportPhase, ImportSpecifier, JSXAttrOrSpread, JSXClosingElement,
    JSXElement, JSXElementChild, JSXElementName, JSXOpeningElement, ModuleDecl, ModuleExportName,
    ModuleItem, Param, Pat, ReturnStmt, SpreadElement, Stmt, VarDecl, VarDeclKind, VarDeclarator,
};
/// JSX runtimes (default: `JsxRuntime::Automatic`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serializable", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serializable", serde(rename_all = "camelCase"))]
pub enum JsxRuntime {
    /// Automatic runtime.
    ///
    /// With the automatic runtime, some module is expected to exist somewhere.
    /// That modules is expected to expose a certain API.
    /// The compiler adds an import of that module and compiles JSX away to
    /// function calls that use that API.
    #[default]
    Automatic,
    /// Classic runtime.
    ///
    /// With the classic runtime, you define two values yourself in each file,
    /// which are expected to work a certain way.
    /// The compiler compiles JSX away to function calls using those two values.
    Classic,
}

/// Configuration.
#[derive(Debug, PartialEq, Eq)]
pub struct Options {
    /// Pragma for JSX (used in classic runtime).
    ///
    /// Default: `React.createElement`.
    pub pragma: Option<String>,
    /// Pragma for JSX fragments (used in classic runtime).
    ///
    /// Default: `React.Fragment`.
    pub pragma_frag: Option<String>,
    /// Where to import the identifier of `pragma` from (used in classic runtime).
    ///
    /// Default: `react`.
    pub pragma_import_source: Option<String>,
    /// Place to import automatic JSX runtimes from (used in automatic runtime).
    ///
    /// Default: `react`.
    pub jsx_import_source: Option<String>,
    /// JSX runtime to use.
    ///
    /// Default: `automatic`.
    pub jsx_runtime: Option<JsxRuntime>,
}

impl Default for Options {
    /// Use the automatic JSX runtime with React.
    fn default() -> Self {
        Self {
            pragma: None,
            pragma_frag: None,
            pragma_import_source: None,
            jsx_import_source: None,
            jsx_runtime: Some(JsxRuntime::default()),
        }
    }
}

/// Wrap the SWC ES AST nodes coming from hast into a whole document.
pub fn mdx_plugin_recma_document(
    program: &mut Program,
    options: &Options,
    location: Option<&Location>,
) -> Result<(), Error> {
    // New body children.
    let mut replacements = vec![];

    // Inject JSX configuration comment.
    if let Some(runtime) = &options.jsx_runtime {
        let mut pragmas = vec![];
        let react = &"react".into();
        let create_element = &"React.createElement".into();
        let fragment = &"React.Fragment".into();

        if *runtime == JsxRuntime::Automatic {
            pragmas.push("@jsxRuntime automatic".into());
            pragmas.push(format!(
                "@jsxImportSource {}",
                if let Some(jsx_import_source) = &options.jsx_import_source {
                    jsx_import_source
                } else {
                    react
                }
            ));
        } else {
            pragmas.push("@jsxRuntime classic".into());
            pragmas.push(format!(
                "@jsx {}",
                if let Some(pragma) = &options.pragma {
                    pragma
                } else {
                    create_element
                }
            ));
            pragmas.push(format!(
                "@jsxFrag {}",
                if let Some(pragma_frag) = &options.pragma_frag {
                    pragma_frag
                } else {
                    fragment
                }
            ));
        }

        if !pragmas.is_empty() {
            program.comments.insert(
                0,
                swc_core::common::comments::Comment {
                    kind: swc_core::common::comments::CommentKind::Block,
                    text: pragmas.join(" ").into(),
                    span: swc_core::common::DUMMY_SP,
                },
            );
        }
    }

    // Inject an import in the classic runtime for the pragma (and presumably,
    // fragment).
    if options.jsx_runtime == Some(JsxRuntime::Classic) {
        let pragma = if let Some(pragma) = &options.pragma {
            pragma
        } else {
            "React"
        };
        let sym = pragma.split('.').next().expect("first item always exists");

        replacements.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
            specifiers: vec![ImportSpecifier::Default(ImportDefaultSpecifier {
                local: create_ident(sym),
                span: swc_core::common::DUMMY_SP,
            })],
            src: Box::new(create_str(
                if let Some(source) = &options.pragma_import_source {
                    source
                } else {
                    "react"
                },
            )),
            type_only: false,
            with: None,
            phase: ImportPhase::default(),
            span: swc_core::common::DUMMY_SP,
        })));
    }

    // Find the `export default`, the JSX expression, and leave the rest as it
    // is.
    let mut input = program.module.body.split_off(0);
    input.reverse();
    let mut layout = false;
    let mut layout_position = None;
    let mut content = false;

    while let Some(module_item) = input.pop() {
        match module_item {
            // ```js
            // export default props => <>{props.children}</>
            // ```
            //
            // Treat it as an inline layout declaration.
            //
            // In estree, the below two are the same node (`ExportDefault`).
            ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(decl)) => {
                err_for_double_layout(
                    layout,
                    layout_position.as_ref(),
                    bytepos_to_point(decl.span.lo, location).as_ref(),
                )?;
                layout = true;
                layout_position = span_to_position(&decl.span, location);
                match decl.decl {
                    DefaultDecl::Class(cls) => {
                        replacements.push(create_layout_decl(Expr::Class(cls)));
                    }
                    DefaultDecl::Fn(func) => {
                        replacements.push(create_layout_decl(Expr::Fn(func)));
                    }
                    DefaultDecl::TsInterfaceDecl(_) => {
                        return Err(prefix_error_with_point(
                            ErrorKind::CannotExportTsInterfaceAsDefault.into(),
                            bytepos_to_point(decl.span.lo, location).as_ref(),
                        ));
                    }
                }
            }
            ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(expr)) => {
                err_for_double_layout(
                    layout,
                    layout_position.as_ref(),
                    bytepos_to_point(expr.span.lo, location).as_ref(),
                )?;
                layout = true;
                layout_position = span_to_position(&expr.span, location);
                replacements.push(create_layout_decl(*expr.expr));
            }
            // ```js
            // export {a, b as c} from 'd'
            // export {a, b as c}
            // ```
            ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(mut named_export)) => {
                let mut index = 0;
                let mut id = None;

                while index < named_export.specifiers.len() {
                    let mut take = false;
                    // Note: the `ExportSpecifier::Default`
                    // branch of this looks interesting, but as far as I
                    // understand it *is not* valid ES.
                    // `export a from 'b'` is a syntax error, even in SWC.
                    if let ExportSpecifier::Named(named) = &named_export.specifiers[index] {
                        if let Some(ModuleExportName::Ident(ident)) = &named.exported {
                            if &ident.sym == "default" {
                                // For some reason the AST supports strings
                                // instead of identifiers.
                                // Looks like some TC39 proposal. Ignore for now
                                // and only do things if this is an ID.
                                if let ModuleExportName::Ident(ident) = &named.orig {
                                    err_for_double_layout(
                                        layout,
                                        layout_position.as_ref(),
                                        bytepos_to_point(ident.span.lo, location).as_ref(),
                                    )?;
                                    layout = true;
                                    layout_position = span_to_position(&ident.span, location);
                                    take = true;
                                    id = Some(ident.clone());
                                }
                            }
                        }
                    }

                    if take {
                        named_export.specifiers.remove(index);
                    } else {
                        index += 1;
                    }
                }

                if let Some(id) = id {
                    let source = named_export.src.clone();

                    // If there was just a default export, we can drop the original node.
                    if !named_export.specifiers.is_empty() {
                        // Pass through.
                        replacements.push(ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(
                            named_export,
                        )));
                    }

                    // It’s an `export {x} from 'y'`, so generate an import.
                    if let Some(source) = source {
                        replacements.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                            specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
                                local: create_ident("MDXLayout"),
                                imported: Some(ModuleExportName::Ident(id)),
                                span: swc_core::common::DUMMY_SP,
                                is_type_only: false,
                            })],
                            src: source,
                            type_only: false,
                            with: None,
                            phase: ImportPhase::default(),
                            span: swc_core::common::DUMMY_SP,
                        })));
                    }
                    // It’s an `export {x}`, so generate a variable declaration.
                    else {
                        replacements.push(create_layout_decl(create_ident_expression(&id.sym)));
                    }
                } else {
                    // Pass through.
                    replacements.push(ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(
                        named_export,
                    )));
                }
            }
            ModuleItem::ModuleDecl(ModuleDecl::Import(x)) => {
                // Pass through.
                replacements.push(ModuleItem::ModuleDecl(ModuleDecl::Import(x)));
            }
            ModuleItem::ModuleDecl(
                ModuleDecl::ExportDecl(_)
                | ModuleDecl::ExportAll(_)
                | ModuleDecl::TsImportEquals(_)
                | ModuleDecl::TsExportAssignment(_)
                | ModuleDecl::TsNamespaceExport(_),
            ) => {
                // Pass through.
                replacements.push(module_item);
            }
            ModuleItem::Stmt(Stmt::Expr(expr_stmt)) => {
                match *expr_stmt.expr {
                    Expr::JSXElement(elem) => {
                        content = true;
                        replacements.append(&mut create_mdx_content(
                            Some(Expr::JSXElement(elem)),
                            layout,
                        ));
                    }
                    Expr::JSXFragment(mut frag) => {
                        // Unwrap if possible.
                        if frag.children.len() == 1 {
                            let item = frag.children.pop().unwrap();

                            if let JSXElementChild::JSXElement(elem) = item {
                                content = true;
                                replacements.append(&mut create_mdx_content(
                                    Some(Expr::JSXElement(elem)),
                                    layout,
                                ));
                                continue;
                            }

                            frag.children.push(item);
                        }

                        content = true;
                        replacements.append(&mut create_mdx_content(
                            Some(Expr::JSXFragment(frag)),
                            layout,
                        ));
                    }
                    _ => {
                        // Pass through.
                        replacements.push(ModuleItem::Stmt(Stmt::Expr(expr_stmt)));
                    }
                }
            }
            ModuleItem::Stmt(stmt) => {
                replacements.push(ModuleItem::Stmt(stmt));
            }
        }
    }

    // Generate an empty component.
    if !content {
        replacements.append(&mut create_mdx_content(None, layout));
    }

    // ```jsx
    // export default MDXContent
    // ```
    replacements.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(
        ExportDefaultExpr {
            expr: Box::new(create_ident_expression("MDXContent")),
            span: swc_core::common::DUMMY_SP,
        },
    )));

    program.module.body = replacements;

    Ok(())
}

/// Create a content component.
fn create_mdx_content(expr: Option<Expr>, has_internal_layout: bool) -> Vec<ModuleItem> {
    // ```jsx
    // <MDXLayout {...props}>xxx</MDXLayout>
    // ```
    let mut result = Expr::JSXElement(Box::new(JSXElement {
        opening: JSXOpeningElement {
            name: JSXElementName::Ident(create_ident("MDXLayout")),
            attrs: vec![JSXAttrOrSpread::SpreadElement(SpreadElement {
                dot3_token: swc_core::common::DUMMY_SP,
                expr: Box::new(create_ident_expression("props")),
            })],
            self_closing: false,
            type_args: None,
            span: swc_core::common::DUMMY_SP,
        },
        closing: Some(JSXClosingElement {
            name: JSXElementName::Ident(create_ident("MDXLayout")),
            span: swc_core::common::DUMMY_SP,
        }),
        // ```jsx
        // <_createMdxContent {...props} />
        // ```
        children: vec![JSXElementChild::JSXElement(Box::new(JSXElement {
            opening: JSXOpeningElement {
                name: JSXElementName::Ident(create_ident("_createMdxContent")),
                attrs: vec![JSXAttrOrSpread::SpreadElement(SpreadElement {
                    dot3_token: swc_core::common::DUMMY_SP,
                    expr: Box::new(create_ident_expression("props")),
                })],
                self_closing: true,
                type_args: None,
                span: swc_core::common::DUMMY_SP,
            },
            closing: None,
            children: vec![],
            span: swc_core::common::DUMMY_SP,
        }))],
        span: swc_core::common::DUMMY_SP,
    }));

    if !has_internal_layout {
        // ```jsx
        // MDXLayout ? <MDXLayout>xxx</MDXLayout> : _createMdxContent(props)
        // ```
        result = Expr::Cond(CondExpr {
            test: Box::new(create_ident_expression("MDXLayout")),
            cons: Box::new(result),
            alt: Box::new(create_call_expression(
                Callee::Expr(Box::new(create_ident_expression("_createMdxContent"))),
                vec![ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_ident_expression("props")),
                }],
            )),
            span: swc_core::common::DUMMY_SP,
        });
    }

    // ```jsx
    // function _createMdxContent(props) {
    //   return xxx
    // }
    // ```
    let create_mdx_content = ModuleItem::Stmt(Stmt::Decl(Decl::Fn(FnDecl {
        ident: create_ident("_createMdxContent"),
        declare: false,
        function: Box::new(Function {
            params: vec![Param {
                pat: Pat::Ident(BindingIdent {
                    id: create_ident("props"),
                    type_ann: None,
                }),
                decorators: vec![],
                span: swc_core::common::DUMMY_SP,
            }],
            decorators: vec![],
            body: Some(BlockStmt {
                stmts: vec![Stmt::Return(ReturnStmt {
                    arg: Some(Box::new(expr.unwrap_or_else(create_null_expression))),
                    span: swc_core::common::DUMMY_SP,
                })],
                span: swc_core::common::DUMMY_SP,
            }),
            is_generator: false,
            is_async: false,
            type_params: None,
            return_type: None,
            span: swc_core::common::DUMMY_SP,
        }),
    })));

    // ```jsx
    // function MDXContent(props = {}) {
    //   return <MDXLayout>xxx</MDXLayout>
    // }
    // ```
    let mdx_content = ModuleItem::Stmt(Stmt::Decl(Decl::Fn(FnDecl {
        ident: create_ident("MDXContent"),
        declare: false,
        function: Box::new(Function {
            params: vec![Param {
                pat: Pat::Assign(AssignPat {
                    left: Box::new(Pat::Ident(BindingIdent {
                        id: create_ident("props"),
                        type_ann: None,
                    })),
                    right: Box::new(create_object_expression(vec![])),
                    span: swc_core::common::DUMMY_SP,
                }),
                decorators: vec![],
                span: swc_core::common::DUMMY_SP,
            }],
            decorators: vec![],
            body: Some(BlockStmt {
                stmts: vec![Stmt::Return(ReturnStmt {
                    arg: Some(Box::new(result)),
                    span: swc_core::common::DUMMY_SP,
                })],
                span: swc_core::common::DUMMY_SP,
            }),
            is_generator: false,
            is_async: false,
            type_params: None,
            return_type: None,
            span: swc_core::common::DUMMY_SP,
        }),
    })));

    vec![create_mdx_content, mdx_content]
}

/// Create a layout, inside the document.
fn create_layout_decl(expr: Expr) -> ModuleItem {
    // ```jsx
    // const MDXLayout = xxx
    // ```
    ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
        kind: VarDeclKind::Const,
        declare: false,
        decls: vec![VarDeclarator {
            name: Pat::Ident(BindingIdent {
                id: create_ident("MDXLayout"),
                type_ann: None,
            }),
            init: Some(Box::new(expr)),
            span: swc_core::common::DUMMY_SP,
            definite: false,
        }],
        span: swc_core::common::DUMMY_SP,
    }))))
}

/// Create an error about multiple layouts.
fn err_for_double_layout(
    layout: bool,
    previous: Option<&Position>,
    at: Option<&Point>,
) -> Result<(), Error> {
    if layout {
        Err(prefix_error_with_point(
            ErrorKind::CannotSpecifyMultipleLayouts {
                previous: previous.cloned(),
            }
            .into(),
            at,
        ))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hast_util_to_swc::hast_util_to_swc;
    use crate::mdast_util_to_hast::mdast_util_to_hast;
    use crate::mdx_plugin_recma_document::{mdx_plugin_recma_document, Options as DocumentOptions};
    use crate::swc::{parse_esm, parse_expression, serialize};
    use crate::swc_utils::create_bool_expression;
    use crate::Error;
    use markdown::{to_mdast, ParseOptions};
    use pretty_assertions::assert_eq;
    use swc_core::ecma::ast::{
        EmptyStmt, ExportDefaultDecl, ExprStmt, JSXClosingFragment, JSXFragment,
        JSXOpeningFragment, JSXText, Module, TsInterfaceBody, TsInterfaceDecl, WhileStmt,
    };

    fn compile(value: &str) -> Result<String, Error> {
        let location = Location::new(value.as_bytes());
        let mdast = to_mdast(
            value,
            &ParseOptions {
                mdx_esm_parse: Some(Box::new(parse_esm)),
                mdx_expression_parse: Some(Box::new(parse_expression)),
                ..ParseOptions::mdx()
            },
        )?;
        let hast = mdast_util_to_hast(&mdast);
        let mut program = hast_util_to_swc(&hast, None, Some(&location))?;
        mdx_plugin_recma_document(&mut program, &DocumentOptions::default(), Some(&location))?;
        Ok(serialize(&mut program.module, Some(&program.comments)))
    }

    #[test]
    fn small() -> Result<(), Error> {
        assert_eq!(
            compile("# hi\n\nAlpha *bravo* **charlie**.")?,
            "function _createMdxContent(props) {
    return <><h1>{\"hi\"}</h1>{\"\\n\"}<p>{\"Alpha \"}<em>{\"bravo\"}</em>{\" \
             \"}<strong>{\"charlie\"}</strong>{\".\"}</p></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support a small program",
        );

        Ok(())
    }

    #[test]
    fn import() -> Result<(), Error> {
        assert_eq!(
            compile("import a from 'b'\n\n# {a}")?,
            "import a from 'b';
function _createMdxContent(props) {
    return <h1>{a}</h1>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support an import",
        );

        Ok(())
    }

    #[test]
    fn export() -> Result<(), Error> {
        assert_eq!(
            compile("export * from 'a'\n\n# b")?,
            "export * from 'a';
function _createMdxContent(props) {
    return <h1>{\"b\"}</h1>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support an export all",
        );

        assert_eq!(
            compile("export function a() {}")?,
            "export function a() {}
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support an export declaration",
        );

        assert_eq!(
            compile("export class A {}")?,
            "export class A {
}
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support an export class",
        );

        Ok(())
    }

    #[test]
    fn export_default() -> Result<(), Error> {
        assert_eq!(
            compile("export default a")?,
            "const MDXLayout = a;
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should support an export default expression",
        );

        assert_eq!(
            compile("export default function () {}")?,
            "const MDXLayout = function() {};
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should support an export default declaration",
        );

        assert_eq!(
            compile("export default class A {}")?,
            "const MDXLayout = class A {
};
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should support an export default class",
        );

        Ok(())
    }

    #[test]
    fn named_exports() -> Result<(), Error> {
        assert_eq!(
            compile("export {a, b as default}")?,
            "export { a };
const MDXLayout = b;
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should support a named export w/o source, w/ a default specifier",
        );

        assert_eq!(
            compile("export {a}")?,
            "export { a };
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support a named export w/o source, w/o a default specifier",
        );

        assert_eq!(
            compile("export {}")?,
            "export { };
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support a named export w/o source, w/o a specifiers",
        );

        assert_eq!(
            compile("export {a, b as default} from 'c'")?,
            "export { a } from 'c';
import { b as MDXLayout } from 'c';
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should support a named export w/ source, w/ a default specifier",
        );

        assert_eq!(
            compile("export {a} from 'b'")?,
            "export { a } from 'b';
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support a named export w/ source, w/o a default specifier",
        );

        assert_eq!(
            compile("export {} from 'a'")?,
            "export { } from 'a';
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should support a named export w/ source, w/o a specifiers",
        );

        Ok(())
    }

    #[test]
    fn multiple_layouts() {
        assert_eq!(
            compile("export default a = 1\n\nexport default b = 2")
                .err()
                .unwrap()
                .to_string(),
            "3:1: Cannot specify multiple layouts (previous: 1:1-1:21)",
            "should crash on multiple layouts"
        );
    }

    #[test]
    fn ts_default_interface_declaration() {
        assert_eq!(
            mdx_plugin_recma_document(
                &mut Program {
                    path: None,
                    comments: vec![],
                    module: Module {
                        span: swc_core::common::DUMMY_SP,
                        shebang: None,
                        body: vec![ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(
                            ExportDefaultDecl {
                                span: swc_core::common::DUMMY_SP,
                                decl: DefaultDecl::TsInterfaceDecl(Box::new(TsInterfaceDecl {
                                    span: swc_core::common::DUMMY_SP,
                                    id: create_ident("a"),
                                    declare: true,
                                    type_params: None,
                                    extends: vec![],
                                    body: TsInterfaceBody {
                                        span: swc_core::common::DUMMY_SP,
                                        body: vec![]
                                    }
                                }))
                            }
                        ))]
                    }
                },
                &Options::default(),
                None
            )
            .err()
            .unwrap()
            .to_string(),
            "0:0: Cannot use TypeScript interface declarations as default export in MDX files. \
             The default export is reserved for a layout, which must be a component",
            "should crash on a TypeScript default interface declaration"
        );
    }

    #[test]
    fn statement_pass_through() -> Result<(), Error> {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: swc_core::common::DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::While(WhileStmt {
                    span: swc_core::common::DUMMY_SP,
                    test: Box::new(create_bool_expression(true)),
                    body: Box::new(Stmt::Empty(EmptyStmt {
                        span: swc_core::common::DUMMY_SP,
                    })),
                }))],
            },
        };

        mdx_plugin_recma_document(&mut program, &Options::default(), None)?;

        assert_eq!(
            serialize(&mut program.module, None),
            "while(true);
function _createMdxContent(props) {
    return null;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should pass statements through"
        );

        Ok(())
    }

    #[test]
    fn expression_pass_through() -> Result<(), Error> {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: swc_core::common::DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: swc_core::common::DUMMY_SP,
                    expr: Box::new(create_bool_expression(true)),
                }))],
            },
        };

        mdx_plugin_recma_document(&mut program, &Options::default(), None)?;

        assert_eq!(
            serialize(&mut program.module, None),
            "true;
function _createMdxContent(props) {
    return null;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should pass expressions through"
        );

        Ok(())
    }

    #[test]
    fn fragment_non_element_single_child() -> Result<(), Error> {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: swc_core::common::DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: swc_core::common::DUMMY_SP,
                    expr: Box::new(Expr::JSXFragment(JSXFragment {
                        span: swc_core::common::DUMMY_SP,
                        opening: JSXOpeningFragment {
                            span: swc_core::common::DUMMY_SP,
                        },
                        closing: JSXClosingFragment {
                            span: swc_core::common::DUMMY_SP,
                        },
                        children: vec![JSXElementChild::JSXText(JSXText {
                            value: "a".into(),
                            span: swc_core::common::DUMMY_SP,
                            raw: "a".into(),
                        })],
                    })),
                }))],
            },
        };

        mdx_plugin_recma_document(&mut program, &Options::default(), None)?;

        assert_eq!(
            serialize(&mut program.module, None),
            "function _createMdxContent(props) {
    return <>a</>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should pass a fragment with a single child that isn’t an element through"
        );

        Ok(())
    }

    #[test]
    fn element() -> Result<(), Error> {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: swc_core::common::DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: swc_core::common::DUMMY_SP,
                    expr: Box::new(Expr::JSXElement(Box::new(JSXElement {
                        span: swc_core::common::DUMMY_SP,
                        opening: JSXOpeningElement {
                            name: JSXElementName::Ident(create_ident("a")),
                            attrs: vec![],
                            self_closing: false,
                            type_args: None,
                            span: swc_core::common::DUMMY_SP,
                        },
                        closing: Some(JSXClosingElement {
                            name: JSXElementName::Ident(create_ident("a")),
                            span: swc_core::common::DUMMY_SP,
                        }),
                        children: vec![JSXElementChild::JSXText(JSXText {
                            value: "b".into(),
                            span: swc_core::common::DUMMY_SP,
                            raw: "b".into(),
                        })],
                    }))),
                }))],
            },
        };

        mdx_plugin_recma_document(&mut program, &Options::default(), None)?;

        assert_eq!(
            serialize(&mut program.module, None),
            "function _createMdxContent(props) {
    return <a>b</a>;
}
function MDXContent(props = {}) {
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : \
             _createMdxContent(props);
}
export default MDXContent;
",
            "should pass an element through"
        );

        Ok(())
    }
}
