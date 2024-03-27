//! Turn JSX into function calls.

use crate::error::ErrorKind;
use crate::hast_util_to_swc::Program;
use crate::mdx_plugin_recma_document::JsxRuntime;
use crate::swc_utils::{
    bytepos_to_point, create_bool_expression, create_call_expression, create_ident,
    create_ident_expression, create_member_expression_from_str, create_null_expression,
    create_num_expression, create_object_expression, create_prop_name, create_str,
    create_str_expression, jsx_attribute_name_to_prop_name, jsx_element_name_to_expression,
    prefix_error_with_point, span_to_position,
};
use crate::Error;
use core::str;
use markdown::Location;
use swc_core::common::{
    comments::{Comment, CommentKind},
    util::take::Take,
};
use swc_core::ecma::ast::{
    ArrayLit, CallExpr, Callee, Expr, ExprOrSpread, ImportDecl, ImportNamedSpecifier, ImportPhase,
    ImportSpecifier, JSXAttrName, JSXAttrOrSpread, JSXAttrValue, JSXElement, JSXElementChild,
    JSXExpr, JSXFragment, KeyValueProp, Lit, ModuleDecl, ModuleExportName, ModuleItem, Prop,
    PropName, PropOrSpread, ThisExpr,
};
use swc_core::ecma::visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

/// Configuration.
#[derive(Debug, Default, Clone)]
pub struct Options {
    /// Whether to add extra information to error messages in generated code.
    pub development: bool,
}

/// Compile JSX away to function calls.
pub fn swc_util_build_jsx(
    program: &mut Program,
    options: &Options,
    location: Option<&Location>,
) -> Result<(), Error> {
    let directives: Directives = find_directives(&program.comments, location)?;

    let mut state = State {
        development: options.development,
        filepath: program.path.clone(),
        location,
        automatic: !matches!(directives.runtime, Some(JsxRuntime::Classic)),
        import_fragment: false,
        import_jsx: false,
        import_jsxs: false,
        import_jsx_dev: false,
        create_element_expression: create_member_expression_from_str(
            &directives
                .pragma
                .unwrap_or_else(|| "React.createElement".into()),
        ),
        fragment_expression: create_member_expression_from_str(
            &directives
                .pragma_frag
                .unwrap_or_else(|| "React.Fragment".into()),
        ),
        error: None,
    };

    // Rewrite JSX and gather specifiers to import.
    program.module.visit_mut_with(&mut state);

    if let Some(err) = state.error.take() {
        return Err(err);
    }

    let mut specifiers = vec![];

    if state.import_fragment {
        specifiers.push(ImportSpecifier::Named(ImportNamedSpecifier {
            local: create_ident("_Fragment"),
            imported: Some(ModuleExportName::Ident(create_ident("Fragment"))),
            span: swc_core::common::DUMMY_SP,
            is_type_only: false,
        }));
    }

    if state.import_jsx {
        specifiers.push(ImportSpecifier::Named(ImportNamedSpecifier {
            local: create_ident("_jsx"),
            imported: Some(ModuleExportName::Ident(create_ident("jsx"))),
            span: swc_core::common::DUMMY_SP,
            is_type_only: false,
        }));
    }

    if state.import_jsxs {
        specifiers.push(ImportSpecifier::Named(ImportNamedSpecifier {
            local: create_ident("_jsxs"),
            imported: Some(ModuleExportName::Ident(create_ident("jsxs"))),
            span: swc_core::common::DUMMY_SP,
            is_type_only: false,
        }));
    }

    if state.import_jsx_dev {
        specifiers.push(ImportSpecifier::Named(ImportNamedSpecifier {
            local: create_ident("_jsxDEV"),
            imported: Some(ModuleExportName::Ident(create_ident("jsxDEV"))),
            span: swc_core::common::DUMMY_SP,
            is_type_only: false,
        }));
    }

    if !specifiers.is_empty() {
        program.module.body.insert(
            0,
            ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                specifiers,
                src: Box::new(create_str(&format!(
                    "{}{}",
                    directives.import_source.unwrap_or_else(|| "react".into()),
                    if options.development {
                        "/jsx-dev-runtime"
                    } else {
                        "/jsx-runtime"
                    }
                ))),
                type_only: false,
                with: None,
                phase: ImportPhase::default(),
                span: swc_core::common::DUMMY_SP,
            })),
        );
    }

    Ok(())
}

/// Info gathered from comments.
#[derive(Debug, Default, Clone)]
struct Directives {
    /// Inferred JSX runtime.
    runtime: Option<JsxRuntime>,
    /// Inferred automatic JSX import source.
    import_source: Option<String>,
    /// Inferred classic JSX pragma.
    pragma: Option<String>,
    /// Inferred classic JSX pragma fragment.
    pragma_frag: Option<String>,
}

/// Context.
#[derive(Debug, Clone)]
struct State<'a> {
    /// Location info.
    location: Option<&'a Location>,
    /// Whether walking the tree produced an error.
    error: Option<Error>,
    /// Path to file.
    filepath: Option<String>,
    /// Whether the user is in development mode.
    development: bool,
    /// Whether to import `Fragment`.
    import_fragment: bool,
    /// Whether to import `jsx`.
    import_jsx: bool,
    /// Whether to import `jsxs`.
    import_jsxs: bool,
    /// Whether to import `jsxDEV`.
    import_jsx_dev: bool,
    /// Whether we’re building in the automatic or classic runtime.
    automatic: bool,
    /// Expression (ident or member) to use for `createElement` calls in
    /// the classic runtime.
    create_element_expression: Expr,
    /// Expression (ident or member) to use as fragment symbol in the classic
    /// runtime.
    fragment_expression: Expr,
}

impl<'a> State<'a> {
    /// Turn an attribute value into an expression.
    fn jsx_attribute_value_to_expression(
        &mut self,
        value: Option<JSXAttrValue>,
    ) -> Result<Expr, Error> {
        match value {
            // Boolean prop.
            None => Ok(create_bool_expression(true)),
            Some(JSXAttrValue::JSXExprContainer(expression_container)) => {
                match expression_container.expr {
                    JSXExpr::JSXEmptyExpr(_) => {
                        unreachable!("Cannot use empty JSX expressions in attribute values");
                    }
                    JSXExpr::Expr(expression) => Ok(*expression),
                }
            }
            Some(JSXAttrValue::Lit(mut literal)) => {
                // Remove `raw` so we don’t get character references in strings.
                if let Lit::Str(string_literal) = &mut literal {
                    string_literal.raw = None;
                }

                Ok(Expr::Lit(literal))
            }
            Some(JSXAttrValue::JSXFragment(fragment)) => self.jsx_fragment_to_expression(fragment),
            Some(JSXAttrValue::JSXElement(element)) => self.jsx_element_to_expression(*element),
        }
    }

    /// Turn children of elements or fragments into expressions.
    fn jsx_children_to_expressions(
        &mut self,
        mut children: Vec<JSXElementChild>,
    ) -> Result<Vec<Expr>, Error> {
        let mut result = vec![];
        children.reverse();
        while let Some(child) = children.pop() {
            match child {
                JSXElementChild::JSXSpreadChild(child) => {
                    let lo = child.span.lo;
                    let start = bytepos_to_point(lo, self.location);
                    let reason = prefix_error_with_point(
                        ErrorKind::JsxSpreadNotSupported.into(),
                        start.as_ref(),
                    );
                    return Err(reason);
                }
                JSXElementChild::JSXExprContainer(container) => {
                    if let JSXExpr::Expr(expression) = container.expr {
                        result.push(*expression);
                    }
                }
                JSXElementChild::JSXText(text) => {
                    let value = jsx_text_to_value(text.value.as_ref());
                    if !value.is_empty() {
                        result.push(create_str_expression(&value));
                    }
                }
                JSXElementChild::JSXElement(element) => {
                    result.push(self.jsx_element_to_expression(*element)?);
                }
                JSXElementChild::JSXFragment(fragment) => {
                    result.push(self.jsx_fragment_to_expression(fragment)?);
                }
            }
        }

        Ok(result)
    }

    /// Turn optional attributes, and perhaps children (when automatic), into props.
    fn jsx_attributes_to_expressions(
        &mut self,
        attributes: Option<Vec<JSXAttrOrSpread>>,
        children: Option<Vec<Expr>>,
    ) -> Result<(Option<Expr>, Option<Expr>), Error> {
        let mut objects = vec![];
        let mut fields = vec![];
        let mut spread = false;
        let mut key = None;

        if let Some(mut attributes) = attributes {
            attributes.reverse();

            // Place props in the right order, because we might have duplicates
            // in them and what’s spread in.
            while let Some(attribute) = attributes.pop() {
                match attribute {
                    JSXAttrOrSpread::SpreadElement(spread_element) => {
                        if !fields.is_empty() {
                            objects.push(create_object_expression(fields));
                            fields = vec![];
                        }

                        objects.push(*spread_element.expr);
                        spread = true;
                    }
                    JSXAttrOrSpread::JSXAttr(jsx_attribute) => {
                        let value = self.jsx_attribute_value_to_expression(jsx_attribute.value)?;
                        let mut value = Some(value);

                        if let JSXAttrName::Ident(ident) = &jsx_attribute.name {
                            if self.automatic && &ident.sym == "key" {
                                if spread {
                                    let lo = jsx_attribute.span.lo;
                                    let start = bytepos_to_point(lo, self.location);
                                    let reason = prefix_error_with_point(
                                        "Expected `key` to come before any spread expressions",
                                        start.as_ref(),
                                    );
                                    return Err(reason);
                                }

                                // Take the value out, so we don’t add it as a prop.
                                key = value.take();
                            }
                        }

                        if let Some(value) = value {
                            fields.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                                KeyValueProp {
                                    key: jsx_attribute_name_to_prop_name(jsx_attribute.name),
                                    value: Box::new(value),
                                },
                            ))));
                        }
                    }
                }
            }
        }

        // In the automatic runtime, add children as a prop.
        if let Some(mut children) = children {
            let value = if children.is_empty() {
                None
            } else if children.len() == 1 {
                Some(children.pop().unwrap())
            } else {
                let mut elements = vec![];
                children.reverse();
                while let Some(child) = children.pop() {
                    elements.push(Some(ExprOrSpread {
                        spread: None,
                        expr: Box::new(child),
                    }));
                }
                let lit = ArrayLit {
                    elems: elements,
                    span: swc_core::common::DUMMY_SP,
                };
                Some(Expr::Array(lit))
            };

            if let Some(value) = value {
                fields.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: create_prop_name("children"),
                    value: Box::new(value),
                }))));
            }
        }

        // Add remaining fields.
        if !fields.is_empty() {
            objects.push(create_object_expression(fields));
        }

        let props = if objects.is_empty() {
            None
        } else if objects.len() == 1 {
            Some(objects.pop().unwrap())
        } else {
            let mut args = vec![];
            objects.reverse();

            // Don’t mutate the first object, shallow clone into a new
            // object instead.
            if !matches!(objects.last(), Some(Expr::Object(_))) {
                objects.push(create_object_expression(vec![]));
            }

            while let Some(object) = objects.pop() {
                args.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(object),
                });
            }

            let callee = Callee::Expr(Box::new(create_member_expression_from_str("Object.assign")));
            Some(create_call_expression(callee, args))
        };

        Ok((props, key))
    }

    /// Turn the parsed parts from fragments or elements into a call.
    fn jsx_expressions_to_call(
        &mut self,
        span: &swc_core::common::Span,
        name: Expr,
        attributes: Option<Vec<JSXAttrOrSpread>>,
        mut children: Vec<Expr>,
    ) -> Result<Expr, Error> {
        let (callee, parameters) = if self.automatic {
            let is_static_children = children.len() > 1;
            let (props, key) = self.jsx_attributes_to_expressions(attributes, Some(children))?;
            let mut parameters = vec![
                // Component name.
                //
                // ```javascript
                // Component
                // ```
                ExprOrSpread {
                    spread: None,
                    expr: Box::new(name),
                },
                // Props (including children) or empty object.
                //
                // ```javascript
                // Object.assign({x: true, y: 'z'}, {children: […]})
                // {x: true, y: 'z'}
                // {}
                // ```
                ExprOrSpread {
                    spread: None,
                    expr: Box::new(props.unwrap_or_else(|| create_object_expression(vec![]))),
                },
            ];

            // Key or, in development, undefined.
            //
            // ```javascript
            // "xyz"
            // ```
            if let Some(key) = key {
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(key),
                });
            } else if self.development {
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_ident_expression("undefined")),
                });
            }

            if self.development {
                // Static children (or not).
                //
                // ```javascript
                // true
                // ```
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_bool_expression(is_static_children)),
                });

                let filename = if let Some(value) = &self.filepath {
                    create_str_expression(value)
                } else {
                    create_str_expression("<source.js>")
                };
                let prop = PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: PropName::Ident(create_ident("fileName")),
                    value: Box::new(filename),
                })));
                let mut meta_fields = vec![prop];

                if let Some(position) = span_to_position(span, self.location) {
                    meta_fields.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                        key: create_prop_name("lineNumber"),
                        value: Box::new(create_num_expression(position.start.line as f64)),
                    }))));

                    meta_fields.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                        key: create_prop_name("columnNumber"),
                        value: Box::new(create_num_expression(position.start.column as f64)),
                    }))));
                }

                // File name and positional info.
                //
                // ```javascript
                // {
                //   fileName: "example.jsx",
                //   lineNumber: 1,
                //   columnNumber: 3
                // }
                // ```
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_object_expression(meta_fields)),
                });

                // Context object.
                //
                // ```javascript
                // this
                // ```
                let this_expression = ThisExpr {
                    span: swc_core::common::DUMMY_SP,
                };
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(Expr::This(this_expression)),
                });
            }

            let callee = if self.development {
                self.import_jsx_dev = true;
                "_jsxDEV"
            } else if is_static_children {
                self.import_jsxs = true;
                "_jsxs"
            } else {
                self.import_jsx = true;
                "_jsx"
            };

            (create_ident_expression(callee), parameters)
        } else {
            // Classic runtime.
            let (props, key) = self.jsx_attributes_to_expressions(attributes, None)?;
            debug_assert!(key.is_none(), "key should not be extracted");
            let mut parameters = vec![
                // Component name.
                //
                // ```javascript
                // Component
                // ```
                ExprOrSpread {
                    spread: None,
                    expr: Box::new(name),
                },
            ];

            // Props or, if with children, null.
            //
            // ```javascript
            // {x: true, y: 'z'}
            // ```
            if let Some(props) = props {
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(props),
                });
            } else if !children.is_empty() {
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_null_expression()),
                });
            }

            // Each child as a parameter.
            children.reverse();
            while let Some(child) = children.pop() {
                parameters.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(child),
                });
            }

            (self.create_element_expression.clone(), parameters)
        };

        let call_expression = CallExpr {
            callee: Callee::Expr(Box::new(callee)),
            args: parameters,
            type_args: None,
            span: *span,
        };

        Ok(Expr::Call(call_expression))
    }

    /// Turn a JSX element into an expression.
    fn jsx_element_to_expression(&mut self, element: JSXElement) -> Result<Expr, Error> {
        let children = self.jsx_children_to_expressions(element.children)?;
        let mut name = jsx_element_name_to_expression(element.opening.name);

        // If the name could be an identifier, but start with a lowercase letter,
        // it’s not a component.
        if let Expr::Ident(ident) = &name {
            let head = ident.as_ref().as_bytes();
            if matches!(head.first(), Some(b'a'..=b'z')) {
                name = create_str_expression(&ident.sym);
            }
        }

        self.jsx_expressions_to_call(&element.span, name, Some(element.opening.attrs), children)
    }

    /// Turn a JSX fragment into an expression.
    fn jsx_fragment_to_expression(&mut self, fragment: JSXFragment) -> Result<Expr, Error> {
        let name = if self.automatic {
            self.import_fragment = true;
            create_ident_expression("_Fragment")
        } else {
            self.fragment_expression.clone()
        };
        let children = self.jsx_children_to_expressions(fragment.children)?;
        self.jsx_expressions_to_call(&fragment.span, name, None, children)
    }
}

impl<'a> VisitMut for State<'a> {
    noop_visit_mut_type!();

    /// Visit expressions, rewriting JSX, and walking deeper.
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        let result = match expr {
            Expr::JSXElement(element) => Some(self.jsx_element_to_expression(*element.take())),
            Expr::JSXFragment(fragment) => Some(self.jsx_fragment_to_expression(fragment.take())),
            _ => None,
        };

        if let Some(result) = result {
            match result {
                Ok(expression) => {
                    *expr = expression;
                    expr.visit_mut_children_with(self);
                }
                Err(err) => {
                    self.error = Some(err);
                }
            }
        } else {
            expr.visit_mut_children_with(self);
        }
    }
}

/// Find directives in comments.
///
/// This looks for block comments (`/* */`) and checks each line that starts
/// with `@jsx`.
/// Then it looks for key/value pairs (each words split by whitespace).
/// Known keys are used for directives.
fn find_directives(
    comments: &Vec<Comment>,
    location: Option<&Location>,
) -> Result<Directives, Error> {
    let mut directives = Directives::default();

    for comment in comments {
        if comment.kind != CommentKind::Block {
            continue;
        }

        let lines = comment.text.lines();

        for line in lines {
            let bytes = line.as_bytes();
            let mut index = 0;
            // Skip initial whitespace.
            while index < bytes.len() && matches!(bytes[index], b' ' | b'\t') {
                index += 1;
            }
            // Skip star.
            if index < bytes.len() && bytes[index] == b'*' {
                index += 1;
                // Skip more whitespace.
                while index < bytes.len() && matches!(bytes[index], b' ' | b'\t') {
                    index += 1;
                }
            }
            // Peek if this looks like a JSX directive.
            if !(index + 4 < bytes.len()
                && bytes[index] == b'@'
                && bytes[index + 1] == b'j'
                && bytes[index + 2] == b's'
                && bytes[index + 3] == b'x')
            {
                // Exit if not.
                continue;
            }

            loop {
                let mut key_range = (index, index);
                while index < bytes.len() && !matches!(bytes[index], b' ' | b'\t') {
                    index += 1;
                }
                key_range.1 = index;
                // Skip whitespace.
                while index < bytes.len() && matches!(bytes[index], b' ' | b'\t') {
                    index += 1;
                }
                let mut value_range = (index, index);
                while index < bytes.len() && !matches!(bytes[index], b' ' | b'\t') {
                    index += 1;
                }
                value_range.1 = index;

                let key = String::from_utf8_lossy(&bytes[key_range.0..key_range.1]);
                let value = String::from_utf8_lossy(&bytes[value_range.0..value_range.1]);

                // Handle the key/value.
                match key.as_ref() {
                    "@jsxRuntime" => match value.as_ref() {
                        "automatic" => directives.runtime = Some(JsxRuntime::Automatic),
                        "classic" => directives.runtime = Some(JsxRuntime::Classic),
                        "" => {}
                        value => {
                            let start = bytepos_to_point(comment.span.lo, location);
                            return Err(prefix_error_with_point(
                                &format!(
                                    "Runtime must be either `automatic` or `classic`, not {}",
                                    value
                                ),
                                start.as_ref(),
                            ));
                        }
                    },
                    "@jsxImportSource" => {
                        match value.as_ref() {
                            "" => {}
                            value => {
                                // SWC sets runtime too, not sure if that’s great.
                                directives.runtime = Some(JsxRuntime::Automatic);
                                directives.import_source = Some(value.into());
                            }
                        }
                    }
                    "@jsxFrag" => match value.as_ref() {
                        "" => {}
                        value => directives.pragma_frag = Some(value.into()),
                    },
                    "@jsx" => match value.as_ref() {
                        "" => {}
                        value => directives.pragma = Some(value.into()),
                    },
                    "" => {
                        // No directive, stop looking for key/value pairs
                        // on this line.
                        break;
                    }
                    _ => {}
                }

                // Skip more whitespace.
                while index < bytes.len() && matches!(bytes[index], b' ' | b'\t') {
                    index += 1;
                }
            }
        }
    }

    Ok(directives)
}

/// Turn JSX text into a string.
fn jsx_text_to_value(value: &str) -> String {
    let mut result = String::with_capacity(value.len());
    // Replace tabs w/ spaces.
    let value = value.replace('\t', " ");
    let bytes = value.as_bytes();
    let mut index = 0;
    let mut start = 0;

    while index < bytes.len() {
        if !matches!(bytes[index], b'\r' | b'\n') {
            index += 1;
            continue;
        }

        // We have an eol, move back past whitespace.
        let mut before = index;
        while before > start && bytes[before - 1] == b' ' {
            before -= 1;
        }

        if start != before {
            if !result.is_empty() {
                result.push(' ');
            }
            result.push_str(str::from_utf8(&bytes[start..before]).unwrap());
        }

        // Move past whitespace.
        index += 1;
        while index < bytes.len() && bytes[index] == b' ' {
            index += 1;
        }
        start = index;
    }

    if start != bytes.len() {
        // Without line endings, if it’s just whitespace, ignore it.
        if result.is_empty() {
            index = 0;

            while index < bytes.len() && bytes[index] == b' ' {
                index += 1;
            }

            if index == bytes.len() {
                return result;
            }
        } else {
            result.push(' ');
        }

        result.push_str(str::from_utf8(&bytes[start..]).unwrap());
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hast_util_to_swc::Program;
    use crate::swc::{flat_comments, serialize};
    use pretty_assertions::assert_eq;
    use swc_core::common::{
        comments::SingleThreadedComments, source_map::Pos, BytePos, FileName, SourceFile,
    };
    use swc_core::ecma::ast::{
        EsVersion, ExprStmt, JSXClosingElement, JSXElementName, JSXOpeningElement, JSXSpreadChild,
        Module, Stmt,
    };
    use swc_core::ecma::parser::{parse_file_as_module, EsConfig, Syntax};

    fn compile(value: &str, options: &Options) -> Result<String, Error> {
        let location = Location::new(value.as_bytes());
        let mut errors = vec![];
        let comments = SingleThreadedComments::default();
        let result = parse_file_as_module(
            &SourceFile::new(
                FileName::Anon,
                false,
                FileName::Anon,
                value.into(),
                BytePos::from_usize(1),
            ),
            Syntax::Es(EsConfig {
                jsx: true,
                ..EsConfig::default()
            }),
            EsVersion::Es2022,
            Some(&comments),
            &mut errors,
        );

        match result {
            Err(error) => Err(error.into()),
            Ok(module) => {
                let mut program = Program {
                    path: Some("example.jsx".into()),
                    module,
                    comments: flat_comments(comments),
                };
                swc_util_build_jsx(&mut program, options, Some(&location))?;
                Ok(serialize(&mut program.module, Some(&program.comments)))
            }
        }
    }

    #[test]
    fn small_default() -> Result<(), Error> {
        assert_eq!(
            compile("let a = <b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\nlet a = _jsx(\"b\", {});\n",
            "should compile JSX away"
        );

        Ok(())
    }

    #[test]
    fn directive_runtime_automatic() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime automatic */\nlet a = <b />",
                &Options::default()
            )?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\nlet a = _jsx(\"b\", {});\n",
            "should support a `@jsxRuntime automatic` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_runtime_classic() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic */\nlet a = <b />",
                &Options::default()
            )?,
            "let a = React.createElement(\"b\");\n",
            "should support a `@jsxRuntime classic` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_runtime_empty() -> Result<(), Error> {
        assert_eq!(
            compile("/* @jsxRuntime */\nlet a = <b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\nlet a = _jsx(\"b\", {});\n",
            "should support an empty `@jsxRuntime` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_runtime_invalid() {
        assert_eq!(
            compile(
                "/* @jsxRuntime unknown */\nlet a = <b />",
                &Options::default()
            )
            .err()
            .unwrap()
            .to_string(),
            "1:1: Runtime must be either `automatic` or `classic`, not unknown",
            "should crash on a non-automatic, non-classic `@jsxRuntime` directive"
        );
    }

    #[test]
    fn directive_import_source() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxImportSource aaa */\nlet a = <b />",
                &Options::default()
            )?,
            "import { jsx as _jsx } from \"aaa/jsx-runtime\";\nlet a = _jsx(\"b\", {});\n",
            "should support a `@jsxImportSource` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_jsx() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic @jsx a */\nlet b = <c />",
                &Options::default()
            )?,
            "let b = a(\"c\");\n",
            "should support a `@jsx` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_jsx_empty() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic @jsx */\nlet a = <b />",
                &Options::default()
            )?,
            "let a = React.createElement(\"b\");\n",
            "should support an empty `@jsx` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_jsx_non_identifier() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic @jsx a.b-c.d! */\n<x />",
                &Options::default()
            )?,
            "a[\"b-c\"][\"d!\"](\"x\");\n",
            "should support an `@jsx` directive set to an invalid identifier"
        );

        Ok(())
    }

    #[test]
    fn directive_jsx_frag() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic @jsxFrag a */\nlet b = <></>",
                &Options::default()
            )?,
            "let b = React.createElement(a);\n",
            "should support a `@jsxFrag` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_jsx_frag_empty() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic @jsxFrag */\nlet a = <></>",
                &Options::default()
            )?,
            "let a = React.createElement(React.Fragment);\n",
            "should support an empty `@jsxFrag` directive"
        );

        Ok(())
    }

    #[test]
    fn directive_non_first_line() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/*\n first line\n @jsxRuntime classic\n */\n<b />",
                &Options::default()
            )?,
            "React.createElement(\"b\");\n",
            "should support a directive on a non-first line"
        );

        Ok(())
    }

    #[test]
    fn directive_asterisked_line() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/*\n * first line\n * @jsxRuntime classic\n */\n<b />",
                &Options::default()
            )?,
            "React.createElement(\"b\");\n",
            "should support a directive on an asterisk’ed line"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_self_closing() -> Result<(), Error> {
        assert_eq!(
            compile("<a />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {});\n",
            "should support a self-closing element"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_self_closing_classic() -> Result<(), Error> {
        assert_eq!(
            compile("/* @jsxRuntime classic */\n<a />", &Options::default())?,
            "React.createElement(\"a\");\n",
            "should support a self-closing element (classic)"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_closed() -> Result<(), Error> {
        assert_eq!(
            compile("<a>b</a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    children: \
             \"b\"\n});\n",
            "should support a closed element"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_member_name() -> Result<(), Error> {
        assert_eq!(
            compile("<a.b.c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(a.b.c, {});\n",
            "should support an element with a member name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_member_name_dashes() -> Result<(), Error> {
        assert_eq!(
            compile("<a.b-c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(a[\"b-c\"], {});\n",
            "should support an element with a member name and dashes"
        );

        Ok(())
    }
    #[test]
    fn jsx_element_member_name_many() -> Result<(), Error> {
        assert_eq!(
            compile("<a.b.c.d />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(a.b.c.d, {});\n",
            "should support an element with a member name of lots of names"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_namespace_name() -> Result<(), Error> {
        assert_eq!(
            compile("<a:b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a:b\", {});\n",
            "should support an element with a namespace name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_name_dashes() -> Result<(), Error> {
        assert_eq!(
            compile("<a-b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a-b\", {});\n",
            "should support an element with a dash in the name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_name_capital() -> Result<(), Error> {
        assert_eq!(
            compile("<Abc />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(Abc, {});\n",
            "should support an element with a non-lowercase first character in the name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_boolean() -> Result<(), Error> {
        assert_eq!(
            compile("<a b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: true\n});\n",
            "should support an element with a boolean attribute"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_boolean_classic() -> Result<(), Error> {
        assert_eq!(
            compile("/* @jsxRuntime classic */\n<a b />", &Options::default())?,
            "React.createElement(\"a\", {\n    b: true\n});\n",
            "should support an element with a boolean attribute (classic"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_name_namespace() -> Result<(), Error> {
        assert_eq!(
            compile("<a b:c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    \"b:c\": \
             true\n});\n",
            "should support an element with colons in an attribute name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_name_non_identifier() -> Result<(), Error> {
        assert_eq!(
            compile("<a b-c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    \"b-c\": \
             true\n});\n",
            "should support an element with non-identifier characters in an attribute name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value() -> Result<(), Error> {
        assert_eq!(
            compile("<a b='c' />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: \
             \"c\"\n});\n",
            "should support an element with an attribute with a value"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value_expression() -> Result<(), Error> {
        assert_eq!(
            compile("<a b={c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: c\n});\n",
            "should support an element with an attribute with a value expression"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value_fragment() -> Result<(), Error> {
        assert_eq!(
            compile("<a b=<>c</> />", &Options::default())?,
            "import { Fragment as _Fragment, jsx as _jsx } from \
             \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: _jsx(_Fragment, {\n        children: \
             \"c\"\n    })\n});\n",
            "should support an element with an attribute with a fragment as value"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value_element() -> Result<(), Error> {
        assert_eq!(
            compile("<a b=<c /> />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: \
             _jsx(\"c\", {})\n});\n",
            "should support an element with an attribute with an element as value"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_spread_attribute() -> Result<(), Error> {
        assert_eq!(
            compile("<a {...b} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", b);\n",
            "should support an element with a spread attribute"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_spread_attribute_then_prop() -> Result<(), Error> {
        assert_eq!(
            compile("<a {...b} c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", Object.assign({}, b, \
             {\n    c: true\n}));\n",
            "should support an element with a spread attribute and then a prop"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_prop_then_spread_attribute() -> Result<(), Error> {
        assert_eq!(
            compile("<a b {...c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", Object.assign({\n    \
             b: true\n}, c));\n",
            "should support an element with a prop and then a spread attribute"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_two_spread_attributes() -> Result<(), Error> {
        assert_eq!(
            compile("<a {...b} {...c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", Object.assign({}, b, \
             c));\n",
            "should support an element two spread attributes"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_complex_spread_attribute() -> Result<(), Error> {
        assert_eq!(
            compile("<a {...{b:1,...c,d:2}} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    b: 1,
    ...c,
    d: 2
});
",
            "should support more complex spreads"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_expression() -> Result<(), Error> {
        assert_eq!(
            compile("<a>{1}</a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: 1
});
",
            "should support a child expression"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_expression_classic() -> Result<(), Error> {
        assert_eq!(
            compile("/* @jsxRuntime classic */\n<a>{1}</a>", &Options::default())?,
            "React.createElement(\"a\", null, 1);\n",
            "should support a child expression (classic)"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_expression_empty() -> Result<(), Error> {
        assert_eq!(
            compile("<a>{}</a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {});
",
            "should support an empty child expression"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_expression_empty_classic() -> Result<(), Error> {
        assert_eq!(
            compile("/* @jsxRuntime classic */\n<a>{}</a>", &Options::default())?,
            "React.createElement(\"a\");\n",
            "should support an empty child expression (classic)"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_fragment() -> Result<(), Error> {
        assert_eq!(
            compile("<a><>b</></a>", &Options::default())?,
            "import { Fragment as _Fragment, jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: _jsx(_Fragment, {
        children: \"b\"
    })
});
",
            "should support a fragment as a child"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_spread() {
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
                        children: vec![JSXElementChild::JSXSpreadChild(JSXSpreadChild {
                            expr: Box::new(create_ident_expression("a")),
                            span: swc_core::common::DUMMY_SP,
                        })],
                    }))),
                }))],
            },
        };

        assert_eq!(
            swc_util_build_jsx(&mut program, &Options::default(), None)
                .err()
                .unwrap()
                .to_string(),
            "0:0: Unexpected spread child, which is not supported in Babel, SWC, or React",
            "should not support a spread child"
        );
    }

    #[test]
    fn jsx_element_child_text_padded_start() -> Result<(), Error> {
        assert_eq!(
            compile("<a>  b</a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: \"  b\"
});
",
            "should support initial spaces in content"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_text_padded_end() -> Result<(), Error> {
        assert_eq!(
            compile("<a>b  </a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: \"b  \"
});
",
            "should support final spaces in content"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_text_padded() -> Result<(), Error> {
        assert_eq!(
            compile("<a>  b  </a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: \"  b  \"
});
",
            "should support initial and final spaces in content"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_text_line_endings_padded() -> Result<(), Error> {
        assert_eq!(
            compile("<a> b \r c \n d \n </a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: \" b c d\"
});
",
            "should support spaces around line endings in content"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_text_blank_lines() -> Result<(), Error> {
        assert_eq!(
            compile("<a> b \r \n c \n\n d \n </a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    children: \" b c d\"
});
",
            "should support blank lines in content"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_child_whitespace_only() -> Result<(), Error> {
        assert_eq!(
            compile("<a> \t\n </a>", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {});
",
            "should support whitespace-only in content"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_key_automatic() -> Result<(), Error> {
        assert_eq!(
            compile("<a b key='c' d />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", {
    b: true,
    d: true
}, \"c\");
",
            "should support a key in the automatic runtime"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_key_classic() -> Result<(), Error> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic */\n<a b key='c' d />",
                &Options::default()
            )?,
            "React.createElement(\"a\", {
    b: true,
    key: \"c\",
    d: true
});
",
            "should support a key in the classic runtime"
        );

        Ok(())
    }
    #[test]
    fn jsx_element_key_after_spread_automatic() {
        assert_eq!(
            compile("<a {...b} key='c' />", &Options::default())
                .err()
                .unwrap()
                .to_string(),
            "1:11: Expected `key` to come before any spread expressions",
            "should crash on a key after a spread in the automatic runtime"
        );
    }

    #[test]
    fn jsx_element_key_before_spread_automatic() -> Result<(), Error> {
        assert_eq!(
            compile("<a key='b' {...c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";
_jsx(\"a\", c, \"b\");
",
            "should support a key before a spread in the automatic runtime"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_development() -> Result<(), Error> {
        assert_eq!(
            compile("<><a /></>", &Options { development: true })?,
            "import { Fragment as _Fragment, jsxDEV as _jsxDEV } from \"react/jsx-dev-runtime\";
_jsxDEV(_Fragment, {
    children: _jsxDEV(\"a\", {}, undefined, false, {
        fileName: \"example.jsx\",
        lineNumber: 1,
        columnNumber: 3
    }, this)
}, undefined, false, {
    fileName: \"example.jsx\",
    lineNumber: 1,
    columnNumber: 1
}, this);
",
            "should support the automatic development runtime if `development` is on"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_development_no_filepath() -> Result<(), Error> {
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
                            self_closing: true,
                            type_args: None,
                            span: swc_core::common::DUMMY_SP,
                        },
                        closing: None,
                        children: vec![],
                    }))),
                }))],
            },
        };

        swc_util_build_jsx(&mut program, &Options { development: true }, None)?;

        assert_eq!(
            serialize(&mut program.module, Some(&program.comments)),
            "import { jsxDEV as _jsxDEV } from \"react/jsx-dev-runtime\";
_jsxDEV(\"a\", {}, undefined, false, {
    fileName: \"<source.js>\"
}, this);
",
            "should support the automatic development runtime without a file path"
        );

        Ok(())
    }

    #[test]
    fn jsx_text() {
        assert_eq!(jsx_text_to_value("a"), "a", "should support jsx text");
        assert_eq!(
            jsx_text_to_value("  a\t"),
            "  a ",
            "should support jsx text w/ initial, final whitespace"
        );
        assert_eq!(
            jsx_text_to_value("  \t"),
            "",
            "should support jsx text that’s just whitespace"
        );
        assert_eq!(
            jsx_text_to_value("a\r\r\n\nb"),
            "a b",
            "should support jsx text with line endings"
        );
        assert_eq!(
            jsx_text_to_value("  a  \n  b  \n  c  "),
            "  a b c  ",
            "should support jsx text with line endings with white space"
        );
        assert_eq!(
            jsx_text_to_value("  \n  a  \n    "),
            "a",
            "should support jsx text with blank initial and final lines"
        );
        assert_eq!(
            jsx_text_to_value("  a  \n  \n  \t  \n  b  "),
            "  a b  ",
            "should support jsx text with blank lines in between"
        );
        assert_eq!(
            jsx_text_to_value("    \n  \n  \t  \n    "),
            "",
            "should support jsx text with only spaces, tabs, and line endings"
        );
    }
}
