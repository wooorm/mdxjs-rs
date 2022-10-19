//! Rewrite JSX tags to accept them from props and an optional provider.
//!
//! Port of <https://github.com/mdx-js/mdx/blob/main/packages/mdx/lib/plugin/recma-jsx-rewrite.js>,
//! by the same author.

extern crate swc_common;
extern crate swc_ecma_ast;
use crate::hast_util_to_swc::{Program, MAGIC_EXPLICIT_MARKER};
use crate::swc_utils::{
    create_binary_expression, create_bool_expression, create_ident, create_ident_expression,
    create_member, create_member_expression_from_str, create_member_prop_from_str,
    create_prop_name, create_str, create_str_expression, is_identifier_name, is_literal_name,
    position_to_string, span_to_position,
};
use markdown::{unist::Position, Location};
use swc_common::util::take::Take;
use swc_ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

/// Configuration.
#[derive(Debug, Default, Clone)]
pub struct Options {
    /// Place to import a provider from.
    ///
    /// See [MDX provider](https://mdxjs.com/docs/using-mdx/#mdx-provider)
    /// on the MDX website for more info.
    pub provider_import_source: Option<String>,
    /// Whether to add extra information to error messages in generated code.
    pub development: bool,
}

/// Rewrite JSX in an MDX file so that components can be passed in and provided.
pub fn mdx_plugin_recma_jsx_rewrite(
    program: &mut Program,
    options: &Options,
    location: Option<&Location>,
) {
    let mut state = State {
        scopes: vec![],
        location,
        provider: options.provider_import_source.is_some(),
        path: program.path.clone(),
        development: options.development,
        create_provider_import: false,
        create_error_helper: false,
    };
    state.enter(Some(Info::default()));
    program.module.visit_mut_with(&mut state);

    // If a provider is used (and can be used), import it.
    if let Some(source) = &options.provider_import_source {
        if state.create_provider_import {
            program
                .module
                .body
                .insert(0, create_import_provider(source));
        }
    }

    // If potentially missing components are used, add the helper used for
    // errors.
    if state.create_error_helper {
        program
            .module
            .body
            .push(create_error_helper(state.development, state.path));
    }
}

/// Collection of different SWC functions.
#[derive(Debug)]
enum Func<'a> {
    /// Function declaration.
    Decl(&'a mut swc_ecma_ast::FnDecl),
    /// Function expression.
    Expr(&'a mut swc_ecma_ast::FnExpr),
    /// Arrow function.
    Arrow(&'a mut swc_ecma_ast::ArrowExpr),
}

/// Info for a function scope.
#[derive(Debug, Default, Clone)]
struct Info {
    /// Function name.
    name: Option<String>,
    /// Used objects (`a` in `<a.b />`).
    objects: Vec<String>,
    /// Used components (`<A />`).
    components: Vec<String>,
    /// Used literals (`<a />`).
    tags: Vec<String>,
    /// List of JSX identifiers of literal tags that are not valid JS
    /// identifiers in the shape of `Vec<(invalid, valid)>`.
    ///
    /// Example:
    ///
    /// ```rust ignore
    /// vec![("a-b".into(), "_component0".into())]
    /// ```
    aliases: Vec<(String, String)>,
    /// Non-literal references in the shape of `Vec<(name, is_component)>`.
    ///
    /// Example:
    ///
    /// ```rust ignore
    /// vec![("a".into(), false), ("a.b".into(), true)]
    /// ```
    references: Vec<(String, bool, Option<Position>)>,
}

/// Scope (block or function/global).
#[derive(Debug, Clone)]
struct Scope {
    /// If this is a function (or global) scope, we track info.
    info: Option<Info>,
    /// Things that are defined in this scope.
    defined: Vec<String>,
}

/// Context.
#[derive(Debug, Default, Clone)]
struct State<'a> {
    /// Location info.
    location: Option<&'a Location>,
    /// Path to file.
    path: Option<String>,
    /// List of current scopes.
    scopes: Vec<Scope>,
    /// Whether the user is in development mode.
    development: bool,
    /// Whether the user uses a provider.
    provider: bool,
    /// Whether a provider is referenced.
    create_provider_import: bool,
    /// Whether a missing component helper is referenced.
    ///
    /// When things are referenced that might not be defined, we reference a
    /// helper function to throw when they are missing.
    create_error_helper: bool,
}

impl<'a> State<'a> {
    /// Open a new scope.
    fn enter(&mut self, info: Option<Info>) {
        self.scopes.push(Scope {
            info,
            defined: vec![],
        });
    }

    /// Close the current scope.
    fn exit(&mut self) -> Scope {
        self.scopes.pop().expect("expected scope")
    }

    /// Close a function.
    fn exit_func(&mut self, func: Func) {
        let mut scope = self.exit();
        let mut defaults = vec![];
        let mut info = scope.info.take().unwrap();
        let mut index = 0;

        // Create defaults for tags.
        //
        // ```jsx
        // {h1: 'h1'}
        // ```
        while index < info.tags.len() {
            let name = &info.tags[index];

            defaults.push(swc_ecma_ast::PropOrSpread::Prop(Box::new(
                swc_ecma_ast::Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                    key: create_prop_name(name),
                    value: Box::new(create_str_expression(name)),
                }),
            )));

            index += 1;
        }

        let mut actual = info.components.split_off(0);
        let mut index = 0;

        // In some cases, a component is used directly (`<X>`) but it’s also
        // used as an object (`<X.Y>`).
        while index < info.objects.len() {
            if !actual.contains(&info.objects[index]) {
                actual.push(info.objects[index].clone());
            }
            index += 1;
        }

        let mut statements = vec![];

        if !defaults.is_empty() || !actual.is_empty() || !info.aliases.is_empty() {
            let mut parameters = vec![];

            // Use a provider, if configured.
            //
            // ```jsx
            // _provideComponents()
            // ```
            if self.provider {
                self.create_provider_import = true;
                parameters.push(swc_ecma_ast::Expr::Call(swc_ecma_ast::CallExpr {
                    callee: swc_ecma_ast::Callee::Expr(Box::new(create_ident_expression(
                        "_provideComponents",
                    ))),
                    args: vec![],
                    type_args: None,
                    span: swc_common::DUMMY_SP,
                }));
            }

            // Accept `components` as a prop if this is the `MDXContent` or
            // `_createMdxContent` function.
            //
            // ```jsx
            // props.components
            // ```
            if is_props_receiving_fn(&info.name) {
                parameters.push(swc_ecma_ast::Expr::Member(swc_ecma_ast::MemberExpr {
                    obj: Box::new(create_ident_expression("props")),
                    prop: swc_ecma_ast::MemberProp::Ident(create_ident("components")),
                    span: swc_common::DUMMY_SP,
                }));
            }

            // Inject an object at the start, when:
            // - there are defaults,
            // - there are two sources
            //
            // ```jsx
            // (_provideComponents(), props.components)
            // ()
            // ```
            //
            // To:
            //
            // ```jsx
            // ({}, _provideComponents(), props.components)
            // ({h1: 'h1'})
            // ```
            if !defaults.is_empty() || parameters.len() > 1 {
                parameters.insert(
                    0,
                    swc_ecma_ast::Expr::Object(swc_ecma_ast::ObjectLit {
                        props: defaults,
                        span: swc_common::DUMMY_SP,
                    }),
                );
            }

            // Merge things and prevent errors.
            //
            // ```jsx
            // {}, _provideComponents(), props.components
            // props.components
            // _provideComponents()
            // ```
            //
            // To:
            //
            // ```jsx
            // Object.assign({}, _provideComponents(), props.components)
            // props.components || {}
            // _provideComponents()
            // ```
            let mut components_init = if parameters.len() > 1 {
                let mut args = vec![];
                parameters.reverse();
                while let Some(param) = parameters.pop() {
                    args.push(swc_ecma_ast::ExprOrSpread {
                        spread: None,
                        expr: Box::new(param),
                    });
                }
                swc_ecma_ast::Expr::Call(swc_ecma_ast::CallExpr {
                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                        create_member_expression_from_str("Object.assign"),
                    )),
                    args,
                    type_args: None,
                    span: swc_common::DUMMY_SP,
                })
            } else {
                // Always one.
                let param = parameters.pop().unwrap();

                if let swc_ecma_ast::Expr::Member(_) = param {
                    create_binary_expression(
                        vec![
                            param,
                            swc_ecma_ast::Expr::Object(swc_ecma_ast::ObjectLit {
                                props: vec![],
                                span: swc_common::DUMMY_SP,
                            }),
                        ],
                        swc_ecma_ast::BinaryOp::LogicalOr,
                    )
                } else {
                    param
                }
            };

            // Add components to scope.
            //
            // For `['MyComponent', 'MDXLayout']` this generates:
            //
            // ```js
            // const {MyComponent, wrapper: MDXLayout} = _components
            // ```
            //
            // Note that MDXLayout is special as it’s taken from
            // `_components.wrapper`.
            let components_pattern = if actual.is_empty() {
                None
            } else {
                let mut props = vec![];
                actual.reverse();
                while let Some(key) = actual.pop() {
                    // `wrapper: MDXLayout`
                    if key == "MDXLayout" {
                        props.push(swc_ecma_ast::ObjectPatProp::KeyValue(
                            swc_ecma_ast::KeyValuePatProp {
                                key: create_prop_name("wrapper"),
                                value: Box::new(swc_ecma_ast::Pat::Ident(
                                    swc_ecma_ast::BindingIdent {
                                        id: create_ident(&key),
                                        type_ann: None,
                                    },
                                )),
                            },
                        ));
                    }
                    // `MyComponent`
                    else {
                        props.push(swc_ecma_ast::ObjectPatProp::Assign(
                            swc_ecma_ast::AssignPatProp {
                                key: create_ident(&key),
                                value: None,
                                span: swc_common::DUMMY_SP,
                            },
                        ));
                    }
                }

                Some(swc_ecma_ast::ObjectPat {
                    props,
                    optional: false,
                    span: swc_common::DUMMY_SP,
                    type_ann: None,
                })
            };

            let mut declarators = vec![];

            // If there are tags, they take them from `_components`, so we need
            // to make it defined.
            if !info.tags.is_empty() {
                declarators.push(swc_ecma_ast::VarDeclarator {
                    span: swc_common::DUMMY_SP,
                    name: swc_ecma_ast::Pat::Ident(swc_ecma_ast::BindingIdent {
                        id: create_ident("_components"),
                        type_ann: None,
                    }),
                    init: Some(Box::new(components_init)),
                    definite: false,
                });
                components_init = create_ident_expression("_components");
            }

            // For JSX IDs that can’t be represented as JavaScript IDs (as in,
            // those with dashes, such as `custom-element`), we generated a
            // separate variable that is a valid JS ID (such as `_component0`),
            // and here we take it from components:
            // ```js
            // const _component0 = _components['custom-element']
            // ```
            if !info.aliases.is_empty() {
                info.aliases.reverse();

                while let Some((id, name)) = info.aliases.pop() {
                    declarators.push(swc_ecma_ast::VarDeclarator {
                        span: swc_common::DUMMY_SP,
                        name: swc_ecma_ast::Pat::Ident(swc_ecma_ast::BindingIdent {
                            id: create_ident(&name),
                            type_ann: None,
                        }),
                        init: Some(Box::new(swc_ecma_ast::Expr::Member(create_member(
                            create_ident_expression("_components"),
                            create_member_prop_from_str(&id),
                        )))),
                        definite: false,
                    });
                }
            }

            if let Some(pat) = components_pattern {
                declarators.push(swc_ecma_ast::VarDeclarator {
                    name: swc_ecma_ast::Pat::Object(pat),
                    init: Some(Box::new(components_init)),
                    span: swc_common::DUMMY_SP,
                    definite: false,
                });
            }

            // Add the variable declaration.
            statements.push(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Var(Box::new(
                swc_ecma_ast::VarDecl {
                    kind: swc_ecma_ast::VarDeclKind::Const,
                    decls: declarators,
                    span: swc_common::DUMMY_SP,
                    declare: false,
                },
            ))));
        }

        // Add checks at runtime to verify that object/components are passed.
        //
        // ```js
        // if (!a) _missingMdxReference("a", false);
        // if (!a.b) _missingMdxReference("a.b", true);
        // ```
        for (id, component, position) in info.references {
            self.create_error_helper = true;

            let mut args = vec![
                swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_str_expression(&id)),
                },
                swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(create_bool_expression(component)),
                },
            ];

            // Add the source location if it exists and if `development` is on.
            if let Some(position) = position.as_ref() {
                if self.development {
                    args.push(swc_ecma_ast::ExprOrSpread {
                        spread: None,
                        expr: Box::new(create_str_expression(&position_to_string(position))),
                    });
                }
            }

            statements.push(swc_ecma_ast::Stmt::If(swc_ecma_ast::IfStmt {
                test: Box::new(swc_ecma_ast::Expr::Unary(swc_ecma_ast::UnaryExpr {
                    op: swc_ecma_ast::UnaryOp::Bang,
                    arg: Box::new(create_member_expression_from_str(&id)),
                    span: swc_common::DUMMY_SP,
                })),
                cons: Box::new(swc_ecma_ast::Stmt::Expr(swc_ecma_ast::ExprStmt {
                    span: swc_common::DUMMY_SP,
                    expr: Box::new(swc_ecma_ast::Expr::Call(swc_ecma_ast::CallExpr {
                        callee: swc_ecma_ast::Callee::Expr(Box::new(create_ident_expression(
                            "_missingMdxReference",
                        ))),
                        args,
                        type_args: None,
                        span: swc_common::DUMMY_SP,
                    })),
                })),
                alt: None,
                span: swc_common::DUMMY_SP,
            }));
        }

        // Add statements to functions.
        if !statements.is_empty() {
            let mut body: &mut swc_ecma_ast::BlockStmt = match func {
                Func::Expr(expr) => {
                    if expr.function.body.is_none() {
                        expr.function.body = Some(swc_ecma_ast::BlockStmt {
                            stmts: vec![],
                            span: swc_common::DUMMY_SP,
                        });
                    }
                    expr.function.body.as_mut().unwrap()
                }
                Func::Decl(decl) => {
                    if decl.function.body.is_none() {
                        decl.function.body = Some(swc_ecma_ast::BlockStmt {
                            stmts: vec![],
                            span: swc_common::DUMMY_SP,
                        });
                    }
                    decl.function.body.as_mut().unwrap()
                }
                Func::Arrow(arr) => {
                    if let swc_ecma_ast::BlockStmtOrExpr::Expr(expr) = &mut arr.body {
                        arr.body =
                            swc_ecma_ast::BlockStmtOrExpr::BlockStmt(swc_ecma_ast::BlockStmt {
                                stmts: vec![swc_ecma_ast::Stmt::Return(swc_ecma_ast::ReturnStmt {
                                    arg: Some(expr.take()),
                                    span: swc_common::DUMMY_SP,
                                })],
                                span: swc_common::DUMMY_SP,
                            });
                    }
                    arr.body.as_mut_block_stmt().unwrap()
                }
            };

            statements.append(&mut body.stmts.split_off(0));
            body.stmts = statements;
        }
    }

    /// Get the current function scope.
    fn current_fn_scope_mut(&mut self) -> &mut Scope {
        let mut index = self.scopes.len();

        while index > 0 {
            index -= 1;
            if self.scopes[index].info.is_some() {
                return &mut self.scopes[index];
            }
        }

        unreachable!("expected scope")
    }

    /// Get the current scope.
    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("expected scope")
    }

    /// Get the top-level scope’s info.
    fn current_top_level_info(&self) -> Option<&Info> {
        if let Some(scope) = self.scopes.get(1) {
            scope.info.as_ref()
        } else {
            None
        }
    }

    /// Get the top-level scope’s info, mutably.
    fn current_top_level_info_mut(&mut self) -> Option<&mut Info> {
        if let Some(scope) = self.scopes.get_mut(1) {
            scope.info.as_mut()
        } else {
            None
        }
    }

    /// Check if `id` is in scope.
    fn in_scope(&self, id: &String) -> bool {
        let mut index = self.scopes.len();

        while index > 0 {
            index -= 1;
            if self.scopes[index].defined.contains(id) {
                return true;
            }
        }

        false
    }

    /// Add an identifier to a scope.
    fn add_id(&mut self, id: String, block: bool) {
        let scope = if block {
            self.current_scope_mut()
        } else {
            self.current_fn_scope_mut()
        };
        scope.defined.push(id);
    }

    // Add a pattern to a scope.
    fn add_pat(&mut self, pat: &swc_ecma_ast::Pat, block: bool) {
        match pat {
            // `x`
            swc_ecma_ast::Pat::Ident(d) => self.add_id(d.id.sym.to_string(), block),
            // `...x`
            swc_ecma_ast::Pat::Array(d) => {
                let mut index = 0;
                while index < d.elems.len() {
                    if let Some(d) = &d.elems[index] {
                        self.add_pat(d, block);
                    }
                    index += 1;
                }
            }
            // `...x`
            swc_ecma_ast::Pat::Rest(d) => self.add_pat(&d.arg, block),
            // `{x=y}`
            swc_ecma_ast::Pat::Assign(d) => self.add_pat(&d.left, block),
            swc_ecma_ast::Pat::Object(d) => {
                let mut index = 0;
                while index < d.props.len() {
                    match &d.props[index] {
                        // `{...x}`
                        swc_ecma_ast::ObjectPatProp::Rest(d) => {
                            self.add_pat(&d.arg, block);
                        }
                        // `{key: value}`
                        swc_ecma_ast::ObjectPatProp::KeyValue(d) => {
                            self.add_pat(&d.value, block);
                        }
                        // `{key}` or `{key = value}`
                        swc_ecma_ast::ObjectPatProp::Assign(d) => {
                            self.add_id(d.key.to_string(), block);
                        }
                    }
                    index += 1;
                }
            }
            // Ignore `Invalid` / `Expr`.
            _ => {}
        }
    }
}

impl<'a> VisitMut for State<'a> {
    noop_visit_mut_type!();

    /// Rewrite JSX identifiers.
    fn visit_mut_jsx_element(&mut self, node: &mut swc_ecma_ast::JSXElement) {
        // If there is a top-level, non-global, scope which is a function.
        if let Some(info) = self.current_top_level_info() {
            // Rewrite only if we can rewrite.
            if is_props_receiving_fn(&info.name) || self.provider {
                let position = span_to_position(&node.span, self.location);
                match &node.opening.name {
                    // `<x.y>`, `<Foo.Bar>`, `<x.y.z>`.
                    swc_ecma_ast::JSXElementName::JSXMemberExpr(d) => {
                        let mut ids = vec![];
                        let mut mem = d;
                        loop {
                            ids.push(mem.prop.sym.to_string());
                            match &mem.obj {
                                swc_ecma_ast::JSXObject::Ident(d) => {
                                    ids.push(d.sym.to_string());
                                    break;
                                }
                                swc_ecma_ast::JSXObject::JSXMemberExpr(d) => {
                                    mem = d;
                                }
                            }
                        }
                        ids.reverse();
                        let primary_id = ids.first().unwrap().clone();
                        let in_scope = self.in_scope(&primary_id);

                        if !in_scope {
                            let info_mut = self.current_top_level_info_mut().unwrap();

                            let mut index = 1;
                            while index <= ids.len() {
                                let full_id = ids[0..index].join(".");
                                let component = index == ids.len();
                                if let Some(reference) =
                                    info_mut.references.iter_mut().find(|d| d.0 == full_id)
                                {
                                    if component {
                                        reference.1 = true;
                                    }
                                } else {
                                    info_mut.references.push((
                                        full_id,
                                        component,
                                        position.clone(),
                                    ));
                                }
                                index += 1;
                            }

                            if !info_mut.objects.contains(&primary_id) {
                                info_mut.objects.push(primary_id);
                            }
                        }
                    }
                    // `<foo>`, `<Foo>`, `<$>`, `<_bar>`, `<a_b>`.
                    swc_ecma_ast::JSXElementName::Ident(d) => {
                        let explicit_jsx = node.span.ctxt.as_u32() == MAGIC_EXPLICIT_MARKER;

                        // If the name is a valid ES identifier, and it doesn’t
                        // start with a lowercase letter, it’s a component.
                        // For example, `$foo`, `_bar`, `Baz` are all component
                        // names.
                        // But `foo` and `b-ar` are tag names.
                        let id = d.sym.to_string();

                        if is_literal_name(&id) {
                            let mut invalid = None;

                            let name = if is_identifier_name(&id) {
                                if explicit_jsx {
                                    swc_ecma_ast::JSXElementName::Ident(create_ident(&id))
                                } else {
                                    swc_ecma_ast::JSXElementName::JSXMemberExpr(
                                        swc_ecma_ast::JSXMemberExpr {
                                            obj: swc_ecma_ast::JSXObject::Ident(create_ident(
                                                "_components",
                                            )),
                                            prop: create_ident(&id),
                                        },
                                    )
                                }
                            } else {
                                let name = if let Some(invalid_ref) =
                                    info.aliases.iter().find(|d| d.0 == id)
                                {
                                    invalid_ref.1.clone()
                                } else {
                                    let name = format!("_component{}", info.aliases.len());
                                    invalid = Some((id.clone(), name.clone()));
                                    name
                                };

                                swc_ecma_ast::JSXElementName::Ident(create_ident(&name))
                            };

                            let info_mut = self.current_top_level_info_mut().unwrap();

                            if !info_mut.tags.contains(&id) {
                                info_mut.tags.push(id);
                            }

                            if let Some(invalid) = invalid {
                                info_mut.aliases.push(invalid);
                            }

                            if let Some(closing) = node.closing.as_mut() {
                                closing.name = name.clone();
                            }

                            node.opening.name = name;
                        } else {
                            let mut is_layout = false;

                            // The MDXLayout is wrapped in a
                            if let Some(name) = &info.name {
                                if name == "MDXContent" && id == "MDXLayout" {
                                    is_layout = true;
                                }
                            }

                            if !self.in_scope(&id) {
                                let info_mut = self.current_top_level_info_mut().unwrap();

                                if !is_layout {
                                    if let Some(reference) =
                                        info_mut.references.iter_mut().find(|d| d.0 == id)
                                    {
                                        reference.1 = true;
                                    } else {
                                        info_mut.references.push((id.clone(), true, position));
                                    }
                                }

                                if !info_mut.components.contains(&id) {
                                    info_mut.components.push(id);
                                }
                            }
                        }
                    }
                    // `<xml:thing>`.
                    swc_ecma_ast::JSXElementName::JSXNamespacedName(_) => {
                        // Ignore.
                    }
                }
            }
        }

        node.visit_mut_children_with(self);
    }

    /// Add specifiers of import declarations.
    fn visit_mut_import_decl(&mut self, node: &mut swc_ecma_ast::ImportDecl) {
        let mut index = 0;
        while index < node.specifiers.len() {
            let ident = match &node.specifiers[index] {
                swc_ecma_ast::ImportSpecifier::Default(x) => &x.local.sym,
                swc_ecma_ast::ImportSpecifier::Namespace(x) => &x.local.sym,
                swc_ecma_ast::ImportSpecifier::Named(x) => &x.local.sym,
            };
            self.add_id(ident.to_string(), false);
            index += 1;
        }

        node.visit_mut_children_with(self);
    }

    /// Add patterns of variable declarations.
    fn visit_mut_var_decl(&mut self, node: &mut swc_ecma_ast::VarDecl) {
        let block = node.kind != swc_ecma_ast::VarDeclKind::Var;
        let mut index = 0;
        while index < node.decls.len() {
            self.add_pat(&node.decls[index].name, block);
            index += 1;
        }
        node.visit_mut_children_with(self);
    }

    /// Add identifier of class declaration.
    fn visit_mut_class_decl(&mut self, node: &mut swc_ecma_ast::ClassDecl) {
        self.add_id(node.ident.sym.to_string(), false);
        node.visit_mut_children_with(self);
    }

    /// On function declarations, add name, create scope, add parameters.
    fn visit_mut_fn_decl(&mut self, node: &mut swc_ecma_ast::FnDecl) {
        let id = node.ident.sym.to_string();
        self.add_id(id.clone(), false);
        self.enter(Some(Info {
            name: Some(id),
            ..Default::default()
        }));
        let mut index = 0;
        while index < node.function.params.len() {
            self.add_pat(&node.function.params[index].pat, false);
            index += 1;
        }
        node.visit_mut_children_with(self);
        // Rewrite.
        self.exit_func(Func::Decl(node));
    }

    /// On function expressions, add name, create scope, add parameters.
    fn visit_mut_fn_expr(&mut self, node: &mut swc_ecma_ast::FnExpr) {
        // Note: `periscopic` adds the ID to the newly generated scope, for
        // fn expressions.
        // That seems wrong?
        let name = if let Some(ident) = &node.ident {
            let id = ident.sym.to_string();
            self.add_id(id.clone(), false);
            Some(id)
        } else {
            None
        };

        self.enter(Some(Info {
            name,
            ..Default::default()
        }));
        let mut index = 0;
        while index < node.function.params.len() {
            self.add_pat(&node.function.params[index].pat, false);
            index += 1;
        }
        node.visit_mut_children_with(self);
        self.exit_func(Func::Expr(node));
    }

    /// On arrow functions, create scope, add parameters.
    fn visit_mut_arrow_expr(&mut self, node: &mut swc_ecma_ast::ArrowExpr) {
        self.enter(Some(Info::default()));
        let mut index = 0;
        while index < node.params.len() {
            self.add_pat(&node.params[index], false);
            index += 1;
        }
        node.visit_mut_children_with(self);
        self.exit_func(Func::Arrow(node));
    }

    // Blocks.
    // Not sure why `periscopic` only does `For`/`ForIn`/`ForOf`/`Block`.
    // I added `While`/`DoWhile` here just to be sure.
    // But there are more.
    /// On for statements, create scope.
    fn visit_mut_for_stmt(&mut self, node: &mut swc_ecma_ast::ForStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On for/in statements, create scope.
    fn visit_mut_for_in_stmt(&mut self, node: &mut swc_ecma_ast::ForInStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On for/of statements, create scope.
    fn visit_mut_for_of_stmt(&mut self, node: &mut swc_ecma_ast::ForOfStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On while statements, create scope.
    fn visit_mut_while_stmt(&mut self, node: &mut swc_ecma_ast::WhileStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On do/while statements, create scope.
    fn visit_mut_do_while_stmt(&mut self, node: &mut swc_ecma_ast::DoWhileStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On block statements, create scope.
    fn visit_mut_block_stmt(&mut self, node: &mut swc_ecma_ast::BlockStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }

    /// On catch clauses, create scope, add param.
    fn visit_mut_catch_clause(&mut self, node: &mut swc_ecma_ast::CatchClause) {
        self.enter(None);
        if let Some(pat) = &node.param {
            self.add_pat(pat, true);
        }
        node.visit_mut_children_with(self);
        self.exit();
    }
}

/// Generate an import provider.
///
/// ```js
/// import { useMDXComponents as _provideComponents } from "x"
/// ```
fn create_import_provider(source: &str) -> swc_ecma_ast::ModuleItem {
    swc_ecma_ast::ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::Import(
        swc_ecma_ast::ImportDecl {
            specifiers: vec![swc_ecma_ast::ImportSpecifier::Named(
                swc_ecma_ast::ImportNamedSpecifier {
                    local: create_ident("_provideComponents"),
                    imported: Some(swc_ecma_ast::ModuleExportName::Ident(create_ident(
                        "useMDXComponents",
                    ))),
                    span: swc_common::DUMMY_SP,
                    is_type_only: false,
                },
            )],
            src: Box::new(create_str(source)),
            type_only: false,
            asserts: None,
            span: swc_common::DUMMY_SP,
        },
    ))
}

/// Generate an error helper.
///
/// ```js
/// function _missingMdxReference(id, component) {
///   throw new Error("Expected " + (component ? "component" : "object") + " `" + id + "` to be defined: you likely forgot to import, pass, or provide it.");
/// }
/// ```
fn create_error_helper(development: bool, path: Option<String>) -> swc_ecma_ast::ModuleItem {
    let mut parameters = vec![
        swc_ecma_ast::Param {
            pat: swc_ecma_ast::Pat::Ident(swc_ecma_ast::BindingIdent {
                id: create_ident("id"),
                type_ann: None,
            }),
            decorators: vec![],
            span: swc_common::DUMMY_SP,
        },
        swc_ecma_ast::Param {
            pat: swc_ecma_ast::Pat::Ident(swc_ecma_ast::BindingIdent {
                id: create_ident("component"),
                type_ann: None,
            }),
            decorators: vec![],
            span: swc_common::DUMMY_SP,
        },
    ];

    // Accept a source location (which might be undefiend).
    if development {
        parameters.push(swc_ecma_ast::Param {
            pat: swc_ecma_ast::Pat::Ident(swc_ecma_ast::BindingIdent {
                id: create_ident("place"),
                type_ann: None,
            }),
            decorators: vec![],
            span: swc_common::DUMMY_SP,
        });
    }

    let mut message = vec![
        create_str_expression("Expected "),
        // `component ? "component" : "object"`
        swc_ecma_ast::Expr::Paren(swc_ecma_ast::ParenExpr {
            expr: Box::new(swc_ecma_ast::Expr::Cond(swc_ecma_ast::CondExpr {
                test: Box::new(create_ident_expression("component")),
                cons: Box::new(create_str_expression("component")),
                alt: Box::new(create_str_expression("object")),
                span: swc_common::DUMMY_SP,
            })),
            span: swc_common::DUMMY_SP,
        }),
        create_str_expression(" `"),
        create_ident_expression("id"),
        create_str_expression("` to be defined: you likely forgot to import, pass, or provide it."),
    ];

    // `place ? "\nIt’s referenced in your code at `" + place+ "`" : ""`
    if development {
        message.push(swc_ecma_ast::Expr::Paren(swc_ecma_ast::ParenExpr {
            expr: Box::new(swc_ecma_ast::Expr::Cond(swc_ecma_ast::CondExpr {
                test: Box::new(create_ident_expression("place")),
                cons: Box::new(create_binary_expression(
                    vec![
                        create_str_expression("\nIt’s referenced in your code at `"),
                        create_ident_expression("place"),
                        if let Some(path) = path {
                            create_str_expression(&format!("` in `{}`", path))
                        } else {
                            create_str_expression("`")
                        },
                    ],
                    swc_ecma_ast::BinaryOp::Add,
                )),
                alt: Box::new(create_str_expression("")),
                span: swc_common::DUMMY_SP,
            })),
            span: swc_common::DUMMY_SP,
        }));
    }

    swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Fn(
        swc_ecma_ast::FnDecl {
            ident: create_ident("_missingMdxReference"),
            declare: false,
            function: Box::new(swc_ecma_ast::Function {
                params: parameters,
                decorators: vec![],
                body: Some(swc_ecma_ast::BlockStmt {
                    stmts: vec![swc_ecma_ast::Stmt::Throw(swc_ecma_ast::ThrowStmt {
                        arg: Box::new(swc_ecma_ast::Expr::New(swc_ecma_ast::NewExpr {
                            callee: Box::new(create_ident_expression("Error")),
                            args: Some(vec![swc_ecma_ast::ExprOrSpread {
                                spread: None,
                                expr: Box::new(create_binary_expression(
                                    message,
                                    swc_ecma_ast::BinaryOp::Add,
                                )),
                            }]),
                            span: swc_common::DUMMY_SP,
                            type_args: None,
                        })),
                        span: swc_common::DUMMY_SP,
                    })],
                    span: swc_common::DUMMY_SP,
                }),
                is_generator: false,
                is_async: false,
                type_params: None,
                return_type: None,
                span: swc_common::DUMMY_SP,
            }),
        },
    )))
}

/// Check if this function is a props receiving component: it’s one of ours.
fn is_props_receiving_fn(name: &Option<String>) -> bool {
    if let Some(name) = name {
        name == "_createMdxContent" || name == "MDXContent"
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hast_util_to_swc::hast_util_to_swc;
    use crate::mdast_util_to_hast::mdast_util_to_hast;
    use crate::mdx_plugin_recma_document::{mdx_plugin_recma_document, Options as DocumentOptions};
    use crate::swc::{parse_esm, parse_expression, serialize};
    use markdown::{to_mdast, Location, ParseOptions};
    use pretty_assertions::assert_eq;

    fn compile(value: &str, options: &Options) -> Result<String, String> {
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
        let mut program = hast_util_to_swc(&hast, Some("example.mdx".into()), Some(&location))?;
        mdx_plugin_recma_document(&mut program, &DocumentOptions::default(), Some(&location))?;
        mdx_plugin_recma_jsx_rewrite(&mut program, options, Some(&location));
        Ok(serialize(&program.module, Some(&program.comments)))
    }

    #[test]
    fn core() -> Result<(), String> {
        assert_eq!(
            compile("", &Options::default())?,
            "function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should work on an empty file",
        );

        Ok(())
    }

    #[test]
    fn passing() -> Result<(), String> {
        assert_eq!(
            compile("# hi", &Options::default())?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return <_components.h1 >{\"hi\"}</_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support passing in a layout (as `wrapper`) and components for literal tags",
        );

        assert_eq!(
            compile(
                "export {MyLayout as default} from './a.js'\n\n# hi",
                &Options::default()
            )?,
            "import { MyLayout as MDXLayout } from './a.js';
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return <_components.h1 >{\"hi\"}</_components.h1>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should not support passing in a layout if one is defined locally",
        );

        assert_eq!(
            compile("# <Hi />", &Options::default())?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components), { Hi  } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true);
    return <_components.h1 ><Hi /></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support passing in a component",
        );

        assert_eq!(
            compile("<X />, <X.y />, <Y.Z />", &Options::default())?,
          "function _createMdxContent(props) {
    const _components = Object.assign({
        p: \"p\"
    }, props.components), { X , Y  } = _components;
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    return <_components.p ><X />{\", \"}<X.y />{\", \"}<Y.Z /></_components.p>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support passing in component objects",
        );

        assert_eq!(
            compile("import {Hi} from './a.js'\n\n# <Hi />", &Options::default())?,
            "import { Hi } from './a.js';
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return <_components.h1 ><Hi /></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should not support passing in a component if one is defined locally",
        );

        assert_eq!(
            compile("# <a-b />", &Options::default())?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\",
        \"a-b\": \"a-b\"
    }, props.components), _component0 = _components[\"a-b\"];
    return <_components.h1 ><_component0 /></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support passing in a component for a JSX identifier that is not a valid JS identifier",
        );

        Ok(())
    }

    #[test]
    fn provider() -> Result<(), String> {
        assert_eq!(
            compile(
                "# <Hi />",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, _provideComponents(), props.components), { Hi  } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true);
    return <_components.h1 ><Hi /></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing a layout, literal tags, and components",
        );

        assert_eq!(
            compile(
                "",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support a provider on an empty file",
        );

        assert_eq!(
            compile(
                "<X />, <X.y />, <Y.Z />",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
function _createMdxContent(props) {
    const _components = Object.assign({
        p: \"p\"
    }, _provideComponents(), props.components), { X , Y  } = _components;
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    return <_components.p ><X />{\", \"}<X.y />{\", \"}<Y.Z /></_components.p>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing component objects",
        );

        assert_eq!(
            compile(
                "export function A() {
    return <B />
}

<A />
",
            &Options::default()
        )?,
            "export function A() {
    return <B />;
}
function _createMdxContent(props) {
    return <A />;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should not support passing components in locally defined components",
        );

        assert_eq!(
            compile(
                "export function A() {
    return <B />
}

<A />
",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const { B  } = _provideComponents();
    if (!B) _missingMdxReference(\"B\", true);
    return <B />;
}
function _createMdxContent(props) {
    return <A />;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in locally defined components",
        );

        assert_eq!(
            compile(
                "export function A() {
    return <b-c />
}

<A />
",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const _components = Object.assign({
        \"b-c\": \"b-c\"
    }, _provideComponents()), _component0 = _components[\"b-c\"];
    return <_component0 />;
}
function _createMdxContent(props) {
    return <A />;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support providing components with JSX identifiers that are not JS identifiers in locally defined components",
        );

        assert_eq!(
            compile(
                "export function A() {
    return <X />, <X.y />, <Y.Z />
}

<A />
",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const { X , Y  } = _provideComponents();
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    return <X />, <X.y />, <Y.Z />;
}
function _createMdxContent(props) {
    return <A />;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components with JSX identifiers that are not JS identifiers in locally defined components",
        );

        Ok(())
    }

    #[test]
    fn development() -> Result<(), String> {
        assert_eq!(
            compile("# <Hi />", &Options {
                development: true,
                ..Options::default()
            })?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components), { Hi  } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true, \"1:3-1:9\");
    return <_components.h1 ><Hi /></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component, place) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\" + (place ? \"\\nIt’s referenced in your code at `\" + place + \"` in `example.mdx`\" : \"\"));
}
",
            "should create missing reference helpers w/o positional info in `development` mode",
        );

        Ok(())
    }
}
