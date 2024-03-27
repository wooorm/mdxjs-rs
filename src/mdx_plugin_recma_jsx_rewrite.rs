//! Rewrite JSX tags to accept them from props and an optional provider.
//!
//! Port of <https://github.com/mdx-js/mdx/blob/main/packages/mdx/lib/plugin/recma-jsx-rewrite.js>,
//! by the same author.

use crate::hast_util_to_swc::{Program, MAGIC_EXPLICIT_MARKER};
use crate::swc_utils::{
    create_binary_expression, create_bool_expression, create_call_expression, create_ident,
    create_ident_expression, create_jsx_name_from_str, create_member,
    create_member_expression_from_str, create_member_prop_from_str, create_object_expression,
    create_prop_name, create_str, create_str_expression, is_identifier_name, is_literal_name,
    jsx_member_to_parts, position_to_string, span_to_position,
};
use markdown::{unist::Position, Location};
use swc_core::common::{util::take::Take, Span, DUMMY_SP};
use swc_core::ecma::ast::{
    ArrowExpr, AssignPatProp, BinaryOp, BindingIdent, BlockStmt, BlockStmtOrExpr, Callee,
    CatchClause, ClassDecl, CondExpr, Decl, DoWhileStmt, Expr, ExprOrSpread, ExprStmt, FnDecl,
    FnExpr, ForInStmt, ForOfStmt, ForStmt, Function, IfStmt, ImportDecl, ImportNamedSpecifier,
    ImportPhase, ImportSpecifier, JSXElement, JSXElementName, KeyValuePatProp, KeyValueProp,
    MemberExpr, MemberProp, ModuleDecl, ModuleExportName, ModuleItem, NewExpr, ObjectPat,
    ObjectPatProp, Param, ParenExpr, Pat, Prop, PropOrSpread, ReturnStmt, Stmt, ThrowStmt,
    UnaryExpr, UnaryOp, VarDecl, VarDeclKind, VarDeclarator, WhileStmt,
};
use swc_core::ecma::visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

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
    Decl(&'a mut FnDecl),
    /// Function expression.
    Expr(&'a mut FnExpr),
    /// Arrow function.
    Arrow(&'a mut ArrowExpr),
}

/// Non-literal reference.
#[derive(Debug, Default, Clone)]
struct Dynamic {
    /// Name.
    ///
    /// ```jsx
    /// "a.b.c"
    /// "A"
    /// ```
    name: String,
    /// Component or not (in which case, object).
    component: bool,
    /// Positional info where it was (first) referenced.
    position: Option<Position>,
}

/// Alias.
#[derive(Debug, Default, Clone)]
struct Alias {
    /// Unsafe.
    original: String,
    /// Safe.
    safe: String,
}

/// Info for a function scope.
#[derive(Debug, Default, Clone)]
struct Info {
    /// Function name.
    name: Option<String>,
    /// Used literals (`<a />`).
    literal: Vec<String>,
    /// Non-literal references (components and objects).
    dynamic: Vec<Dynamic>,
    /// List of JSX identifiers of literal that are not valid JS identifiers.
    aliases: Vec<Alias>,
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
        let info = scope.info.take().unwrap();
        let mut statements = vec![];

        if !info.literal.is_empty() || !info.dynamic.is_empty() {
            let mut parameters = vec![];

            // Use a provider, if configured.
            //
            // ```jsx
            // _provideComponents()
            // ```
            if self.provider {
                self.create_provider_import = true;
                let call = create_ident_expression("_provideComponents");
                let callee = Callee::Expr(Box::new(call));
                parameters.push(create_call_expression(callee, vec![]));
            }

            // Accept `components` as a prop if this is the `MDXContent` or
            // `_createMdxContent` function.
            //
            // ```jsx
            // props.components
            // ```
            if is_props_receiving_fn(&info.name) {
                let member = MemberExpr {
                    obj: Box::new(create_ident_expression("props")),
                    prop: MemberProp::Ident(create_ident("components")),
                    span: DUMMY_SP,
                };
                parameters.push(Expr::Member(member));
            }

            // Create defaults for literal tags.
            //
            // Literal tags are optional.
            // When they are not passed, they default to their tag name.
            //
            // ```jsx
            // {h1: 'h1'}
            // ```
            let mut index = 0;
            while index < info.literal.len() {
                let name = &info.literal[index];

                defaults.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: create_prop_name(name),
                    value: Box::new(create_str_expression(name)),
                }))));

                index += 1;
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
                parameters.insert(0, create_object_expression(defaults));
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
                    args.push(ExprOrSpread {
                        spread: None,
                        expr: Box::new(param),
                    });
                }
                let callee = create_member_expression_from_str("Object.assign");
                create_call_expression(Callee::Expr(Box::new(callee)), args)
            } else {
                // Always one.
                let param = parameters.pop().unwrap();

                if let Expr::Member(_) = param {
                    create_binary_expression(
                        vec![param, create_object_expression(vec![])],
                        BinaryOp::LogicalOr,
                    )
                } else {
                    param
                }
            };

            let mut declarators = vec![];

            // If there are tags, they are taken from `_components`, so we need
            // to make it defined.
            if !info.literal.is_empty() {
                let declarator = VarDeclarator {
                    span: DUMMY_SP,
                    name: Pat::Ident(BindingIdent {
                        id: create_ident("_components"),
                        type_ann: None,
                    }),
                    init: Some(Box::new(components_init)),
                    definite: false,
                };
                declarators.push(declarator);
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
                let mut index = 0;
                while index < info.aliases.len() {
                    let alias = &info.aliases[index];
                    let declarator = VarDeclarator {
                        span: DUMMY_SP,
                        name: Pat::Ident(BindingIdent {
                            id: create_ident(&alias.safe),
                            type_ann: None,
                        }),
                        init: Some(Box::new(Expr::Member(create_member(
                            create_ident_expression("_components"),
                            create_member_prop_from_str(&alias.original),
                        )))),
                        definite: false,
                    };
                    declarators.push(declarator);
                    index += 1;
                }
            }

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
            let mut props = vec![];

            for reference in &info.dynamic {
                let invalid = info.aliases.iter().any(|d| d.original == reference.name);
                // The primary ID of objects and components that are referenced.
                // Ignore if invalid.
                if !reference.name.contains('.') && !invalid {
                    // `wrapper: MDXLayout`
                    if reference.name == "MDXLayout" {
                        let binding = BindingIdent {
                            id: create_ident(&reference.name),
                            type_ann: None,
                        };
                        let prop = KeyValuePatProp {
                            key: create_prop_name("wrapper"),
                            value: Box::new(Pat::Ident(binding)),
                        };
                        props.push(ObjectPatProp::KeyValue(prop));
                    } else {
                        // `MyComponent`
                        let prop = AssignPatProp {
                            key: create_ident(&reference.name).into(),
                            value: None,
                            span: DUMMY_SP,
                        };
                        props.push(ObjectPatProp::Assign(prop));
                    }
                }
            }

            if !props.is_empty() {
                let pat = ObjectPat {
                    props,
                    optional: false,
                    span: DUMMY_SP,
                    type_ann: None,
                };
                let declarator = VarDeclarator {
                    name: Pat::Object(pat),
                    init: Some(Box::new(components_init)),
                    span: DUMMY_SP,
                    definite: false,
                };
                declarators.push(declarator);
            };

            // Add the variable declaration.
            let decl = VarDecl {
                kind: VarDeclKind::Const,
                decls: declarators,
                span: DUMMY_SP,
                declare: false,
            };
            let var_decl = Decl::Var(Box::new(decl));
            statements.push(Stmt::Decl(var_decl));
        }

        // Add checks at runtime to verify that object/components are passed.
        //
        // ```js
        // if (!a) _missingMdxReference("a", false);
        // if (!a.b) _missingMdxReference("a.b", true);
        // ```
        for reference in info.dynamic {
            // We use a conditional to check if `MDXLayout` is defined or not
            // in the `MDXContent` component.
            let layout = reference.name == "MDXLayout" && info.name == Some("MDXContent".into());

            if !layout {
                self.create_error_helper = true;

                let mut args = vec![
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(create_str_expression(&reference.name)),
                    },
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(create_bool_expression(reference.component)),
                    },
                ];

                // Add the source location if it exists and if `development` is on.
                if let Some(position) = reference.position.as_ref() {
                    if self.development {
                        args.push(ExprOrSpread {
                            spread: None,
                            expr: Box::new(create_str_expression(&position_to_string(position))),
                        });
                    }
                }

                let mut name = reference.name;
                let split = name.split('.');
                let mut path = split.map(String::from).collect::<Vec<_>>();
                let alias = info.aliases.iter().find(|d| d.original == path[0]);
                if let Some(alias) = alias {
                    path[0] = alias.safe.clone();
                    name = path.join(".");
                }
                let test = UnaryExpr {
                    op: UnaryOp::Bang,
                    arg: Box::new(create_member_expression_from_str(&name)),
                    span: DUMMY_SP,
                };
                let callee = create_ident_expression("_missingMdxReference");
                let call = create_call_expression(Callee::Expr(Box::new(callee)), args);
                let cons = ExprStmt {
                    span: DUMMY_SP,
                    expr: Box::new(call),
                };
                let statement = IfStmt {
                    test: Box::new(Expr::Unary(test)),
                    cons: Box::new(Stmt::Expr(cons)),
                    alt: None,
                    span: DUMMY_SP,
                };
                statements.push(Stmt::If(statement));
            }
        }

        // Add statements to functions.
        if !statements.is_empty() {
            let body: &mut BlockStmt = match func {
                Func::Expr(expr) => {
                    // Always exists if we have components in it.
                    expr.function.body.as_mut().unwrap()
                }
                Func::Decl(decl) => {
                    // Always exists if we have components in it.
                    decl.function.body.as_mut().unwrap()
                }
                Func::Arrow(arr) => {
                    if let BlockStmtOrExpr::Expr(expr) = &mut *arr.body {
                        let block = BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                arg: Some(expr.take()),
                                span: DUMMY_SP,
                            })],
                            span: DUMMY_SP,
                        };
                        arr.body = Box::new(BlockStmtOrExpr::BlockStmt(block));
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

    /// Get the top-level scope’s info, mutably.
    fn current_top_level_info(&mut self) -> Option<&mut Info> {
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

    /// Reference a literal tag name.
    fn ref_tag(&mut self, name: &str) {
        let scope = self.current_top_level_info().expect("expected scope");
        let name = name.to_string();
        if !scope.literal.contains(&name) {
            scope.literal.push(name);
        }
    }

    /// Reference a component or object name.
    fn ref_dynamic(&mut self, path: &[String], component: bool, position: &Option<Position>) {
        let scope = self.current_top_level_info().expect("expected scope");
        let name = path.join(".");
        let existing = scope.dynamic.iter_mut().find(|d| d.name == name);

        if let Some(existing) = existing {
            if component {
                existing.component = component;
            }
        } else {
            let dynamic = Dynamic {
                name,
                component,
                position: position.clone(),
            };

            scope.dynamic.push(dynamic);
        }
    }

    fn create_alias(&mut self, id: &str) -> String {
        let scope = self.current_top_level_info().expect("expected scope");
        let existing = scope.aliases.iter().find(|d| d.original == id);

        if let Some(alias) = existing {
            alias.safe.to_string()
        } else {
            let name = format!("_component{}", scope.aliases.len());
            scope.aliases.push(Alias {
                original: id.to_string(),
                safe: name.clone(),
            });
            name
        }
    }

    fn ref_ids(&mut self, ids: &[String], span: &Span) -> Option<JSXElementName> {
        // If there is a top-level, non-global, scope which is a function:
        if let Some(info) = self.current_top_level_info() {
            // Rewrite only if we can rewrite.
            if is_props_receiving_fn(&info.name) || self.provider {
                debug_assert!(!ids.is_empty(), "expected non-empty ids");
                let explicit_jsx = span.ctxt.as_u32() == MAGIC_EXPLICIT_MARKER;
                let mut path = ids.to_owned();
                let position = span_to_position(span, self.location);

                // A tag name of a literal element (not a component).
                if ids.len() == 1 && is_literal_name(&path[0]) {
                    self.ref_tag(&path[0]);

                    // The author did not used explicit JSX (`<h1>a</h1>`),
                    // but markdown (`# a`), so rewrite.
                    if !explicit_jsx {
                        path.insert(0, "_components".into());
                    }
                } else if !self.in_scope(&path[0]) {
                    // Component or object not in scope.
                    let mut index = 1;
                    while index <= path.len() {
                        self.ref_dynamic(&path[0..index], index == ids.len(), &position);
                        index += 1;
                    }
                }

                // If the primary ID is not a valid JS ID:
                if !is_identifier_name(&path[0]) {
                    path[0] = self.create_alias(&path[0]);
                }

                if path != ids {
                    return Some(create_jsx_name_from_str(&path.join(".")));
                }
            }
        }

        None
    }

    /// Define an identifier in a scope.
    fn define_id(&mut self, id: String, block: bool) {
        let scope = if block {
            self.current_scope_mut()
        } else {
            self.current_fn_scope_mut()
        };
        scope.defined.push(id);
    }

    /// Define a pattern in a scope.
    fn define_pat(&mut self, pat: &Pat, block: bool) {
        // `x`
        if let Pat::Ident(d) = pat {
            self.define_id(d.id.sym.to_string(), block);
        }

        // `...x`
        if let Pat::Array(d) = pat {
            let mut index = 0;
            while index < d.elems.len() {
                if let Some(d) = &d.elems[index] {
                    self.define_pat(d, block);
                }
                index += 1;
            }
        }

        // `...x`
        if let Pat::Rest(d) = pat {
            self.define_pat(&d.arg, block);
        }

        // `{x=y}`
        if let Pat::Assign(d) = pat {
            self.define_pat(&d.left, block);
        }

        if let Pat::Object(d) = pat {
            let mut index = 0;
            while index < d.props.len() {
                match &d.props[index] {
                    // `{...x}`
                    ObjectPatProp::Rest(d) => {
                        self.define_pat(&d.arg, block);
                    }
                    // `{key: value}`
                    ObjectPatProp::KeyValue(d) => {
                        self.define_pat(&d.value, block);
                    }
                    // `{key}` or `{key = value}`
                    ObjectPatProp::Assign(d) => {
                        self.define_id(d.key.sym.to_string(), block);
                    }
                }
                index += 1;
            }
        }
    }
}

impl<'a> VisitMut for State<'a> {
    noop_visit_mut_type!();

    /// Rewrite JSX identifiers.
    fn visit_mut_jsx_element(&mut self, node: &mut JSXElement) {
        let parts = match &node.opening.name {
            // `<x.y>`, `<Foo.Bar>`, `<x.y.z>`.
            JSXElementName::JSXMemberExpr(d) => {
                let parts = jsx_member_to_parts(d);
                parts.into_iter().map(String::from).collect::<Vec<_>>()
            }
            // `<foo>`, `<Foo>`, `<$>`, `<_bar>`, `<a_b>`.
            JSXElementName::Ident(d) => vec![(d.sym).to_string()],
            // `<xml:thing>`.
            JSXElementName::JSXNamespacedName(d) => {
                vec![format!("{}:{}", d.ns.sym, d.name.sym)]
            }
        };

        if let Some(name) = self.ref_ids(&parts, &node.span) {
            if let Some(closing) = node.closing.as_mut() {
                closing.name = name.clone();
            }

            node.opening.name = name;
        }

        node.visit_mut_children_with(self);
    }

    /// Add specifiers of import declarations.
    fn visit_mut_import_decl(&mut self, node: &mut ImportDecl) {
        let mut index = 0;
        while index < node.specifiers.len() {
            let ident = match &node.specifiers[index] {
                ImportSpecifier::Default(x) => &x.local.sym,
                ImportSpecifier::Namespace(x) => &x.local.sym,
                ImportSpecifier::Named(x) => &x.local.sym,
            };
            self.define_id(ident.to_string(), false);
            index += 1;
        }

        node.visit_mut_children_with(self);
    }

    /// Add patterns of variable declarations.
    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        let block = node.kind != VarDeclKind::Var;
        let mut index = 0;
        while index < node.decls.len() {
            self.define_pat(&node.decls[index].name, block);
            index += 1;
        }
        node.visit_mut_children_with(self);
    }

    /// Add identifier of class declaration.
    fn visit_mut_class_decl(&mut self, node: &mut ClassDecl) {
        self.define_id(node.ident.sym.to_string(), false);
        node.visit_mut_children_with(self);
    }

    /// On function declarations, add name, create scope, add parameters.
    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        let id = node.ident.sym.to_string();
        self.define_id(id.clone(), false);
        self.enter(Some(Info {
            name: Some(id),
            ..Default::default()
        }));
        let mut index = 0;
        while index < node.function.params.len() {
            self.define_pat(&node.function.params[index].pat, false);
            index += 1;
        }
        node.visit_mut_children_with(self);
        // Rewrite.
        self.exit_func(Func::Decl(node));
    }

    /// On function expressions, add name, create scope, add parameters.
    fn visit_mut_fn_expr(&mut self, node: &mut FnExpr) {
        // Note: `periscopic` adds the ID to the newly generated scope, for
        // fn expressions.
        // That seems wrong?
        let name = if let Some(ident) = &node.ident {
            let id = ident.sym.to_string();
            self.define_id(id.clone(), false);
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
            self.define_pat(&node.function.params[index].pat, false);
            index += 1;
        }
        node.visit_mut_children_with(self);
        self.exit_func(Func::Expr(node));
    }

    /// On arrow functions, create scope, add parameters.
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        self.enter(Some(Info::default()));
        let mut index = 0;
        while index < node.params.len() {
            self.define_pat(&node.params[index], false);
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
    fn visit_mut_for_stmt(&mut self, node: &mut ForStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On for/in statements, create scope.
    fn visit_mut_for_in_stmt(&mut self, node: &mut ForInStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On for/of statements, create scope.
    fn visit_mut_for_of_stmt(&mut self, node: &mut ForOfStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On while statements, create scope.
    fn visit_mut_while_stmt(&mut self, node: &mut WhileStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On do/while statements, create scope.
    fn visit_mut_do_while_stmt(&mut self, node: &mut DoWhileStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }
    /// On block statements, create scope.
    fn visit_mut_block_stmt(&mut self, node: &mut BlockStmt) {
        self.enter(None);
        node.visit_mut_children_with(self);
        self.exit();
    }

    /// On catch clauses, create scope, add param.
    fn visit_mut_catch_clause(&mut self, node: &mut CatchClause) {
        self.enter(None);
        if let Some(pat) = &node.param {
            self.define_pat(pat, true);
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
fn create_import_provider(source: &str) -> ModuleItem {
    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
        specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
            local: create_ident("_provideComponents"),
            imported: Some(ModuleExportName::Ident(create_ident("useMDXComponents"))),
            span: DUMMY_SP,
            is_type_only: false,
        })],
        src: Box::new(create_str(source)),
        type_only: false,
        with: None,
        phase: ImportPhase::default(),
        span: DUMMY_SP,
    }))
}

/// Generate an error helper.
///
/// ```js
/// function _missingMdxReference(id, component) {
///   throw new Error("Expected " + (component ? "component" : "object") + " `" + id + "` to be defined: you likely forgot to import, pass, or provide it.");
/// }
/// ```
fn create_error_helper(development: bool, path: Option<String>) -> ModuleItem {
    let mut parameters = vec![
        Param {
            pat: Pat::Ident(BindingIdent {
                id: create_ident("id"),
                type_ann: None,
            }),
            decorators: vec![],
            span: DUMMY_SP,
        },
        Param {
            pat: Pat::Ident(BindingIdent {
                id: create_ident("component"),
                type_ann: None,
            }),
            decorators: vec![],
            span: DUMMY_SP,
        },
    ];

    // Accept a source location (which might be undefiend).
    if development {
        parameters.push(Param {
            pat: Pat::Ident(BindingIdent {
                id: create_ident("place"),
                type_ann: None,
            }),
            decorators: vec![],
            span: DUMMY_SP,
        });
    }

    let mut message = vec![
        create_str_expression("Expected "),
        // `component ? "component" : "object"`
        Expr::Paren(ParenExpr {
            expr: Box::new(Expr::Cond(CondExpr {
                test: Box::new(create_ident_expression("component")),
                cons: Box::new(create_str_expression("component")),
                alt: Box::new(create_str_expression("object")),
                span: DUMMY_SP,
            })),
            span: DUMMY_SP,
        }),
        create_str_expression(" `"),
        create_ident_expression("id"),
        create_str_expression("` to be defined: you likely forgot to import, pass, or provide it."),
    ];

    // `place ? "\nIt’s referenced in your code at `" + place+ "`" : ""`
    if development {
        message.push(Expr::Paren(ParenExpr {
            expr: Box::new(Expr::Cond(CondExpr {
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
                    BinaryOp::Add,
                )),
                alt: Box::new(create_str_expression("")),
                span: DUMMY_SP,
            })),
            span: DUMMY_SP,
        }));
    }

    ModuleItem::Stmt(Stmt::Decl(Decl::Fn(FnDecl {
        ident: create_ident("_missingMdxReference"),
        declare: false,
        function: Box::new(Function {
            params: parameters,
            decorators: vec![],
            body: Some(BlockStmt {
                stmts: vec![Stmt::Throw(ThrowStmt {
                    arg: Box::new(Expr::New(NewExpr {
                        callee: Box::new(create_ident_expression("Error")),
                        args: Some(vec![ExprOrSpread {
                            spread: None,
                            expr: Box::new(create_binary_expression(message, BinaryOp::Add)),
                        }]),
                        span: DUMMY_SP,
                        type_args: None,
                    })),
                    span: DUMMY_SP,
                })],
                span: DUMMY_SP,
            }),
            is_generator: false,
            is_async: false,
            type_params: None,
            return_type: None,
            span: DUMMY_SP,
        }),
    })))
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
    use crate::swc_utils::create_jsx_name_from_str;
    use crate::Error;
    use markdown::{to_mdast, Location, ParseOptions};
    use pretty_assertions::assert_eq;
    use swc_core::ecma::ast::{Invalid, JSXOpeningElement, Module};

    fn compile(value: &str, options: &Options, named: bool) -> Result<String, Error> {
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
        let filepath = if named {
            Some("example.mdx".into())
        } else {
            None
        };
        let mut program = hast_util_to_swc(&hast, filepath, Some(&location))?;
        mdx_plugin_recma_document(&mut program, &DocumentOptions::default(), Some(&location))?;
        mdx_plugin_recma_jsx_rewrite(&mut program, options, Some(&location));
        Ok(serialize(&mut program.module, Some(&program.comments)))
    }

    #[test]
    fn empty() -> Result<(), Error> {
        assert_eq!(
            compile("", &Options::default(), true)?,
            "function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should work on an empty file",
        );

        Ok(())
    }

    #[test]
    fn pass_literal() -> Result<(), Error> {
        assert_eq!(
            compile("# hi", &Options::default(), true)?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return <_components.h1>{\"hi\"}</_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support passing in a layout (as `wrapper`) and components for literal tags",
        );

        Ok(())
    }

    #[test]
    fn pass_namespace() -> Result<(), Error> {
        assert_eq!(
            compile("<a:b />", &Options::default(), true)?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        \"a:b\": \"a:b\"
    }, props.components), _component0 = _components[\"a:b\"];
    return <_component0/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support passing in a component for a JSX namespace name (`x:y`)",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_defined_layout_import_named() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export {MyLayout as default} from './a.js'\n\n# hi",
                &Options::default(),
                true
            )?,
            "import { MyLayout as MDXLayout } from './a.js';
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return <_components.h1>{\"hi\"}</_components.h1>;
}
function MDXContent(props = {}) {
    return <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout>;
}
export default MDXContent;
",
            "should not support passing in a layout if one is defined locally",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_missing_component() -> Result<(), Error> {
        assert_eq!(
            compile("# <Hi />", &Options::default(), true)?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components), { Hi } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true);
    return <_components.h1><Hi/></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support passing in a component",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_missing_objects_in_component() -> Result<(), Error> {
        assert_eq!(
            compile("<X />, <X.y />, <Y.Z />, <a.b.c.d />, <a.b />", &Options::default(), true)?,
          "function _createMdxContent(props) {
    const _components = Object.assign({
        p: \"p\"
    }, props.components), { X, Y, a } = _components;
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    if (!a) _missingMdxReference(\"a\", false);
    if (!a.b) _missingMdxReference(\"a.b\", true);
    if (!a.b.c) _missingMdxReference(\"a.b.c\", false);
    if (!a.b.c.d) _missingMdxReference(\"a.b.c.d\", true);
    return <_components.p><X/>{\", \"}<X.y/>{\", \"}<Y.Z/>{\", \"}<a.b.c.d/>{\", \"}<a.b/></_components.p>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support passing in component objects",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_missing_non_js_identifiers() -> Result<(), Error> {
        assert_eq!(
            compile("# <a-b />, <qwe-rty />, <a-b />, <c-d.e-f />", &Options::default(), true)?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\",
        \"a-b\": \"a-b\",
        \"qwe-rty\": \"qwe-rty\"
    }, props.components), _component0 = _components[\"a-b\"], _component1 = _components[\"qwe-rty\"], _component2 = _components[\"c-d\"];
    if (!_component2) _missingMdxReference(\"c-d\", false);
    if (!_component2[\"e-f\"]) _missingMdxReference(\"c-d.e-f\", true);
    return <_components.h1><_component0/>{\", \"}<_component1/>{\", \"}<_component0/>{\", \"}<_component2.e-f/></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support passing in a component for a JSX identifier that is not a valid JS identifier",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_defined_import_named() -> Result<(), Error> {
        assert_eq!(
            compile("import {Hi} from './a.js'\n\n# <Hi />", &Options::default(), true)?,
            "import { Hi } from './a.js';
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return <_components.h1><Hi/></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should not support passing in a component if one is defined locally",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_defined_import_namespace() -> Result<(), Error> {
        assert_eq!(
            compile(
                "import * as X from './a.js'\n\n<X />",
                &Options::default(), true
            )?,
            "import * as X from './a.js';
function _createMdxContent(props) {
    return <X/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should not support passing in a component if one is defined locally (namespace import)",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_defined_function() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export function A() {
    return <B />
}

<A />
",
            &Options::default(), true
        )?,
            "export function A() {
    return <B/>;
}
function _createMdxContent(props) {
    return <A/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should not support passing components in locally defined components",
        );

        Ok(())
    }

    #[test]
    fn pass_scope_defined_class() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export class A {}

<A />
",
            &Options::default(), true
        )?,
            "export class A {
}
function _createMdxContent(props) {
    return <A/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should be aware of classes",
        );

        Ok(())
    }

    #[test]
    fn provide() -> Result<(), Error> {
        assert_eq!(
            compile(
                "# <Hi />",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, _provideComponents(), props.components), { Hi } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true);
    return <_components.h1><Hi/></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing a layout, literal tags, and components",
        );

        Ok(())
    }

    #[test]
    fn provide_empty() -> Result<(), Error> {
        assert_eq!(
            compile(
                "",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support a provider on an empty file",
        );

        Ok(())
    }

    #[test]
    fn provide_local() -> Result<(), Error> {
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
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const { B } = _provideComponents();
    if (!B) _missingMdxReference(\"B\", true);
    return <B/>;
}
function _createMdxContent(props) {
    return <A/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in locally defined components",
        );

        Ok(())
    }

    #[test]
    fn provide_local_scope_defined() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export function X(x) {
    let [A] = x
    let [...B] = x
    let {C} = x
    let {...D} = x
    let {_: E} = x
    let {F = _} = x;
    return <><A /><B /><C /><D /><E /><F /><G /></>
}",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function X(x) {
    const { G } = _provideComponents();
    if (!G) _missingMdxReference(\"G\", true);
    let [A] = x;
    let [...B] = x;
    let { C } = x;
    let { ...D } = x;
    let { _: E } = x;
    let { F = _ } = x;
    return <><A/><B/><C/><D/><E/><F/><G/></>;
}
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in top-level components, aware of scopes (defined)",
        );

        Ok(())
    }

    #[test]
    fn provide_local_scope_missing() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export function A() {
    while (true) {
        let B = true;
        break;
    }

    do {
        let B = true;
        break;
    } while (true)

    for (;;) {
        let B = true;
        break;
    }

    for (a in b) {
        let B = true;
        break;
    }

    for (a of b) {
        let B = true;
        break;
    }

    try {
        let B = true;
    } catch (B) {
        let B = true;
    }

    ;(function () {
        let B = true;
    })()

    ;(function (B) {})()

    ;(() => {
        let B = true;
    })()

    ;((B) => {})()

    return <B/>
}",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const { B } = _provideComponents();
    if (!B) _missingMdxReference(\"B\", true);
    while(true){
        let B = true;
        break;
    }
    do {
        let B = true;
        break;
    }while (true)
    for(;;){
        let B = true;
        break;
    }
    for(a in b){
        let B = true;
        break;
    }
    for (a of b){
        let B = true;
        break;
    }
    try {
        let B = true;
    } catch (B) {
        let B = true;
    }
    ;
    (function() {
        let B = true;
    })();
    (function(B) {})();
    (()=>{
        let B = true;
    })();
    ((B)=>{})();
    return <B/>;
}
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in top-level components, aware of scopes (missing)",
        );

        Ok(())
    }

    #[test]
    fn provide_local_scope_missing_objects() -> Result<(), Error> {
        assert_eq!(
            compile(
                "<X />, <X.y />, <Y.Z />",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
function _createMdxContent(props) {
    const _components = Object.assign({
        p: \"p\"
    }, _provideComponents(), props.components), { X, Y } = _components;
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    return <_components.p><X/>{\", \"}<X.y/>{\", \"}<Y.Z/></_components.p>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing component objects",
        );

        Ok(())
    }

    #[test]
    fn provide_local_scope_missing_objects_in_component() -> Result<(), Error> {
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
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const { X, Y } = _provideComponents();
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    return <X/>, <X.y/>, <Y.Z/>;
}
function _createMdxContent(props) {
    return <A/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in locally defined components",
        );

        Ok(())
    }

    #[test]
    fn provide_local_arrow_function_component() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export const A = () => <B />",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export const A = ()=>{
    const { B } = _provideComponents();
    if (!B) _missingMdxReference(\"B\", true);
    return <B/>;
};
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in locally defined arrow functions",
        );

        Ok(())
    }

    #[test]
    fn provide_local_function_declaration_component() -> Result<(), Error> {
        assert_eq!(
            compile(
                "export const A = function B() { return <C /> }",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export const A = function B() {
    const { C } = _provideComponents();
    if (!C) _missingMdxReference(\"C\", true);
    return <C/>;
};
function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
            "should support providing components in locally defined function expressions",
        );

        Ok(())
    }

    #[test]
    fn provide_local_non_js_identifiers() -> Result<(), Error> {
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
                }, true
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const _components = Object.assign({
        \"b-c\": \"b-c\"
    }, _provideComponents());
    return <_components.b-c/>;
}
function _createMdxContent(props) {
    return <A/>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should support providing components with JSX identifiers that are not JS identifiers in locally defined components",
        );

        Ok(())
    }

    #[test]
    fn development() -> Result<(), Error> {
        assert_eq!(
            compile("# <Hi />", &Options {
                development: true,
                ..Options::default()
            }, true)?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components), { Hi } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true, \"1:3-1:9\");
    return <_components.h1><Hi/></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
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

    #[test]
    fn development_no_filepath() -> Result<(), Error> {
        assert_eq!(
            compile("# <Hi />", &Options {
                development: true,
                ..Options::default()
            }, false)?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components), { Hi } = _components;
    if (!Hi) _missingMdxReference(\"Hi\", true, \"1:3-1:9\");
    return <_components.h1><Hi/></_components.h1>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component, place) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\" + (place ? \"\\nIt’s referenced in your code at `\" + place + \"`\" : \"\"));
}
",
            "should create missing reference helpers w/o positional info in `development` mode",
        );

        Ok(())
    }

    #[test]
    fn jsx_outside_components() {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    kind: VarDeclKind::Let,
                    decls: vec![VarDeclarator {
                        span: DUMMY_SP,
                        name: Pat::Ident(BindingIdent {
                            id: create_ident("a"),
                            type_ann: None,
                        }),
                        init: Some(Box::new(Expr::JSXElement(Box::new(JSXElement {
                            opening: JSXOpeningElement {
                                name: create_jsx_name_from_str("b"),
                                attrs: vec![],
                                self_closing: true,
                                type_args: None,
                                span: DUMMY_SP,
                            },
                            closing: None,
                            children: vec![],
                            span: DUMMY_SP,
                        })))),
                        definite: false,
                    }],
                    span: DUMMY_SP,
                    declare: false,
                }))))],
            },
        };
        mdx_plugin_recma_jsx_rewrite(&mut program, &Options::default(), None);
        assert_eq!(
            serialize(&mut program.module, None),
            "let a = <b/>;\n",
            "should not rewrite JSX outside of components"
        );
    }

    #[test]
    fn invalid_patterns() {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    kind: VarDeclKind::Let,
                    decls: vec![VarDeclarator {
                        span: DUMMY_SP,
                        name: Pat::Invalid(Invalid { span: DUMMY_SP }),
                        init: None,
                        definite: false,
                    }],
                    span: DUMMY_SP,
                    declare: false,
                }))))],
            },
        };
        mdx_plugin_recma_jsx_rewrite(&mut program, &Options::default(), None);
        assert_eq!(
            serialize(&mut program.module, None),
            "let <invalid>;\n",
            "should ignore invalid patterns"
        );
    }

    #[test]
    fn expr_patterns() {
        let mut program = Program {
            path: None,
            comments: vec![],
            module: Module {
                span: DUMMY_SP,
                shebang: None,
                body: vec![ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    kind: VarDeclKind::Let,
                    decls: vec![VarDeclarator {
                        span: DUMMY_SP,
                        name: Pat::Expr(Box::new(Expr::Ident(create_ident("a")))),
                        init: None,
                        definite: false,
                    }],
                    span: DUMMY_SP,
                    declare: false,
                }))))],
            },
        };
        mdx_plugin_recma_jsx_rewrite(&mut program, &Options::default(), None);
        assert_eq!(
            serialize(&mut program.module, None),
            "let a;\n",
            "should ignore expression patterns"
        );
    }
}
