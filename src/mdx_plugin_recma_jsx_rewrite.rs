//! Rewrite JSX tags to accept them from props and an optional provider.
//!
//! Port of <https://github.com/mdx-js/mdx/blob/main/packages/mdx/lib/plugin/recma-jsx-rewrite.js>,
//! by the same author.

extern crate swc_common;
extern crate swc_ecma_ast;
use crate::hast_util_to_swc::{Program, MAGIC_EXPLICIT_MARKER};
use crate::swc_utils::{
    create_binary_expression, create_bool_expression, create_call_expression, create_ident,
    create_ident_expression, create_member, create_member_expression_from_str,
    create_member_prop_from_str, create_object_expression, create_prop_name, create_str,
    create_str_expression, is_identifier_name, is_literal_name, jsx_member_to_parts,
    position_to_string, span_to_position,
};
use markdown::{unist::Position, Location};
use swc_common::util::take::Take;
use swc_ecma_ast::{
    ArrowExpr, AssignPatProp, BinaryOp, BindingIdent, BlockStmt, BlockStmtOrExpr, Callee,
    CatchClause, ClassDecl, CondExpr, Decl, DoWhileStmt, Expr, ExprOrSpread, ExprStmt, FnDecl,
    FnExpr, ForInStmt, ForOfStmt, ForStmt, Function, IfStmt, ImportDecl, ImportNamedSpecifier,
    ImportSpecifier, JSXElement, JSXElementName, JSXMemberExpr, JSXObject, KeyValuePatProp,
    KeyValueProp, MemberExpr, MemberProp, ModuleDecl, ModuleExportName, ModuleItem, NewExpr,
    ObjectPat, ObjectPatProp, Param, ParenExpr, Pat, Prop, PropOrSpread, ReturnStmt, Stmt,
    ThrowStmt, UnaryExpr, UnaryOp, VarDecl, VarDeclKind, VarDeclarator, WhileStmt,
};
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
    Decl(&'a mut FnDecl),
    /// Function expression.
    Expr(&'a mut FnExpr),
    /// Arrow function.
    Arrow(&'a mut ArrowExpr),
}

/// Non-literal reference.
#[derive(Debug, Default, Clone)]
struct Reference {
    /// Name.
    ///
    /// ```jsx
    /// "a.b.c"
    /// "A"
    /// ```
    name: String,
    /// Component or not.
    component: bool,
    /// Positional info
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
    /// Used objects (`a` in `<a.b />`).
    objects: Vec<String>,
    /// Used components (`<A />`).
    components: Vec<String>,
    /// Used literals (`<a />`).
    tags: Vec<String>,
    /// List of JSX identifiers of literal tags that are not valid JS
    /// identifiers.
    aliases: Vec<Alias>,
    /// Non-literal references (components and objects).
    references: Vec<Reference>,
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

            defaults.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                key: create_prop_name(name),
                value: Box::new(create_str_expression(name)),
            }))));

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
                    span: swc_common::DUMMY_SP,
                };
                parameters.push(Expr::Member(member));
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
                        let binding = BindingIdent {
                            id: create_ident(&key),
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
                            key: create_ident(&key),
                            value: None,
                            span: swc_common::DUMMY_SP,
                        };
                        props.push(ObjectPatProp::Assign(prop));
                    }
                }

                let pat = ObjectPat {
                    props,
                    optional: false,
                    span: swc_common::DUMMY_SP,
                    type_ann: None,
                };
                Some(pat)
            };

            let mut declarators = vec![];

            // If there are tags, they take them from `_components`, so we need
            // to make it defined.
            if !info.tags.is_empty() {
                let declarator = VarDeclarator {
                    span: swc_common::DUMMY_SP,
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
                info.aliases.reverse();

                while let Some(alias) = info.aliases.pop() {
                    let declarator = VarDeclarator {
                        span: swc_common::DUMMY_SP,
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
                }
            }

            if let Some(pat) = components_pattern {
                let declarator = VarDeclarator {
                    name: Pat::Object(pat),
                    init: Some(Box::new(components_init)),
                    span: swc_common::DUMMY_SP,
                    definite: false,
                };
                declarators.push(declarator);
            }

            // Add the variable declaration.
            let decl = VarDecl {
                kind: VarDeclKind::Const,
                decls: declarators,
                span: swc_common::DUMMY_SP,
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
        for reference in info.references {
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

            let test = Expr::Unary(UnaryExpr {
                op: UnaryOp::Bang,
                arg: Box::new(create_member_expression_from_str(&reference.name)),
                span: swc_common::DUMMY_SP,
            });
            let cons = Stmt::Expr(ExprStmt {
                span: swc_common::DUMMY_SP,
                expr: Box::new(create_call_expression(
                    Callee::Expr(Box::new(create_ident_expression("_missingMdxReference"))),
                    args,
                )),
            });
            let statement = Stmt::If(IfStmt {
                test: Box::new(test),
                cons: Box::new(cons),
                alt: None,
                span: swc_common::DUMMY_SP,
            });
            statements.push(statement);
        }

        // Add statements to functions.
        if !statements.is_empty() {
            let mut body: &mut BlockStmt = match func {
                Func::Expr(expr) => {
                    // Always exists if we have components in it.
                    expr.function.body.as_mut().unwrap()
                }
                Func::Decl(decl) => {
                    // Always exists if we have components in it.
                    decl.function.body.as_mut().unwrap()
                }
                Func::Arrow(arr) => {
                    if let BlockStmtOrExpr::Expr(expr) = &mut arr.body {
                        let block = BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                arg: Some(expr.take()),
                                span: swc_common::DUMMY_SP,
                            })],
                            span: swc_common::DUMMY_SP,
                        };
                        arr.body = BlockStmtOrExpr::BlockStmt(block);
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
    fn add_pat(&mut self, pat: &Pat, block: bool) {
        match pat {
            // `x`
            Pat::Ident(d) => self.add_id(d.id.sym.to_string(), block),
            // `...x`
            Pat::Array(d) => {
                let mut index = 0;
                while index < d.elems.len() {
                    if let Some(d) = &d.elems[index] {
                        self.add_pat(d, block);
                    }
                    index += 1;
                }
            }
            // `...x`
            Pat::Rest(d) => self.add_pat(&d.arg, block),
            // `{x=y}`
            Pat::Assign(d) => self.add_pat(&d.left, block),
            Pat::Object(d) => {
                let mut index = 0;
                while index < d.props.len() {
                    match &d.props[index] {
                        // `{...x}`
                        ObjectPatProp::Rest(d) => {
                            self.add_pat(&d.arg, block);
                        }
                        // `{key: value}`
                        ObjectPatProp::KeyValue(d) => {
                            self.add_pat(&d.value, block);
                        }
                        // `{key}` or `{key = value}`
                        ObjectPatProp::Assign(d) => {
                            self.add_id(d.key.sym.to_string(), block);
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
    fn visit_mut_jsx_element(&mut self, node: &mut JSXElement) {
        // If there is a top-level, non-global, scope which is a function.
        if let Some(info) = self.current_top_level_info() {
            // Rewrite only if we can rewrite.
            if is_props_receiving_fn(&info.name) || self.provider {
                let position = span_to_position(&node.span, self.location);
                match &node.opening.name {
                    // `<x.y>`, `<Foo.Bar>`, `<x.y.z>`.
                    JSXElementName::JSXMemberExpr(d) => {
                        let ids = jsx_member_to_parts(d);
                        let primary_id = (*ids[0]).to_string();

                        if !self.in_scope(&primary_id) {
                            let info_mut = self.current_top_level_info_mut().unwrap();

                            let mut index = 1;
                            while index <= ids.len() {
                                let name = ids[0..index].join(".");
                                let component = index == ids.len();
                                let reference =
                                    info_mut.references.iter_mut().find(|d| d.name == name);

                                if let Some(reference) = reference {
                                    if component {
                                        reference.component = true;
                                    }
                                } else {
                                    let reference = Reference {
                                        name,
                                        component,
                                        position: position.clone(),
                                    };
                                    info_mut.references.push(reference);
                                }
                                index += 1;
                            }

                            // We only need to get the first ID.
                            if !info_mut.objects.contains(&primary_id) {
                                info_mut.objects.push(primary_id);
                            }
                        }
                    }
                    // `<foo>`, `<Foo>`, `<$>`, `<_bar>`, `<a_b>`.
                    JSXElementName::Ident(d) => {
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
                                    JSXElementName::Ident(create_ident(&id))
                                } else {
                                    let member = JSXMemberExpr {
                                        obj: JSXObject::Ident(create_ident("_components")),
                                        prop: create_ident(&id),
                                    };
                                    JSXElementName::JSXMemberExpr(member)
                                }
                            } else {
                                let alias = info.aliases.iter().find(|d| d.original == id);
                                let name = if let Some(alias) = alias {
                                    alias.safe.clone()
                                } else {
                                    let name = format!("_component{}", info.aliases.len());
                                    invalid = Some(Alias {
                                        original: id.clone(),
                                        safe: name.clone(),
                                    });
                                    name
                                };

                                JSXElementName::Ident(create_ident(&name))
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
                            // A component.
                            let mut is_layout = false;

                            if let Some(name) = &info.name {
                                if name == "MDXContent" && id == "MDXLayout" {
                                    is_layout = true;
                                }
                            }

                            if !self.in_scope(&id) {
                                let info_mut = self.current_top_level_info_mut().unwrap();

                                if !is_layout {
                                    let reference =
                                        info_mut.references.iter_mut().find(|d| d.name == id);

                                    if let Some(reference) = reference {
                                        reference.component = true;
                                    } else {
                                        let reference = Reference {
                                            name: id.clone(),
                                            component: true,
                                            position,
                                        };
                                        info_mut.references.push(reference);
                                    }
                                }

                                if !info_mut.components.contains(&id) {
                                    info_mut.components.push(id);
                                }
                            }
                        }
                    }
                    // `<xml:thing>`.
                    JSXElementName::JSXNamespacedName(_) => {
                        // Ignore.
                    }
                }
            }
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
            self.add_id(ident.to_string(), false);
            index += 1;
        }

        node.visit_mut_children_with(self);
    }

    /// Add patterns of variable declarations.
    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        let block = node.kind != VarDeclKind::Var;
        let mut index = 0;
        while index < node.decls.len() {
            self.add_pat(&node.decls[index].name, block);
            index += 1;
        }
        node.visit_mut_children_with(self);
    }

    /// Add identifier of class declaration.
    fn visit_mut_class_decl(&mut self, node: &mut ClassDecl) {
        self.add_id(node.ident.sym.to_string(), false);
        node.visit_mut_children_with(self);
    }

    /// On function declarations, add name, create scope, add parameters.
    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
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
    fn visit_mut_fn_expr(&mut self, node: &mut FnExpr) {
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
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
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
fn create_import_provider(source: &str) -> ModuleItem {
    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
        specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
            local: create_ident("_provideComponents"),
            imported: Some(ModuleExportName::Ident(create_ident("useMDXComponents"))),
            span: swc_common::DUMMY_SP,
            is_type_only: false,
        })],
        src: Box::new(create_str(source)),
        type_only: false,
        asserts: None,
        span: swc_common::DUMMY_SP,
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
            span: swc_common::DUMMY_SP,
        },
        Param {
            pat: Pat::Ident(BindingIdent {
                id: create_ident("component"),
                type_ann: None,
            }),
            decorators: vec![],
            span: swc_common::DUMMY_SP,
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
            span: swc_common::DUMMY_SP,
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
                span: swc_common::DUMMY_SP,
            })),
            span: swc_common::DUMMY_SP,
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
        Ok(serialize(&mut program.module, Some(&program.comments)))
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
            compile("<X />, <X.y />, <Y.Z />, <a.b.c.d />, <a.b />", &Options::default())?,
          "function _createMdxContent(props) {
    const _components = Object.assign({
        p: \"p\"
    }, props.components), { X , Y , a  } = _components;
    if (!X) _missingMdxReference(\"X\", true);
    if (!X.y) _missingMdxReference(\"X.y\", true);
    if (!Y) _missingMdxReference(\"Y\", false);
    if (!Y.Z) _missingMdxReference(\"Y.Z\", true);
    if (!a) _missingMdxReference(\"a\", false);
    if (!a.b) _missingMdxReference(\"a.b\", true);
    if (!a.b.c) _missingMdxReference(\"a.b.c\", false);
    if (!a.b.c.d) _missingMdxReference(\"a.b.c.d\", true);
    return <_components.p ><X />{\", \"}<X.y />{\", \"}<Y.Z />{\", \"}<a.b.c.d />{\", \"}<a.b /></_components.p>;
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
            compile(
                "import * as X from './a.js'\n\n<X />",
                &Options::default()
            )?,
            "import * as X from './a.js';
function _createMdxContent(props) {
    return <X />;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
            "should not support passing in a component if one is defined locally (namespace import)",
        );

        assert_eq!(
            compile("# <a-b />, <qwe-rty />, <a-b />", &Options::default())?,
            "function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\",
        \"a-b\": \"a-b\",
        \"qwe-rty\": \"qwe-rty\"
    }, props.components), _component0 = _components[\"a-b\"], _component1 = _components[\"qwe-rty\"];
    return <_components.h1 ><_component0 />{\", \"}<_component1 />{\", \"}<_component0 /></_components.h1>;
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
                "export class A {}

<A />
",
            &Options::default()
        )?,
            "export class A {
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
            "should be aware of classes",
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
                "export const A = () => <B />",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export const A = ()=>{
    const { B  } = _provideComponents();
    if (!B) _missingMdxReference(\"B\", true);
    return <B />;
};
function _createMdxContent(props) {
    return <></>;
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
            "should support providing components in locally defined arrow functions",
        );

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
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function X(x) {
    const { G  } = _provideComponents();
    if (!G) _missingMdxReference(\"G\", true);
    let [A] = x;
    let [...B] = x;
    let { C  } = x;
    let { ...D } = x;
    let { _: E  } = x;
    let { F =_  } = x;
    return <><A /><B /><C /><D /><E /><F /><G /></>;
}
function _createMdxContent(props) {
    return <></>;
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
            "should support providing components in top-level components, aware of scopes",
        );

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

    (function () {
        let B = true;
    })()

    (() => {
        let B = true;
    })()

    return <B/>
}",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export function A() {
    const { B  } = _provideComponents();
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
    (function() {
        let B = true;
    })()(()=>{
        let B = true;
    })();
    return <B />;
}
function _createMdxContent(props) {
    return <></>;
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
            "should support providing components in top-level components, aware of scopes",
        );

        assert_eq!(
            compile(
                "export const A = function B() { return <C /> }",
                &Options {
                    provider_import_source: Some("x".into()),
                    ..Options::default()
                }
            )?,
            "import { useMDXComponents as _provideComponents } from \"x\";
export const A = function B() {
    const { C  } = _provideComponents();
    if (!C) _missingMdxReference(\"C\", true);
    return <C />;
};
function _createMdxContent(props) {
    return <></>;
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
            "should support providing components in locally defined function expressions",
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
