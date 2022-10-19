//! To do.

extern crate swc_common;
extern crate swc_ecma_ast;
use crate::hast_util_to_swc::Program;
use crate::mdx_plugin_recma_document::JsxRuntime;
use crate::swc_utils::{
    bytepos_to_point, create_ident, create_ident_expression, create_member_expression,
    is_identifier_name, prefix_error_with_point, span_to_position,
};
use core::str;
use markdown::Location;
use swc_common::{
    comments::{Comment, CommentKind},
    util::take::Take,
};
use swc_ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

/// Configuration.
#[derive(Debug, Default, Clone)]
pub struct Options {
    /// Whether to add extra information to error messages in generated code.
    pub development: bool,
}

pub fn swc_util_build_jsx(
    program: &mut Program,
    options: &Options,
    location: Option<&Location>,
) -> Result<(), String> {
    let directives = find_directives(&program.comments, location)?;

    let mut state = State {
        development: options.development,
        filepath: program.path.clone(),
        location,
        automatic: !matches!(directives.runtime, Some(JsxRuntime::Classic)),
        import_fragment: false,
        import_jsx: false,
        import_jsxs: false,
        import_jsx_dev: false,
        create_element_expression: create_member_expression(
            &directives
                .pragma
                .unwrap_or_else(|| "React.createElement".into()),
        ),
        fragment_expression: create_member_expression(
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
        specifiers.push(swc_ecma_ast::ImportSpecifier::Named(
            swc_ecma_ast::ImportNamedSpecifier {
                local: create_ident("_Fragment"),
                imported: Some(swc_ecma_ast::ModuleExportName::Ident(create_ident(
                    "Fragment",
                ))),
                span: swc_common::DUMMY_SP,
                is_type_only: false,
            },
        ));
    }

    if state.import_jsx {
        specifiers.push(swc_ecma_ast::ImportSpecifier::Named(
            swc_ecma_ast::ImportNamedSpecifier {
                local: create_ident("_jsx"),
                imported: Some(swc_ecma_ast::ModuleExportName::Ident(create_ident("jsx"))),
                span: swc_common::DUMMY_SP,
                is_type_only: false,
            },
        ));
    }

    if state.import_jsxs {
        specifiers.push(swc_ecma_ast::ImportSpecifier::Named(
            swc_ecma_ast::ImportNamedSpecifier {
                local: create_ident("_jsxs"),
                imported: Some(swc_ecma_ast::ModuleExportName::Ident(create_ident("jsxs"))),
                span: swc_common::DUMMY_SP,
                is_type_only: false,
            },
        ));
    }

    if state.import_jsx_dev {
        specifiers.push(swc_ecma_ast::ImportSpecifier::Named(
            swc_ecma_ast::ImportNamedSpecifier {
                local: create_ident("_jsxDEV"),
                imported: Some(swc_ecma_ast::ModuleExportName::Ident(create_ident(
                    "jsxDEV",
                ))),
                span: swc_common::DUMMY_SP,
                is_type_only: false,
            },
        ));
    }

    if !specifiers.is_empty() {
        program.module.body.insert(
            0,
            swc_ecma_ast::ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::Import(
                swc_ecma_ast::ImportDecl {
                    specifiers,
                    src: Box::new(swc_ecma_ast::Str {
                        value: format!(
                            "{}{}",
                            directives.import_source.unwrap_or_else(|| "react".into()),
                            if options.development {
                                "/jsx-dev-runtime"
                            } else {
                                "/jsx-runtime"
                            }
                        )
                        .into(),
                        span: swc_common::DUMMY_SP,
                        raw: None,
                    }),
                    type_only: false,
                    asserts: None,
                    span: swc_common::DUMMY_SP,
                },
            )),
        );
    }

    Ok(())
}

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
    /// Whether the user is in development mode.
    development: bool,
    automatic: bool,
    import_fragment: bool,
    import_jsx: bool,
    import_jsxs: bool,
    import_jsx_dev: bool,
    error: Option<String>,
    filepath: Option<String>,

    create_element_expression: swc_ecma_ast::Expr,
    fragment_expression: swc_ecma_ast::Expr,
}

impl<'a> State<'a> {
    /// Turn an attribute value into an expression.
    fn jsx_attribute_value_to_expression(
        &mut self,
        value: Option<swc_ecma_ast::JSXAttrValue>,
    ) -> Result<swc_ecma_ast::Expr, String> {
        match value {
            // Boolean prop.
            None => Ok(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Bool(
                swc_ecma_ast::Bool {
                    value: true,
                    span: swc_common::DUMMY_SP,
                },
            ))),
            Some(swc_ecma_ast::JSXAttrValue::JSXExprContainer(expression_container)) => {
                match expression_container.expr {
                    swc_ecma_ast::JSXExpr::JSXEmptyExpr(_) => {
                        unreachable!("Cannot use empty JSX expressions in attribute values");
                    }
                    swc_ecma_ast::JSXExpr::Expr(expression) => Ok(*expression),
                }
            }
            Some(swc_ecma_ast::JSXAttrValue::Lit(mut literal)) => {
                // Remove `raw` so we don’t get character references in strings.
                if let swc_ecma_ast::Lit::Str(string_literal) = &mut literal {
                    string_literal.raw = None;
                }

                Ok(swc_ecma_ast::Expr::Lit(literal))
            }
            Some(swc_ecma_ast::JSXAttrValue::JSXFragment(fragment)) => {
                self.jsx_fragment_to_expression(fragment)
            }
            Some(swc_ecma_ast::JSXAttrValue::JSXElement(element)) => {
                self.jsx_element_to_expression(*element)
            }
        }
    }

    /// Turn children of elements or fragments into expressions.
    fn jsx_children_to_expressions(
        &mut self,
        mut children: Vec<swc_ecma_ast::JSXElementChild>,
    ) -> Result<Vec<swc_ecma_ast::Expr>, String> {
        let mut result = vec![];
        children.reverse();
        while let Some(child) = children.pop() {
            match child {
                swc_ecma_ast::JSXElementChild::JSXExprContainer(x) => match x.expr {
                    swc_ecma_ast::JSXExpr::JSXEmptyExpr(_) => {}
                    swc_ecma_ast::JSXExpr::Expr(expression) => result.push(*expression),
                },
                swc_ecma_ast::JSXElementChild::JSXText(text) => {
                    let value = jsx_text_to_value(text.value.as_ref());
                    if !value.is_empty() {
                        result.push(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(
                            swc_ecma_ast::Str {
                                value: value.into(),
                                span: text.span,
                                raw: None,
                            },
                        )));
                    }
                }
                swc_ecma_ast::JSXElementChild::JSXSpreadChild(_) => {
                    panic!("Spread children not supported in Babel, SWC, or React");
                }
                swc_ecma_ast::JSXElementChild::JSXElement(element) => {
                    result.push(self.jsx_element_to_expression(*element)?);
                }
                swc_ecma_ast::JSXElementChild::JSXFragment(fragment) => {
                    result.push(self.jsx_fragment_to_expression(fragment)?);
                }
            }
        }

        Ok(result)
    }

    /// Turn optional attributes, and perhaps children (when automatic), into props.
    fn jsx_attributes_to_expressions(
        &mut self,
        attributes: Option<Vec<swc_ecma_ast::JSXAttrOrSpread>>,
        children: Option<Vec<swc_ecma_ast::Expr>>,
    ) -> Result<(Option<swc_ecma_ast::Expr>, Option<swc_ecma_ast::Expr>), String> {
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
                    swc_ecma_ast::JSXAttrOrSpread::SpreadElement(spread_element) => {
                        if !fields.is_empty() {
                            objects.push(swc_ecma_ast::Expr::Object(swc_ecma_ast::ObjectLit {
                                props: fields,
                                span: swc_common::DUMMY_SP,
                            }));
                            fields = vec![];
                        }

                        objects.push(*spread_element.expr);
                        spread = true;
                    }
                    swc_ecma_ast::JSXAttrOrSpread::JSXAttr(jsx_attribute) => {
                        let name = jsx_attribute_name_to_prop_name(jsx_attribute.name);
                        let value = self.jsx_attribute_value_to_expression(jsx_attribute.value)?;

                        if let swc_ecma_ast::PropName::Ident(ident) = &name {
                            if self.automatic && ident.sym.as_ref() == "key" {
                                if spread {
                                    let start =
                                        bytepos_to_point(jsx_attribute.span.lo, self.location);
                                    return Err(prefix_error_with_point(
                                        "Expected `key` to come before any spread expressions"
                                            .into(),
                                        start.as_ref(),
                                    ));
                                }

                                key = Some(value);
                                continue;
                            }
                        }

                        fields.push(swc_ecma_ast::PropOrSpread::Prop(Box::new(
                            swc_ecma_ast::Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                                key: name,
                                value: Box::new(value),
                            }),
                        )));
                    }
                }
            }
        }

        // In the automatic runtime, add children as a prop.
        if let Some(mut children) = children {
            let value = match children.len() {
                0 => None,
                1 => Some(children.pop().unwrap()),
                _ => {
                    let mut elements = vec![];
                    children.reverse();
                    while let Some(child) = children.pop() {
                        elements.push(Some(swc_ecma_ast::ExprOrSpread {
                            spread: None,
                            expr: Box::new(child),
                        }));
                    }
                    Some(swc_ecma_ast::Expr::Array(swc_ecma_ast::ArrayLit {
                        elems: elements,
                        span: swc_common::DUMMY_SP,
                    }))
                }
            };

            if let Some(value) = value {
                fields.push(swc_ecma_ast::PropOrSpread::Prop(Box::new(
                    swc_ecma_ast::Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                        key: swc_ecma_ast::PropName::Ident(create_ident("children")),
                        value: Box::new(value),
                    }),
                )));
            }
        }

        // Add remaining fields.
        if !fields.is_empty() {
            objects.push(swc_ecma_ast::Expr::Object(swc_ecma_ast::ObjectLit {
                props: fields,
                span: swc_common::DUMMY_SP,
            }));
        }

        let props = match objects.len() {
            0 => None,
            1 => Some(objects.pop().unwrap()),
            _ => {
                let mut args = vec![];
                objects.reverse();

                // Don’t mutate the first object, shallow clone instead.
                if !matches!(objects.last(), Some(swc_ecma_ast::Expr::Object(_))) {
                    objects.push(swc_ecma_ast::Expr::Object(swc_ecma_ast::ObjectLit {
                        props: vec![],
                        span: swc_common::DUMMY_SP,
                    }));
                }

                while let Some(object) = objects.pop() {
                    args.push(swc_ecma_ast::ExprOrSpread {
                        spread: None,
                        expr: Box::new(object),
                    });
                }

                Some(swc_ecma_ast::Expr::Call(swc_ecma_ast::CallExpr {
                    callee: swc_ecma_ast::Callee::Expr(Box::new(create_member_expression(
                        "Object.assign",
                    ))),
                    args,
                    span: swc_common::DUMMY_SP,
                    type_args: None,
                }))
            }
        };

        Ok((props, key))
    }

    fn jsx_expressions_to_call(
        &mut self,
        span: &swc_common::Span,
        name: swc_ecma_ast::Expr,
        attributes: Option<Vec<swc_ecma_ast::JSXAttrOrSpread>>,
        mut children: Vec<swc_ecma_ast::Expr>,
    ) -> Result<swc_ecma_ast::Expr, String> {
        let (callee, parameters) = if self.automatic {
            let is_static_children = children.len() > 1;
            let (props, key) = self.jsx_attributes_to_expressions(attributes, Some(children))?;
            let mut parameters = vec![
                // Component name.
                //
                // ```javascript
                // Component
                // ```
                swc_ecma_ast::ExprOrSpread {
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
                swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(props.unwrap_or(swc_ecma_ast::Expr::Object(
                        swc_ecma_ast::ObjectLit {
                            props: vec![],
                            span: swc_common::DUMMY_SP,
                        },
                    ))),
                },
            ];

            // Key or, in development, undefined.
            //
            // ```javascript
            // "xyz"
            // ```
            if let Some(key) = key {
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(key),
                });
            } else if self.development {
                parameters.push(swc_ecma_ast::ExprOrSpread {
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
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Bool(
                        swc_ecma_ast::Bool {
                            value: is_static_children,
                            span: swc_common::DUMMY_SP,
                        },
                    ))),
                });

                let mut meta_fields = vec![swc_ecma_ast::PropOrSpread::Prop(Box::new(
                    swc_ecma_ast::Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                        key: swc_ecma_ast::PropName::Ident(create_ident("fileName")),
                        value: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(
                            swc_ecma_ast::Str {
                                value: self
                                    .filepath
                                    .as_ref()
                                    .map_or_else(|| "<source.js>".into(), Clone::clone)
                                    .into(),
                                raw: None,
                                span: swc_common::DUMMY_SP,
                            },
                        ))),
                    }),
                ))];

                if let Some(position) = span_to_position(span, self.location) {
                    meta_fields.push(swc_ecma_ast::PropOrSpread::Prop(Box::new(
                        swc_ecma_ast::Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                            key: swc_ecma_ast::PropName::Ident(create_ident("lineNumber")),
                            value: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Num(
                                swc_ecma_ast::Number {
                                    value: position.start.line as f64,
                                    raw: None,
                                    span: swc_common::DUMMY_SP,
                                },
                            ))),
                        }),
                    )));

                    meta_fields.push(swc_ecma_ast::PropOrSpread::Prop(Box::new(
                        swc_ecma_ast::Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                            key: swc_ecma_ast::PropName::Ident(create_ident("columnNumber")),
                            value: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Num(
                                swc_ecma_ast::Number {
                                    value: position.start.column as f64,
                                    raw: None,
                                    span: swc_common::DUMMY_SP,
                                },
                            ))),
                        }),
                    )));
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
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(swc_ecma_ast::Expr::Object(swc_ecma_ast::ObjectLit {
                        props: meta_fields,
                        span: swc_common::DUMMY_SP,
                    })),
                });

                // File name and positional info.
                //
                // ```javascript
                // {
                //   fileName: "example.jsx",
                //   lineNumber: 1,
                //   columnNumber: 3
                // }
                // ```
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(swc_ecma_ast::Expr::This(swc_ecma_ast::ThisExpr {
                        span: swc_common::DUMMY_SP,
                    })),
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
        }
        // Classic runtime.
        else {
            // Key is only extracted in the automatic runtime.
            let (props, _) = self.jsx_attributes_to_expressions(attributes, None)?;
            let mut parameters = vec![
                // Component name.
                //
                // ```javascript
                // Component
                // ```
                swc_ecma_ast::ExprOrSpread {
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
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(props),
                });
            } else if !children.is_empty() {
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Null(
                        swc_ecma_ast::Null {
                            span: swc_common::DUMMY_SP,
                        },
                    ))),
                });
            }

            // Each child as a parameter.
            children.reverse();
            while let Some(child) = children.pop() {
                parameters.push(swc_ecma_ast::ExprOrSpread {
                    spread: None,
                    expr: Box::new(child),
                });
            }

            (self.create_element_expression.clone(), parameters)
        };

        Ok(swc_ecma_ast::Expr::Call(swc_ecma_ast::CallExpr {
            callee: swc_ecma_ast::Callee::Expr(Box::new(callee)),
            args: parameters,
            type_args: None,
            span: swc_common::DUMMY_SP,
        }))
    }

    /// Turn a JSX element into an expression.
    fn jsx_element_to_expression(
        &mut self,
        element: swc_ecma_ast::JSXElement,
    ) -> Result<swc_ecma_ast::Expr, String> {
        let children = self.jsx_children_to_expressions(element.children)?;
        let mut name = jsx_element_name_to_identifier(element.opening.name);

        // If the name could be an identifier, but start with a lowercase letter,
        // it’s not a component.
        if let swc_ecma_ast::Expr::Ident(ident) = &name {
            let head = ident.as_ref().as_bytes();
            if matches!(head.first(), Some(b'a'..=b'z')) {
                name = swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                    value: ident.sym.clone(),
                    raw: None,
                    span: ident.span,
                }));
            }
        }

        self.jsx_expressions_to_call(&element.span, name, Some(element.opening.attrs), children)
    }

    /// Turn a JSX fragment into an expression.
    fn jsx_fragment_to_expression(
        &mut self,
        fragment: swc_ecma_ast::JSXFragment,
    ) -> Result<swc_ecma_ast::Expr, String> {
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

    fn visit_mut_expr(&mut self, expr: &mut swc_ecma_ast::Expr) {
        let result = match expr {
            swc_ecma_ast::Expr::JSXElement(element) => {
                Some(self.jsx_element_to_expression(*element.take()))
            }
            swc_ecma_ast::Expr::JSXFragment(fragment) => {
                Some(self.jsx_fragment_to_expression(fragment.take()))
            }
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

    // Exit: Program
    //      Add import

    // Exit JSX element/fragment
    //      - compile children
    //      - compile name
    //          - attributes
    //      - `jsxDEV` w/ file path, line, column

    // Replace

    // Refs: https://github.com/swc-project/swc/blob/main/crates/swc_ecma_transforms_react/src/jsx/mod.rs
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
) -> Result<Directives, String> {
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
                break;
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
                                format!(
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

fn jsx_element_name_to_identifier(node: swc_ecma_ast::JSXElementName) -> swc_ecma_ast::Expr {
    match node {
        swc_ecma_ast::JSXElementName::JSXMemberExpr(member_expr) => {
            jsx_member_expression_to_expression(member_expr)
        }
        swc_ecma_ast::JSXElementName::JSXNamespacedName(namespace_name) => {
            swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                value: format!("{}:{}", namespace_name.ns.sym, namespace_name.name.sym).into(),
                raw: None,
                span: swc_common::DUMMY_SP,
            }))
        }
        swc_ecma_ast::JSXElementName::Ident(ident) => create_ident_or_literal(&ident),
    }
}

fn jsx_member_expression_to_expression(node: swc_ecma_ast::JSXMemberExpr) -> swc_ecma_ast::Expr {
    swc_ecma_ast::Expr::Member(swc_ecma_ast::MemberExpr {
        obj: Box::new(jsx_object_to_expression(node.obj)),
        prop: jsx_ident_to_member_prop(&node.prop),
        span: swc_common::DUMMY_SP,
    })
}

fn jsx_ident_to_member_prop(node: &swc_ecma_ast::Ident) -> swc_ecma_ast::MemberProp {
    if is_identifier_name(node.as_ref()) {
        swc_ecma_ast::MemberProp::Ident(swc_ecma_ast::Ident {
            sym: node.sym.clone(),
            optional: false,
            span: node.span,
        })
    } else {
        swc_ecma_ast::MemberProp::Computed(swc_ecma_ast::ComputedPropName {
            expr: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(
                swc_ecma_ast::Str {
                    value: node.sym.clone(),
                    span: node.span,
                    raw: None,
                },
            ))),
            span: node.span,
        })
    }
}

fn jsx_attribute_name_to_prop_name(node: swc_ecma_ast::JSXAttrName) -> swc_ecma_ast::PropName {
    match node {
        swc_ecma_ast::JSXAttrName::JSXNamespacedName(namespace_name) => {
            swc_ecma_ast::PropName::Str(swc_ecma_ast::Str {
                value: format!("{}:{}", namespace_name.ns.sym, namespace_name.name.sym).into(),
                span: swc_common::DUMMY_SP,
                raw: None,
            })
        }
        swc_ecma_ast::JSXAttrName::Ident(ident) => create_prop_name(&ident),
    }
}

fn jsx_object_to_expression(node: swc_ecma_ast::JSXObject) -> swc_ecma_ast::Expr {
    match node {
        swc_ecma_ast::JSXObject::Ident(ident) => create_ident_or_literal(&ident),
        swc_ecma_ast::JSXObject::JSXMemberExpr(member_expr) => {
            jsx_member_expression_to_expression(*member_expr)
        }
    }
}

fn create_ident_or_literal(node: &swc_ecma_ast::Ident) -> swc_ecma_ast::Expr {
    if is_identifier_name(node.as_ref()) {
        create_ident_expression(node.sym.as_ref())
    } else {
        swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
            value: node.sym.clone(),
            span: node.span,
            raw: None,
        }))
    }
}

fn create_prop_name(node: &swc_ecma_ast::Ident) -> swc_ecma_ast::PropName {
    if is_identifier_name(node.as_ref()) {
        swc_ecma_ast::PropName::Ident(create_ident(node.sym.as_ref()))
    } else {
        swc_ecma_ast::PropName::Str(swc_ecma_ast::Str {
            value: node.sym.clone(),
            span: node.span,
            raw: None,
        })
    }
}

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
    use swc_common::comments::SingleThreadedComments;
    use swc_common::{source_map::Pos, BytePos, FileName, SourceFile};
    use swc_ecma_ast::EsVersion;
    use swc_ecma_parser::{parse_file_as_module, EsConfig, Syntax};

    fn compile(value: &str, options: &Options) -> Result<String, String> {
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
            Err(error) => Err(error.kind().msg().into()),
            Ok(module) => {
                let mut program = Program {
                    path: Some("example.jsx".into()),
                    module,
                    comments: flat_comments(comments),
                };
                swc_util_build_jsx(&mut program, options, Some(&location))?;
                Ok(serialize(&program.module, Some(&program.comments)))
            }
        }
    }

    #[test]
    fn small_default() -> Result<(), String> {
        assert_eq!(
            compile("let a = <b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\nlet a = _jsx(\"b\", {});\n",
            "should compile JSX away"
        );

        Ok(())
    }

    #[test]
    fn directive_runtime_automatic() -> Result<(), String> {
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
    fn directive_runtime_classic() -> Result<(), String> {
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
    fn directive_runtime_empty() -> Result<(), String> {
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
            .unwrap(),
            "1:1: Runtime must be either `automatic` or `classic`, not unknown",
            "should crash on a non-automatic, non-classic `@jsxRuntime` directive"
        );
    }

    #[test]
    fn directive_import_source() -> Result<(), String> {
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
    fn directive_jsx() -> Result<(), String> {
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
    fn directive_jsx_empty() -> Result<(), String> {
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
    fn directive_jsx_non_identifier() -> Result<(), String> {
        assert_eq!(
            compile(
                "/* @jsxRuntime classic @jsx a.b-c */\n<x />",
                &Options::default()
            )?,
            "a[\"b-c\"](\"x\");\n",
            "should support an `@jsx` directive set to an invalid identifier"
        );

        Ok(())
    }

    #[test]
    fn directive_jsx_frag() -> Result<(), String> {
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
    fn directive_jsx_frag_empty() -> Result<(), String> {
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
    fn jsx_element_self_closing() -> Result<(), String> {
        assert_eq!(
            compile("<a />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {});\n",
            "should support a self-closing element"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_closed() -> Result<(), String> {
        assert_eq!(
            compile(
                "<a>b</a>",
                &Options::default()
            )?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    children: \"b\"\n});\n",
            "should support a closed element"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_member_name() -> Result<(), String> {
        assert_eq!(
            compile("<a.b.c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(a.b.c, {});\n",
            "should support an element with a member name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_member_name_dashes() -> Result<(), String> {
        assert_eq!(
            compile("<a.b-c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(a[\"b-c\"], {});\n",
            "should support an element with a member name and dashes"
        );

        Ok(())
    }
    #[test]
    fn jsx_element_member_name_many() -> Result<(), String> {
        assert_eq!(
            compile("<a.b.c.d />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(a.b.c.d, {});\n",
            "should support an element with a member name of lots of names"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_namespace_name() -> Result<(), String> {
        assert_eq!(
            compile("<a:b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a:b\", {});\n",
            "should support an element with a namespace name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_name_dashes() -> Result<(), String> {
        assert_eq!(
            compile("<a-b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a-b\", {});\n",
            "should support an element with a dash in the name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_name_capital() -> Result<(), String> {
        assert_eq!(
            compile("<Abc />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(Abc, {});\n",
            "should support an element with a non-lowercase first character in the name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_boolean() -> Result<(), String> {
        assert_eq!(
            compile("<a b />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: true\n});\n",
            "should support an element with a boolean attribute"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_name_namespace() -> Result<(), String> {
        assert_eq!(
            compile("<a b:c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    \"b:c\": true\n});\n",
            "should support an element with colons in an attribute name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_name_non_identifier() -> Result<(), String> {
        assert_eq!(
            compile("<a b-c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    \"b-c\": true\n});\n",
            "should support an element with non-identifier characters in an attribute name"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value() -> Result<(), String> {
        assert_eq!(
            compile("<a b='c' />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: \"c\"\n});\n",
            "should support an element with an attribute with a value"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value_expression() -> Result<(), String> {
        assert_eq!(
            compile("<a b={c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: c\n});\n",
            "should support an element with an attribute with a value expression"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value_fragment() -> Result<(), String> {
        assert_eq!(
            compile("<a b=<>c</> />", &Options::default())?,
            "import { Fragment as _Fragment, jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: _jsx(_Fragment, {\n        children: \"c\"\n    })\n});\n",
            "should support an element with an attribute with a fragment as value"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_attribute_value_element() -> Result<(), String> {
        assert_eq!(
            compile("<a b=<c /> />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", {\n    b: _jsx(\"c\", {})\n});\n",
            "should support an element with an attribute with an element as value"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_spread_attribute() -> Result<(), String> {
        assert_eq!(
            compile("<a {...b} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", b);\n",
            "should support an element with a spread attribute"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_spread_attribute_then_prop() -> Result<(), String> {
        assert_eq!(
            compile("<a {...b} c />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", Object.assign({}, b, {\n    c: true\n}));\n",
            "should support an element with a spread attribute and then a prop"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_prop_then_spread_attribute() -> Result<(), String> {
        assert_eq!(
            compile("<a b {...c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", Object.assign({\n    b: true\n}, c));\n",
            "should support an element with a prop and then a spread attribute"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_two_spread_attributes() -> Result<(), String> {
        assert_eq!(
            compile("<a {...b} {...c} />", &Options::default())?,
            "import { jsx as _jsx } from \"react/jsx-runtime\";\n_jsx(\"a\", Object.assign({}, b, c));\n",
            "should support an element two spread attributes"
        );

        Ok(())
    }

    #[test]
    fn jsx_element_complex_spread_attribute() -> Result<(), String> {
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
    fn jsx_element_child_expression() -> Result<(), String> {
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
    fn jsx_element_child_expression_empty() -> Result<(), String> {
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
    fn jsx_element_child_text_padded_start() -> Result<(), String> {
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
    fn jsx_element_child_text_padded_end() -> Result<(), String> {
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
    fn jsx_element_child_text_padded() -> Result<(), String> {
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
    fn jsx_element_child_text_line_endings_padded() -> Result<(), String> {
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
    fn jsx_element_child_text_blank_lines() -> Result<(), String> {
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
    fn jsx_element_child_whitespace_only() -> Result<(), String> {
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
    fn jsx_element_key_automatic() -> Result<(), String> {
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
    fn jsx_element_key_after_spread_automatic() {
        assert_eq!(
            compile("<a {...b} key='c' />", &Options::default())
                .err()
                .unwrap(),
            "1:11: Expected `key` to come before any spread expressions",
            "should crash on a key after a spread in the automatic runtime"
        );
    }

    #[test]
    fn jsx_element_key_before_spread_automatic() -> Result<(), String> {
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
    fn jsx_element_development() -> Result<(), String> {
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
