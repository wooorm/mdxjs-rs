//! Turn an HTML AST into a JavaScript AST.
//!
//! Port of <https://github.com/syntax-tree/hast-util-to-estree>, by the same
//! author:
//!
//! (The MIT License)
//!
//! Copyright (c) 2016 Titus Wormer <tituswormer@gmail.com>
//!
//! Permission is hereby granted, free of charge, to any person obtaining
//! a copy of this software and associated documentation files (the
//! 'Software'), to deal in the Software without restriction, including
//! without limitation the rights to use, copy, modify, merge, publish,
//! distribute, sublicense, and/or sell copies of the Software, and to
//! permit persons to whom the Software is furnished to do so, subject to
//! the following conditions:
//!
//! The above copyright notice and this permission notice shall be
//! included in all copies or substantial portions of the Software.
//!
//! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
//! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//! IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
//! CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
//! TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
//! SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

extern crate swc_common;
extern crate swc_ecma_ast;
use crate::hast;
use crate::swc::{parse_esm_to_tree, parse_expression_to_tree};
use crate::swc_utils::{create_ident, parse_jsx_name, position_to_span, JsxName};
use core::str;
use markdown::{Location, MdxExpressionKind};

/// Result.
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    /// File path.
    pub path: Option<String>,
    /// JS AST.
    pub module: swc_ecma_ast::Module,
    /// Comments relating to AST.
    pub comments: Vec<swc_common::comments::Comment>,
}

/// Whether we’re in HTML or SVG.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Space {
    /// The HTML space.
    Html,
    /// The SVG space.
    Svg,
}

/// Context used to compile hast into SWC’s ES AST.
#[derive(Debug)]
struct Context<'a> {
    /// Whether we’re in HTML or SVG.
    ///
    /// Not used yet, likely useful in the future.
    space: Space,
    /// Comments we gather.
    comments: Vec<swc_common::comments::Comment>,
    /// Declarations and stuff.
    esm: Vec<swc_ecma_ast::ModuleItem>,
    /// Optional way to turn relative positions into points.
    location: Option<&'a Location>,
}

/// Compile hast into SWC’s ES AST.
pub fn hast_util_to_swc(
    tree: &hast::Node,
    path: Option<String>,
    location: Option<&Location>,
) -> Result<Program, String> {
    let mut context = Context {
        space: Space::Html,
        comments: vec![],
        esm: vec![],
        location,
    };
    let expr = match one(&mut context, tree)? {
        Some(swc_ecma_ast::JSXElementChild::JSXFragment(x)) => {
            Some(swc_ecma_ast::Expr::JSXFragment(x))
        }
        Some(swc_ecma_ast::JSXElementChild::JSXElement(x)) => {
            Some(swc_ecma_ast::Expr::JSXElement(x))
        }
        Some(child) => Some(swc_ecma_ast::Expr::JSXFragment(create_fragment(
            vec![child],
            tree,
        ))),
        None => None,
    };

    // Add the ESM.
    let mut module = swc_ecma_ast::Module {
        shebang: None,
        body: context.esm,
        span: position_to_span(tree.position()),
    };

    // We have some content, wrap it.
    if let Some(expr) = expr {
        module
            .body
            .push(swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                swc_ecma_ast::ExprStmt {
                    expr: Box::new(expr),
                    span: swc_common::DUMMY_SP,
                },
            )));
    }

    Ok(Program {
        path,
        module,
        comments: context.comments,
    })
}

/// Transform one node.
fn one(
    context: &mut Context,
    node: &hast::Node,
) -> Result<Option<swc_ecma_ast::JSXElementChild>, String> {
    let value = match node {
        hast::Node::Comment(x) => Some(transform_comment(context, node, x)),
        hast::Node::Element(x) => transform_element(context, node, x)?,
        hast::Node::MdxJsxElement(x) => transform_mdx_jsx_element(context, node, x)?,
        hast::Node::MdxExpression(x) => transform_mdx_expression(context, node, x)?,
        hast::Node::MdxjsEsm(x) => transform_mdxjs_esm(context, node, x)?,
        hast::Node::Root(x) => transform_root(context, node, x)?,
        hast::Node::Text(x) => transform_text(context, node, x),
        // Ignore:
        hast::Node::Doctype(_) => None,
    };
    Ok(value)
}

/// Transform children of `parent`.
fn all(
    context: &mut Context,
    parent: &hast::Node,
) -> Result<Vec<swc_ecma_ast::JSXElementChild>, String> {
    let mut result = vec![];
    if let Some(children) = parent.children() {
        let mut index = 0;
        while index < children.len() {
            let child = &children[index];
            // To do: remove line endings between table elements?
            // <https://github.com/syntax-tree/hast-util-to-estree/blob/6c45f166d106ea3a165c14ec50c35ed190055e65/lib/index.js>
            if let Some(child) = one(context, child)? {
                result.push(child);
            }
            index += 1;
        }
    }

    Ok(result)
}

/// [`Comment`][hast::Comment].
fn transform_comment(
    context: &mut Context,
    node: &hast::Node,
    comment: &hast::Comment,
) -> swc_ecma_ast::JSXElementChild {
    context.comments.push(swc_common::comments::Comment {
        kind: swc_common::comments::CommentKind::Block,
        text: comment.value.clone().into(),
        span: position_to_span(node.position()),
    });

    // Might be useless.
    // Might be useful when transforming to acorn/babel later.
    // This is done in the JS version too:
    // <https://github.com/syntax-tree/hast-util-to-estree/blob/6c45f166d106ea3a165c14ec50c35ed190055e65/lib/index.js#L168>
    swc_ecma_ast::JSXElementChild::JSXExprContainer(swc_ecma_ast::JSXExprContainer {
        expr: swc_ecma_ast::JSXExpr::JSXEmptyExpr(swc_ecma_ast::JSXEmptyExpr {
            span: position_to_span(node.position()),
        }),
        span: position_to_span(node.position()),
    })
}

/// [`Element`][hast::Element].
fn transform_element(
    context: &mut Context,
    node: &hast::Node,
    element: &hast::Element,
) -> Result<Option<swc_ecma_ast::JSXElementChild>, String> {
    let space = context.space;

    if space == Space::Html && element.tag_name == "svg" {
        context.space = Space::Svg;
    }

    let children = all(context, node)?;

    context.space = space;

    let mut attrs = vec![];

    let mut index = 0;
    while index < element.properties.len() {
        let prop = &element.properties[index];

        // To do: turn style props into objects.
        let value = match &prop.1 {
            hast::PropertyValue::Boolean(x) => {
                // No value is same as `{true}` / Ignore `false`.
                if *x {
                    None
                } else {
                    index += 1;
                    continue;
                }
            }
            hast::PropertyValue::String(x) => Some(swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                value: x.clone().into(),
                span: swc_common::DUMMY_SP,
                raw: None,
            })),
            hast::PropertyValue::CommaSeparated(x) => {
                Some(swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                    value: x.join(", ").into(),
                    span: swc_common::DUMMY_SP,
                    raw: None,
                }))
            }
            hast::PropertyValue::SpaceSeparated(x) => {
                Some(swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                    value: x.join(" ").into(),
                    span: swc_common::DUMMY_SP,
                    raw: None,
                }))
            }
        };

        // Turn property case into either React-specific case, or HTML
        // attribute case.
        // To do: create a spread if this is an invalid attr name.
        let attr_name = prop_to_attr_name(&prop.0);

        attrs.push(swc_ecma_ast::JSXAttrOrSpread::JSXAttr(
            swc_ecma_ast::JSXAttr {
                name: create_jsx_attr_name(&attr_name),
                value: value.map(swc_ecma_ast::JSXAttrValue::Lit),
                span: swc_common::DUMMY_SP,
            },
        ));

        index += 1;
    }

    Ok(Some(swc_ecma_ast::JSXElementChild::JSXElement(
        create_element(&element.tag_name, attrs, children, node),
    )))
}

/// [`MdxJsxElement`][hast::MdxJsxElement].
fn transform_mdx_jsx_element(
    context: &mut Context,
    node: &hast::Node,
    element: &hast::MdxJsxElement,
) -> Result<Option<swc_ecma_ast::JSXElementChild>, String> {
    let space = context.space;

    if let Some(name) = &element.name {
        if space == Space::Html && name == "svg" {
            context.space = Space::Svg;
        }
    }

    let children = all(context, node)?;

    context.space = space;

    let mut attrs = vec![];
    let mut index = 0;

    while index < element.attributes.len() {
        let attr = match &element.attributes[index] {
            hast::AttributeContent::Property(prop) => {
                let value = match prop.value.as_ref() {
                    Some(hast::AttributeValue::Literal(x)) => {
                        Some(swc_ecma_ast::JSXAttrValue::Lit(swc_ecma_ast::Lit::Str(
                            swc_ecma_ast::Str {
                                value: x.clone().into(),
                                span: swc_common::DUMMY_SP,
                                raw: None,
                            },
                        )))
                    }
                    Some(hast::AttributeValue::Expression(value, stops)) => {
                        Some(swc_ecma_ast::JSXAttrValue::JSXExprContainer(
                            swc_ecma_ast::JSXExprContainer {
                                expr: swc_ecma_ast::JSXExpr::Expr(parse_expression_to_tree(
                                    value,
                                    &MdxExpressionKind::AttributeValueExpression,
                                    stops,
                                    context.location,
                                )?),
                                span: swc_common::DUMMY_SP,
                            },
                        ))
                    }
                    None => None,
                };

                swc_ecma_ast::JSXAttrOrSpread::JSXAttr(swc_ecma_ast::JSXAttr {
                    span: swc_common::DUMMY_SP,
                    name: create_jsx_attr_name(&prop.name),
                    value,
                })
            }
            hast::AttributeContent::Expression(value, stops) => {
                let expr = parse_expression_to_tree(
                    value,
                    &MdxExpressionKind::AttributeExpression,
                    stops,
                    context.location,
                )?;
                swc_ecma_ast::JSXAttrOrSpread::SpreadElement(swc_ecma_ast::SpreadElement {
                    dot3_token: swc_common::DUMMY_SP,
                    expr,
                })
            }
        };

        attrs.push(attr);
        index += 1;
    }

    Ok(Some(if let Some(name) = &element.name {
        swc_ecma_ast::JSXElementChild::JSXElement(create_element(name, attrs, children, node))
    } else {
        swc_ecma_ast::JSXElementChild::JSXFragment(create_fragment(children, node))
    }))
}

/// [`MdxExpression`][hast::MdxExpression].
fn transform_mdx_expression(
    context: &mut Context,
    node: &hast::Node,
    expression: &hast::MdxExpression,
) -> Result<Option<swc_ecma_ast::JSXElementChild>, String> {
    Ok(Some(swc_ecma_ast::JSXElementChild::JSXExprContainer(
        swc_ecma_ast::JSXExprContainer {
            expr: swc_ecma_ast::JSXExpr::Expr(parse_expression_to_tree(
                &expression.value,
                &MdxExpressionKind::Expression,
                &expression.stops,
                context.location,
            )?),
            span: position_to_span(node.position()),
        },
    )))
}

/// [`MdxjsEsm`][hast::MdxjsEsm].
fn transform_mdxjs_esm(
    context: &mut Context,
    _node: &hast::Node,
    esm: &hast::MdxjsEsm,
) -> Result<Option<swc_ecma_ast::JSXElementChild>, String> {
    let mut module = parse_esm_to_tree(&esm.value, &esm.stops, context.location)?;
    let mut index = 0;

    // To do: check that identifiers are not duplicated across esm blocks.
    while index < module.body.len() {
        if !matches!(module.body[index], swc_ecma_ast::ModuleItem::ModuleDecl(_)) {
            return Err("Unexpected `statement` in code: only import/exports are supported".into());
        }
        index += 1;
    }

    context.esm.append(&mut module.body);
    Ok(None)
}

/// [`Root`][hast::Root].
fn transform_root(
    context: &mut Context,
    node: &hast::Node,
    _root: &hast::Root,
) -> Result<Option<swc_ecma_ast::JSXElementChild>, String> {
    let mut children = all(context, node)?;
    let mut queue = vec![];
    let mut nodes = vec![];
    let mut seen = false;

    children.reverse();

    // Remove initial/final whitespace.
    while let Some(child) = children.pop() {
        let mut stash = false;

        if let swc_ecma_ast::JSXElementChild::JSXExprContainer(container) = &child {
            if let swc_ecma_ast::JSXExpr::Expr(expr) = &container.expr {
                if let swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(str)) = (*expr).as_ref() {
                    if inter_element_whitespace(str.value.as_ref()) {
                        stash = true;
                    }
                }
            }
        }

        if stash {
            if seen {
                queue.push(child);
            }
        } else {
            if !queue.is_empty() {
                nodes.append(&mut queue);
            }
            nodes.push(child);
            seen = true;
        }
    }

    Ok(Some(swc_ecma_ast::JSXElementChild::JSXFragment(
        create_fragment(nodes, node),
    )))
}

/// [`Text`][hast::Text].
fn transform_text(
    _context: &mut Context,
    node: &hast::Node,
    text: &hast::Text,
) -> Option<swc_ecma_ast::JSXElementChild> {
    if text.value.is_empty() {
        None
    } else {
        Some(swc_ecma_ast::JSXElementChild::JSXExprContainer(
            swc_ecma_ast::JSXExprContainer {
                expr: swc_ecma_ast::JSXExpr::Expr(Box::new(swc_ecma_ast::Expr::Lit(
                    swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                        value: text.value.clone().into(),
                        span: position_to_span(node.position()),
                        raw: None,
                    }),
                ))),
                span: position_to_span(node.position()),
            },
        ))
    }
}

/// Create an element.
///
/// Creates a void one if there are no children.
fn create_element(
    name: &str,
    attrs: Vec<swc_ecma_ast::JSXAttrOrSpread>,
    children: Vec<swc_ecma_ast::JSXElementChild>,
    node: &hast::Node,
) -> Box<swc_ecma_ast::JSXElement> {
    Box::new(swc_ecma_ast::JSXElement {
        opening: swc_ecma_ast::JSXOpeningElement {
            name: create_jsx_name(name),
            attrs,
            self_closing: children.is_empty(),
            type_args: None,
            span: swc_common::DUMMY_SP,
        },
        closing: if children.is_empty() {
            None
        } else {
            Some(swc_ecma_ast::JSXClosingElement {
                name: create_jsx_name(name),
                span: swc_common::DUMMY_SP,
            })
        },
        children,
        span: position_to_span(node.position()),
    })
}

/// Create a fragment.
fn create_fragment(
    children: Vec<swc_ecma_ast::JSXElementChild>,
    node: &hast::Node,
) -> swc_ecma_ast::JSXFragment {
    swc_ecma_ast::JSXFragment {
        opening: swc_ecma_ast::JSXOpeningFragment {
            span: swc_common::DUMMY_SP,
        },
        closing: swc_ecma_ast::JSXClosingFragment {
            span: swc_common::DUMMY_SP,
        },
        children,
        span: position_to_span(node.position()),
    }
}

/// Create a JSX element name.
fn create_jsx_name(name: &str) -> swc_ecma_ast::JSXElementName {
    match parse_jsx_name(name) {
        // `<a.b.c />`
        // `<a.b />`
        JsxName::Member(parts) => {
            // Always two or more items.
            let mut member = swc_ecma_ast::JSXMemberExpr {
                obj: swc_ecma_ast::JSXObject::Ident(create_ident(parts[0])),
                prop: create_ident(parts[1]),
            };
            let mut index = 2;
            while index < parts.len() {
                member = swc_ecma_ast::JSXMemberExpr {
                    obj: swc_ecma_ast::JSXObject::JSXMemberExpr(Box::new(member)),
                    prop: create_ident(parts[index]),
                };
                index += 1;
            }
            swc_ecma_ast::JSXElementName::JSXMemberExpr(member)
        }
        // `<a:b />`
        JsxName::Namespace(ns, name) => {
            swc_ecma_ast::JSXElementName::JSXNamespacedName(swc_ecma_ast::JSXNamespacedName {
                ns: create_ident(ns),
                name: create_ident(name),
            })
        }
        // `<a />`
        JsxName::Normal(name) => swc_ecma_ast::JSXElementName::Ident(create_ident(name)),
    }
}

/// Create a JSX attribute name.
fn create_jsx_attr_name(name: &str) -> swc_ecma_ast::JSXAttrName {
    match parse_jsx_name(name) {
        JsxName::Member(_) => {
            unreachable!("member expressions in attribute names are not supported")
        }
        // `<a b:c />`
        JsxName::Namespace(ns, name) => {
            swc_ecma_ast::JSXAttrName::JSXNamespacedName(swc_ecma_ast::JSXNamespacedName {
                ns: create_ident(ns),
                name: create_ident(name),
            })
        }
        // `<a b />`
        JsxName::Normal(name) => swc_ecma_ast::JSXAttrName::Ident(create_ident(name)),
    }
}

/// Check if a text value is inter-element whitespace.
///
/// See: <https://github.com/syntax-tree/hast-util-whitespace>.
fn inter_element_whitespace(value: &str) -> bool {
    let bytes = value.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        match bytes[index] {
            b'\t' | 0x0C | b'\r' | b'\n' | b' ' => {}
            _ => return false,
        }
        index += 1;
    }

    true
}

/// Turn a hast property into something that particularly React understands.
fn prop_to_attr_name(prop: &str) -> String {
    // Arbitrary data props, kebab case them.
    if prop.len() > 4 && prop.starts_with("data") {
        // Assume like two dashes maybe?
        let mut result = String::with_capacity(prop.len() + 2);
        let bytes = prop.as_bytes();
        let mut index = 4;
        let mut start = index;
        let mut valid = true;

        result.push_str("data");

        while index < bytes.len() {
            let byte = bytes[index];
            let mut dash = index == 4;

            match byte {
                b'A'..=b'Z' => dash = true,
                b'-' | b'.' | b':' | b'0'..=b'9' | b'a'..=b'z' => {}
                _ => {
                    valid = false;
                    break;
                }
            }

            if dash {
                if start != index {
                    result.push_str(&prop[start..index]);
                }
                result.push('-');
                result.push(byte.to_ascii_lowercase().into());
                start = index + 1;
            }

            index += 1;
        }

        if valid {
            result.push_str(&prop[start..]);
            return result;
        }
    }

    // Look up if prop differs from attribute case.
    // Unknown things are passed through.
    PROP_TO_REACT_PROP
        .iter()
        .find(|d| d.0 == prop)
        .or_else(|| PROP_TO_ATTR_EXCEPTIONS_SHARED.iter().find(|d| d.0 == prop))
        .map_or_else(|| prop.into(), |d| d.1.into())
}

// Below data is generated with:
//
// Note: there are currently no HTML and SVG specific exceptions.
// If those would start appearing, the logic that uses these lists needs
// To support spaces.
//
// ```js
// import * as x from "property-information";
//
// /** @type {Record<string, string>} */
// let shared = {};
// /** @type {Record<string, string>} */
// let html = {};
// /** @type {Record<string, string>} */
// let svg = {};
//
// Object.keys(x.html.property).forEach((prop) => {
//   let attr = x.html.property[prop].attribute;
//   if (!x.html.property[prop].space && prop !== attr) {
//     html[prop] = attr;
//   }
// });
//
// Object.keys(x.svg.property).forEach((prop) => {
//   let attr = x.svg.property[prop].attribute;
//   if (!x.svg.property[prop].space && prop !== attr) {
//     // Shared.
//     if (prop in html && html[prop] === attr) {
//       shared[prop] = attr;
//       delete html[prop];
//     } else {
//       svg[prop] = attr;
//     }
//   }
// });
//
// /** @type {Array<[string, Array<[string, string]>]>} */
// const all = [
//   ["PROP_TO_REACT_PROP", Object.entries(x.hastToReact)],
//   ["PROP_TO_ATTR_EXCEPTIONS", Object.entries(shared)],
//   ["PROP_TO_ATTR_EXCEPTIONS_HTML", Object.entries(html)],
//   ["PROP_TO_ATTR_EXCEPTIONS_SVG", Object.entries(svg)],
// ];
//
// console.log(
//   all
//     .map((d) => {
//       return `const ${d[0]}: [(&str, &str); ${d[1].length}] = [
// ${d[1].map((d) => `    ("${d[0]}", "${d[1]}")`).join(",\n")}
// ];`;
//     })
//     .join("\n\n")
// );
// ```
/// hast property names to React property names, if they differ.
const PROP_TO_REACT_PROP: [(&str, &str); 17] = [
    ("classId", "classID"),
    ("dataType", "datatype"),
    ("itemId", "itemID"),
    ("strokeDashArray", "strokeDasharray"),
    ("strokeDashOffset", "strokeDashoffset"),
    ("strokeLineCap", "strokeLinecap"),
    ("strokeLineJoin", "strokeLinejoin"),
    ("strokeMiterLimit", "strokeMiterlimit"),
    ("typeOf", "typeof"),
    ("xLinkActuate", "xlinkActuate"),
    ("xLinkArcRole", "xlinkArcrole"),
    ("xLinkHref", "xlinkHref"),
    ("xLinkRole", "xlinkRole"),
    ("xLinkShow", "xlinkShow"),
    ("xLinkTitle", "xlinkTitle"),
    ("xLinkType", "xlinkType"),
    ("xmlnsXLink", "xmlnsXlink"),
];

/// hast property names to HTML attribute names, if they differ.
const PROP_TO_ATTR_EXCEPTIONS_SHARED: [(&str, &str); 48] = [
    ("ariaActiveDescendant", "aria-activedescendant"),
    ("ariaAtomic", "aria-atomic"),
    ("ariaAutoComplete", "aria-autocomplete"),
    ("ariaBusy", "aria-busy"),
    ("ariaChecked", "aria-checked"),
    ("ariaColCount", "aria-colcount"),
    ("ariaColIndex", "aria-colindex"),
    ("ariaColSpan", "aria-colspan"),
    ("ariaControls", "aria-controls"),
    ("ariaCurrent", "aria-current"),
    ("ariaDescribedBy", "aria-describedby"),
    ("ariaDetails", "aria-details"),
    ("ariaDisabled", "aria-disabled"),
    ("ariaDropEffect", "aria-dropeffect"),
    ("ariaErrorMessage", "aria-errormessage"),
    ("ariaExpanded", "aria-expanded"),
    ("ariaFlowTo", "aria-flowto"),
    ("ariaGrabbed", "aria-grabbed"),
    ("ariaHasPopup", "aria-haspopup"),
    ("ariaHidden", "aria-hidden"),
    ("ariaInvalid", "aria-invalid"),
    ("ariaKeyShortcuts", "aria-keyshortcuts"),
    ("ariaLabel", "aria-label"),
    ("ariaLabelledBy", "aria-labelledby"),
    ("ariaLevel", "aria-level"),
    ("ariaLive", "aria-live"),
    ("ariaModal", "aria-modal"),
    ("ariaMultiLine", "aria-multiline"),
    ("ariaMultiSelectable", "aria-multiselectable"),
    ("ariaOrientation", "aria-orientation"),
    ("ariaOwns", "aria-owns"),
    ("ariaPlaceholder", "aria-placeholder"),
    ("ariaPosInSet", "aria-posinset"),
    ("ariaPressed", "aria-pressed"),
    ("ariaReadOnly", "aria-readonly"),
    ("ariaRelevant", "aria-relevant"),
    ("ariaRequired", "aria-required"),
    ("ariaRoleDescription", "aria-roledescription"),
    ("ariaRowCount", "aria-rowcount"),
    ("ariaRowIndex", "aria-rowindex"),
    ("ariaRowSpan", "aria-rowspan"),
    ("ariaSelected", "aria-selected"),
    ("ariaSetSize", "aria-setsize"),
    ("ariaSort", "aria-sort"),
    ("ariaValueMax", "aria-valuemax"),
    ("ariaValueMin", "aria-valuemin"),
    ("ariaValueNow", "aria-valuenow"),
    ("ariaValueText", "aria-valuetext"),
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hast;
    use crate::hast_util_to_swc::{hast_util_to_swc, Program};
    use crate::swc::serialize;
    use pretty_assertions::assert_eq;

    #[test]
    fn comments() -> Result<(), String> {
        let comment_ast = hast_util_to_swc(
            &hast::Node::Comment(hast::Comment {
                value: "a".into(),
                position: None,
            }),
            None,
            None,
        )?;

        assert_eq!(
            comment_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                        swc_ecma_ast::ExprStmt {
                            expr: Box::new(swc_ecma_ast::Expr::JSXFragment(
                                swc_ecma_ast::JSXFragment {
                                    opening: swc_ecma_ast::JSXOpeningFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    closing: swc_ecma_ast::JSXClosingFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    children: vec![
                                        swc_ecma_ast::JSXElementChild::JSXExprContainer(
                                            swc_ecma_ast::JSXExprContainer {
                                                expr: swc_ecma_ast::JSXExpr::JSXEmptyExpr(
                                                    swc_ecma_ast::JSXEmptyExpr {
                                                        span: swc_common::DUMMY_SP,
                                                    }
                                                ),
                                                span: swc_common::DUMMY_SP,
                                            },
                                        )
                                    ],
                                    span: swc_common::DUMMY_SP,
                                }
                            )),
                            span: swc_common::DUMMY_SP,
                        },
                    ))],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![swc_common::comments::Comment {
                    kind: swc_common::comments::CommentKind::Block,
                    text: "a".into(),
                    span: swc_common::DUMMY_SP,
                }],
            },
            "should support a `Comment`",
        );

        assert_eq!(
            serialize(&comment_ast.module, Some(&comment_ast.comments)),
            // To do: comment should be in this.
            "<>{}</>;\n",
            "should support a `Comment` (serialize)",
        );

        Ok(())
    }

    #[test]
    fn elements() -> Result<(), String> {
        let element_ast = hast_util_to_swc(
            &hast::Node::Element(hast::Element {
                tag_name: "a".into(),
                properties: vec![(
                    "className".into(),
                    hast::PropertyValue::SpaceSeparated(vec!["b".into()]),
                )],
                children: vec![],
                position: None,
            }),
            None,
            None,
        )?;

        assert_eq!(
            element_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                        swc_ecma_ast::ExprStmt {
                            expr: Box::new(swc_ecma_ast::Expr::JSXElement(Box::new(
                                swc_ecma_ast::JSXElement {
                                    opening: swc_ecma_ast::JSXOpeningElement {
                                        name: swc_ecma_ast::JSXElementName::Ident(
                                            swc_ecma_ast::Ident {
                                                span: swc_common::DUMMY_SP,
                                                sym: "a".into(),
                                                optional: false,
                                            }
                                        ),
                                        attrs: vec![swc_ecma_ast::JSXAttrOrSpread::JSXAttr(
                                            swc_ecma_ast::JSXAttr {
                                                name: swc_ecma_ast::JSXAttrName::Ident(
                                                    swc_ecma_ast::Ident {
                                                        sym: "className".into(),
                                                        span: swc_common::DUMMY_SP,
                                                        optional: false,
                                                    }
                                                ),
                                                value: Some(swc_ecma_ast::JSXAttrValue::Lit(
                                                    swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                                                        value: "b".into(),
                                                        span: swc_common::DUMMY_SP,
                                                        raw: None,
                                                    })
                                                )),
                                                span: swc_common::DUMMY_SP,
                                            },
                                        )],
                                        self_closing: true,
                                        type_args: None,
                                        span: swc_common::DUMMY_SP,
                                    },
                                    closing: None,
                                    children: vec![],
                                    span: swc_common::DUMMY_SP,
                                }
                            ))),
                            span: swc_common::DUMMY_SP,
                        },
                    ))],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![],
            },
            "should support an `Element`",
        );

        assert_eq!(
            serialize(&element_ast.module, Some(&element_ast.comments)),
            "<a className=\"b\"/>;\n",
            "should support an `Element` (serialize)",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![],
                        children: vec![hast::Node::Text(hast::Text {
                            value: "a".into(),
                            position: None,
                        })],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a >{\"a\"}</a>;\n",
            "should support an `Element` w/ children",
        );

        Ok(())
    }

    #[test]
    fn element_attributes() -> Result<(), String> {
        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![("b".into(), hast::PropertyValue::String("c".into()),)],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a b=\"c\"/>;\n",
            "should support an `Element` w/ a string attribute",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![("b".into(), hast::PropertyValue::Boolean(true),)],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a b/>;\n",
            "should support an `Element` w/ a boolean (true) attribute",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![("b".into(), hast::PropertyValue::Boolean(false),)],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a />;\n",
            "should support an `Element` w/ a boolean (false) attribute",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![(
                            "b".into(),
                            hast::PropertyValue::CommaSeparated(vec!["c".into(), "d".into()]),
                        )],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a b=\"c, d\"/>;\n",
            "should support an `Element` w/ a comma-separated attribute",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![
                            ("data123".into(), hast::PropertyValue::Boolean(true),),
                            ("dataFoo".into(), hast::PropertyValue::Boolean(true),),
                            ("dataBAR".into(), hast::PropertyValue::Boolean(true),)
                        ],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a data-123 data-foo data-b-a-r/>;\n",
            "should support an `Element` w/ data attributes",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::Element(hast::Element {
                        tag_name: "a".into(),
                        properties: vec![
                            ("role".into(), hast::PropertyValue::Boolean(true),),
                            ("ariaValueNow".into(), hast::PropertyValue::Boolean(true),),
                            ("ariaDescribedBy".into(), hast::PropertyValue::Boolean(true),)
                        ],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a role aria-valuenow aria-describedby/>;\n",
            "should support an `Element` w/ aria attributes",
        );

        Ok(())
    }

    #[test]
    fn mdx_element() -> Result<(), String> {
        let mdx_element_ast = hast_util_to_swc(
            &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                name: None,
                attributes: vec![],
                children: vec![],
                position: None,
            }),
            None,
            None,
        )?;

        assert_eq!(
            mdx_element_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                        swc_ecma_ast::ExprStmt {
                            expr: Box::new(swc_ecma_ast::Expr::JSXFragment(
                                swc_ecma_ast::JSXFragment {
                                    opening: swc_ecma_ast::JSXOpeningFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    closing: swc_ecma_ast::JSXClosingFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    children: vec![],
                                    span: swc_common::DUMMY_SP,
                                }
                            )),
                            span: swc_common::DUMMY_SP,
                        },
                    ))],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![],
            },
            "should support an `MdxElement` (fragment)",
        );

        assert_eq!(
            serialize(&mdx_element_ast.module, Some(&mdx_element_ast.comments)),
            "<></>;\n",
            "should support an `MdxElement` (fragment, serialize)",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a".into()),
                        attributes: vec![],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a />;\n",
            "should support an `MdxElement` (element, no children)",
        );
        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a".into()),
                        attributes: vec![],
                        children: vec![hast::Node::Text(hast::Text {
                            value: "b".into(),
                            position: None,
                        })],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a >{\"b\"}</a>;\n",
            "should support an `MdxElement` (element, children)",
        );

        Ok(())
    }

    #[test]
    fn mdx_element_name() -> Result<(), String> {
        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a:b".into()),
                        attributes: vec![],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a:b />;\n",
            "should support an `MdxElement` (element, namespace id)",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a.b.c".into()),
                        attributes: vec![],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a.b.c />;\n",
            "should support an `MdxElement` (element, member expression)",
        );

        Ok(())
    }

    #[test]
    fn mdx_element_attributes() -> Result<(), String> {
        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a".into()),
                        attributes: vec![hast::AttributeContent::Property(hast::MdxJsxAttribute {
                            name: "b".into(),
                            value: None
                        })],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a b/>;\n",
            "should support an `MdxElement` (element, boolean attribute)",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a".into()),
                        attributes: vec![hast::AttributeContent::Property(hast::MdxJsxAttribute {
                            name: "b".into(),
                            value: Some(hast::AttributeValue::Literal("c".into()))
                        })],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a b=\"c\"/>;\n",
            "should support an `MdxElement` (element, attribute w/ literal value)",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a".into()),
                        attributes: vec![hast::AttributeContent::Property(hast::MdxJsxAttribute {
                            name: "b".into(),
                            value: Some(hast::AttributeValue::Expression("c".into(), vec![]))
                        })],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a b={c}/>;\n",
            "should support an `MdxElement` (element, attribute w/ expression value)",
        );

        assert_eq!(
            serialize(
                &hast_util_to_swc(
                    &hast::Node::MdxJsxElement(hast::MdxJsxElement {
                        name: Some("a".into()),
                        attributes: vec![hast::AttributeContent::Expression("...c".into(), vec![])],
                        children: vec![],
                        position: None,
                    }),
                    None,
                    None
                )?
                .module,
                None
            ),
            "<a {...c}/>;\n",
            "should support an `MdxElement` (element, expression attribute)",
        );

        Ok(())
    }

    #[test]
    fn mdx_expression() -> Result<(), String> {
        let mdx_expression_ast = hast_util_to_swc(
            &hast::Node::MdxExpression(hast::MdxExpression {
                value: "a".into(),
                position: None,
                stops: vec![],
            }),
            None,
            None,
        )?;

        assert_eq!(
            mdx_expression_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                        swc_ecma_ast::ExprStmt {
                            expr: Box::new(swc_ecma_ast::Expr::JSXFragment(
                                swc_ecma_ast::JSXFragment {
                                    opening: swc_ecma_ast::JSXOpeningFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    closing: swc_ecma_ast::JSXClosingFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    children: vec![
                                        swc_ecma_ast::JSXElementChild::JSXExprContainer(
                                            swc_ecma_ast::JSXExprContainer {
                                                expr: swc_ecma_ast::JSXExpr::Expr(Box::new(
                                                    swc_ecma_ast::Expr::Ident(
                                                        swc_ecma_ast::Ident {
                                                            sym: "a".into(),
                                                            span: swc_common::DUMMY_SP,
                                                            optional: false,
                                                        }
                                                    )
                                                )),
                                                span: swc_common::DUMMY_SP,
                                            },
                                        )
                                    ],
                                    span: swc_common::DUMMY_SP,
                                }
                            )),
                            span: swc_common::DUMMY_SP,
                        },
                    ))],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![],
            },
            "should support an `MdxExpression`",
        );

        assert_eq!(
            serialize(
                &mdx_expression_ast.module,
                Some(&mdx_expression_ast.comments)
            ),
            "<>{a}</>;\n",
            "should support an `MdxExpression` (serialize)",
        );

        Ok(())
    }

    #[test]
    fn mdx_esm() -> Result<(), String> {
        let mdxjs_esm_ast = hast_util_to_swc(
            &hast::Node::MdxjsEsm(hast::MdxjsEsm {
                value: "import a from 'b'".into(),
                position: None,
                stops: vec![],
            }),
            None,
            None,
        )?;

        assert_eq!(
            mdxjs_esm_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::ModuleDecl(
                        swc_ecma_ast::ModuleDecl::Import(swc_ecma_ast::ImportDecl {
                            specifiers: vec![swc_ecma_ast::ImportSpecifier::Default(
                                swc_ecma_ast::ImportDefaultSpecifier {
                                    local: swc_ecma_ast::Ident {
                                        sym: "a".into(),
                                        optional: false,
                                        span: swc_common::DUMMY_SP,
                                    },
                                    span: swc_common::DUMMY_SP,
                                }
                            )],
                            src: Box::new(swc_ecma_ast::Str {
                                value: "b".into(),
                                span: swc_common::DUMMY_SP,
                                raw: Some("\'b\'".into()),
                            }),
                            type_only: false,
                            asserts: None,
                            span: swc_common::DUMMY_SP,
                        })
                    )],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![],
            },
            "should support an `MdxjsEsm`",
        );

        assert_eq!(
            serialize(&mdxjs_esm_ast.module, Some(&mdxjs_esm_ast.comments)),
            "import a from 'b';\n",
            "should support an `MdxjsEsm` (serialize)",
        );

        Ok(())
    }

    #[test]
    fn root() -> Result<(), String> {
        let root_ast = hast_util_to_swc(
            &hast::Node::Root(hast::Root {
                children: vec![hast::Node::Text(hast::Text {
                    value: "a".into(),
                    position: None,
                })],
                position: None,
            }),
            None,
            None,
        )?;

        assert_eq!(
            root_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                        swc_ecma_ast::ExprStmt {
                            expr: Box::new(swc_ecma_ast::Expr::JSXFragment(
                                swc_ecma_ast::JSXFragment {
                                    opening: swc_ecma_ast::JSXOpeningFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    closing: swc_ecma_ast::JSXClosingFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    children: vec![
                                        swc_ecma_ast::JSXElementChild::JSXExprContainer(
                                            swc_ecma_ast::JSXExprContainer {
                                                expr: swc_ecma_ast::JSXExpr::Expr(Box::new(
                                                    swc_ecma_ast::Expr::Lit(
                                                        swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                                                            value: "a".into(),
                                                            span: swc_common::DUMMY_SP,
                                                            raw: None,
                                                        }),
                                                    )
                                                )),
                                                span: swc_common::DUMMY_SP,
                                            },
                                        )
                                    ],
                                    span: swc_common::DUMMY_SP,
                                }
                            )),
                            span: swc_common::DUMMY_SP,
                        },
                    ))],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![],
            },
            "should support a `Root`",
        );

        assert_eq!(
            serialize(&root_ast.module, Some(&root_ast.comments)),
            "<>{\"a\"}</>;\n",
            "should support a `Root` (serialize)",
        );

        Ok(())
    }

    #[test]
    fn text() -> Result<(), String> {
        let text_ast = hast_util_to_swc(
            &hast::Node::Text(hast::Text {
                value: "a".into(),
                position: None,
            }),
            None,
            None,
        )?;

        assert_eq!(
            text_ast,
            Program {
                path: None,
                module: swc_ecma_ast::Module {
                    shebang: None,
                    body: vec![swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(
                        swc_ecma_ast::ExprStmt {
                            expr: Box::new(swc_ecma_ast::Expr::JSXFragment(
                                swc_ecma_ast::JSXFragment {
                                    opening: swc_ecma_ast::JSXOpeningFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    closing: swc_ecma_ast::JSXClosingFragment {
                                        span: swc_common::DUMMY_SP,
                                    },
                                    children: vec![
                                        swc_ecma_ast::JSXElementChild::JSXExprContainer(
                                            swc_ecma_ast::JSXExprContainer {
                                                expr: swc_ecma_ast::JSXExpr::Expr(Box::new(
                                                    swc_ecma_ast::Expr::Lit(
                                                        swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                                                            value: "a".into(),
                                                            span: swc_common::DUMMY_SP,
                                                            raw: None,
                                                        }),
                                                    )
                                                )),
                                                span: swc_common::DUMMY_SP,
                                            },
                                        )
                                    ],
                                    span: swc_common::DUMMY_SP,
                                }
                            )),
                            span: swc_common::DUMMY_SP,
                        },
                    ))],
                    span: swc_common::DUMMY_SP,
                },
                comments: vec![],
            },
            "should support a `Text`",
        );

        assert_eq!(
            serialize(&text_ast.module, Some(&text_ast.comments)),
            "<>{\"a\"}</>;\n",
            "should support a `Text` (serialize)",
        );

        Ok(())
    }
}
