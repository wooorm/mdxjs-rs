extern crate mdxjs;

use markdown::mdast;
use mdxjs::hast;
use mdxjs::{HastNode, MdastNode, Options, PluginOptions, RecmaProgram};
use std::rc::Rc;
use swc_core::common::{Span, SyntaxContext};
use swc_core::ecma::ast as estree;
use swc_core::ecma::atoms::JsWord;

/// Example that compiles the example MDX document from <https://mdxjs.com>
/// to JavaScript.
fn main() -> Result<(), String> {
    println!(
        "{}",
        mdxjs::compile_with_plugins(
            "# test",
            &Options {
                ..Default::default()
            },
            &PluginOptions {
                experimental_mdast_transforms: Some(vec![Rc::new(|root: &mut MdastNode| {
                    mdast_visit_mut(root, |n| {
                        if let mdast::Node::Text(text) = n {
                            text.value = "Hello World!".into();
                        }
                    });
                    Ok(())
                })]),
                experimental_hast_transforms: Some(vec![Rc::new(|root: &mut HastNode| {
                    hast_visit_mut(root, |n| {
                        if let hast::Node::Element(e) = n {
                            if e.tag_name == "h1" {
                                e.tag_name = "h2".into();
                            }
                        };
                    });
                    Ok(())
                })]),
                experimental_recma_transforms: Some(vec![Rc::new(|program: &mut RecmaProgram| {
                    let body = &mut program.module.body;
                    body.push(estree::ModuleItem::Stmt(estree::Stmt::Expr(
                        estree::ExprStmt {
                            expr: Box::new(estree::Expr::Ident(estree::Ident::from((
                                JsWord::from("hello"),
                                SyntaxContext::empty(),
                            )))),
                            span: Span::default(),
                        },
                    )));
                    Ok(())
                })])
            }
        )?
    );

    Ok(())
}

fn mdast_visit_mut<Visitor>(node: &mut mdast::Node, visitor: Visitor)
where
    Visitor: FnMut(&mut mdast::Node),
{
    mdast_visit_mut_impl(node, visitor);
}

fn mdast_visit_mut_impl<Visitor>(node: &mut mdast::Node, mut visitor: Visitor) -> Visitor
where
    Visitor: FnMut(&mut mdast::Node),
{
    visitor(node);

    if let Some(children) = node.children_mut() {
        let mut index = 0;
        while index < children.len() {
            let child = &mut children[index];
            visitor = mdast_visit_mut_impl(child, visitor);
            index += 1;
        }
    }

    visitor
}

fn hast_visit_mut<Visitor>(node: &mut hast::Node, visitor: Visitor)
where
    Visitor: FnMut(&mut hast::Node),
{
    hast_visit_mut_impl(node, visitor);
}

fn hast_visit_mut_impl<Visitor>(node: &mut hast::Node, mut visitor: Visitor) -> Visitor
where
    Visitor: FnMut(&mut hast::Node),
{
    visitor(node);

    if let Some(children) = node.children_mut() {
        let mut index = 0;
        while index < children.len() {
            let child = &mut children[index];
            visitor = hast_visit_mut_impl(child, visitor);
            index += 1;
        }
    }

    visitor
}
