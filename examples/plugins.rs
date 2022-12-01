extern crate mdxjs;

use markdown::mdast;
use mdxjs::{HastNode, MdastNode, Options, PluginOptions, RecmaProgram};
use std::rc::Rc;

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
                experimental_mdast_transforms: Some(vec![Rc::new(|root: &MdastNode| {
                    let mut root1 = root.clone();
                    visit_mut(&mut root1, |n| {
                        match n {
                            mdast::Node::Text(text) => text.value = "Hello World!".into(),
                            _ => {}
                        };
                    });
                    root1
                })]),
                experimental_hast_transforms: Some(vec![Rc::new(|root: &HastNode| {
                    root.clone()
                })]),
                experimental_recma_transforms: Some(vec![Rc::new(|program: &RecmaProgram| {
                    program.clone()
                })]),
                ..Default::default()
            }
        )?
    );

    Ok(())
}

/// Visit.
fn visit<Visitor>(node: &mdast::Node, visitor: Visitor)
where
    Visitor: FnMut(&mdast::Node),
{
    visit_impl(node, visitor);
}

/// Internal implementation to visit.
fn visit_impl<Visitor>(node: &mdast::Node, mut visitor: Visitor) -> Visitor
where
    Visitor: FnMut(&mdast::Node),
{
    visitor(node);

    if let Some(children) = node.children() {
        let mut index = 0;
        while index < children.len() {
            let child = &children[index];
            visitor = visit_impl(child, visitor);
            index += 1;
        }
    }

    visitor
}

/// Visit.
fn visit_mut<Visitor>(node: &mut mdast::Node, visitor: Visitor)
where
    Visitor: FnMut(&mut mdast::Node),
{
    visit_mut_impl(node, visitor);
}

/// Internal implementation to visit.
fn visit_mut_impl<Visitor>(node: &mut mdast::Node, mut visitor: Visitor) -> Visitor
where
    Visitor: FnMut(&mut mdast::Node),
{
    visitor(node);

    if let Some(children) = node.children_mut() {
        let mut index = 0;
        while index < children.len() {
            let child = &mut children[index];
            visitor = visit_mut_impl(child, visitor);
            index += 1;
        }
    }

    visitor
}
