//! Public API of `mdxjs-rs`.
//!
//! This module exposes primarily [`compile()`][].
//!
//! *   [`compile()`][]
//!     — turn MDX into JavaScript
#![deny(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::struct_excessive_bools)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_precision_loss)]

extern crate markdown;
mod configuration;
pub mod hast;
mod hast_util_to_swc;
mod mdast_util_to_hast;
mod mdx_plugin_recma_document;
mod mdx_plugin_recma_jsx_rewrite;
mod swc;
mod swc_util_build_jsx;
mod swc_utils;

use crate::{
    hast_util_to_swc::hast_util_to_swc as to_swc,
    mdx_plugin_recma_document::{
        mdx_plugin_recma_document as recma_document, Options as DocumentOptions,
    },
    mdx_plugin_recma_jsx_rewrite::{
        mdx_plugin_recma_jsx_rewrite as recma_jsx_rewrite, Options as RewriteOptions,
    },
    swc::{parse_esm, parse_expression, serialize},
    swc_util_build_jsx::{swc_util_build_jsx, Options as BuildOptions},
};
use hast_util_to_swc::Program;
use markdown::{
    message::{self, Message},
    to_mdast, Constructs, Location, ParseOptions,
};
use swc_core::{alloc::collections::FxHashSet, common::Span};

pub use crate::configuration::{MdxConstructs, MdxParseOptions, Options};
pub use crate::mdast_util_to_hast::mdast_util_to_hast;
pub use crate::mdx_plugin_recma_document::JsxRuntime;

/// Turn MDX into JavaScript.
///
/// ## Examples
///
/// ```
/// use mdxjs::compile;
/// # fn main() -> Result<(), markdown::message::Message> {
///
/// assert_eq!(compile("# Hi!", &Default::default())?, "import { jsx as _jsx } from \"react/jsx-runtime\";\nfunction _createMdxContent(props) {\n    const _components = Object.assign({\n        h1: \"h1\"\n    }, props.components);\n    return _jsx(_components.h1, {\n        children: \"Hi!\"\n    });\n}\nfunction MDXContent(props = {}) {\n    const { wrapper: MDXLayout } = props.components || {};\n    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {\n        children: _jsx(_createMdxContent, props)\n    })) : _createMdxContent(props);\n}\nexport default MDXContent;\n");
/// # Ok(())
/// # }
/// ```
///
/// ## Errors
///
/// This project errors for many different reasons, such as syntax errors in
/// the MDX format or misconfiguration.
pub fn compile(value: &str, options: &Options) -> Result<String, message::Message> {
    let mdast = mdast_util_from_mdx(value, options)?;
    let hast = mdast_util_to_hast(&mdast);
    let location = Location::new(value.as_bytes());
    let mut explicit_jsxs = FxHashSet::default();
    let mut program = hast_util_to_swc(&hast, options, Some(&location), &mut explicit_jsxs)?;
    mdx_plugin_recma_document(&mut program, options, Some(&location))?;
    mdx_plugin_recma_jsx_rewrite(&mut program, options, Some(&location), &explicit_jsxs)?;
    Ok(serialize(&mut program.module, Some(&program.comments)))
}

/// Turn MDX into a syntax tree.
///
/// ## Errors
///
/// There are several errors that can occur with how
/// JSX, expressions, or ESM are written.
///
/// ## Examples
///
/// ```
/// use mdxjs::{mdast_util_from_mdx, Options};
/// # fn main() -> Result<(), markdown::message::Message> {
///
/// let tree = mdast_util_from_mdx("# Hey, *you*!", &Options::default())?;
///
/// println!("{:?}", tree);
/// // => Root { children: [Heading { children: [Text { value: "Hey, ", position: Some(1:3-1:8 (2-7)) }, Emphasis { children: [Text { value: "you", position: Some(1:9-1:12 (8-11)) }], position: Some(1:8-1:13 (7-12)) }, Text { value: "!", position: Some(1:13-1:14 (12-13)) }], position: Some(1:1-1:14 (0-13)), depth: 1 }], position: Some(1:1-1:14 (0-13)) }
/// # Ok(())
/// # }
/// ```
pub fn mdast_util_from_mdx(
    value: &str,
    options: &Options,
) -> Result<markdown::mdast::Node, Message> {
    let parse_options = ParseOptions {
        constructs: Constructs {
            attention: options.parse.constructs.attention,
            autolink: false,
            block_quote: options.parse.constructs.block_quote,
            character_escape: options.parse.constructs.character_escape,
            character_reference: options.parse.constructs.character_reference,
            code_fenced: options.parse.constructs.code_fenced,
            code_indented: false,
            code_text: options.parse.constructs.code_text,
            definition: options.parse.constructs.definition,
            frontmatter: options.parse.constructs.frontmatter,
            gfm_autolink_literal: options.parse.constructs.gfm_autolink_literal,
            gfm_footnote_definition: options.parse.constructs.gfm_footnote_definition,
            gfm_label_start_footnote: options.parse.constructs.gfm_label_start_footnote,
            gfm_strikethrough: options.parse.constructs.gfm_strikethrough,
            gfm_table: options.parse.constructs.gfm_table,
            gfm_task_list_item: options.parse.constructs.gfm_task_list_item,
            hard_break_escape: options.parse.constructs.hard_break_escape,
            hard_break_trailing: options.parse.constructs.hard_break_trailing,
            html_flow: false,
            html_text: false,
            heading_atx: options.parse.constructs.heading_atx,
            heading_setext: options.parse.constructs.heading_setext,
            label_start_image: options.parse.constructs.label_start_image,
            label_start_link: options.parse.constructs.label_start_link,
            label_end: options.parse.constructs.label_end,
            list_item: options.parse.constructs.list_item,
            math_flow: options.parse.constructs.math_flow,
            math_text: options.parse.constructs.math_text,
            mdx_esm: true,
            mdx_expression_flow: true,
            mdx_expression_text: true,
            mdx_jsx_flow: true,
            mdx_jsx_text: true,
            thematic_break: options.parse.constructs.thematic_break,
        },
        gfm_strikethrough_single_tilde: options.parse.gfm_strikethrough_single_tilde,
        math_text_single_dollar: options.parse.math_text_single_dollar,
        mdx_esm_parse: Some(Box::new(parse_esm)),
        mdx_expression_parse: Some(Box::new(parse_expression)),
    };

    to_mdast(value, &parse_options)
}

/// Compile hast into SWC’s ES AST.
///
/// ## Errors
///
/// This function currently does not emit errors.
pub fn hast_util_to_swc(
    hast: &hast::Node,
    options: &Options,
    location: Option<&Location>,
    explicit_jsxs: &mut FxHashSet<Span>,
) -> Result<Program, markdown::message::Message> {
    to_swc(hast, options.filepath.clone(), location, explicit_jsxs)
}

/// Wrap the SWC ES AST nodes coming from hast into a whole document.
///
/// ## Errors
///
/// This functions errors for double layouts (default exports).
pub fn mdx_plugin_recma_document(
    program: &mut Program,
    options: &Options,
    location: Option<&Location>,
) -> Result<(), markdown::message::Message> {
    let document_options = DocumentOptions {
        pragma: options.pragma.clone(),
        pragma_frag: options.pragma_frag.clone(),
        pragma_import_source: options.pragma_import_source.clone(),
        jsx_import_source: options.jsx_import_source.clone(),
        jsx_runtime: options.jsx_runtime,
    };
    recma_document(program, &document_options, location)
}

/// Rewrite JSX in an MDX file so that components can be passed in and provided.
/// Also compiles JSX to function calls unless `options.jsx` is true.
///
/// ## Errors
///
/// This functions errors for incorrect JSX runtime configuration *inside*
/// MDX files and problems with SWC (broken JS syntax).
pub fn mdx_plugin_recma_jsx_rewrite(
    program: &mut Program,
    options: &Options,
    location: Option<&Location>,
    explicit_jsxs: &FxHashSet<Span>,
) -> Result<(), markdown::message::Message> {
    let rewrite_options = RewriteOptions {
        development: options.development,
        provider_import_source: options.provider_import_source.clone(),
    };

    recma_jsx_rewrite(program, &rewrite_options, location, explicit_jsxs);

    if !options.jsx {
        let build_options = BuildOptions {
            development: options.development,
        };

        swc_util_build_jsx(program, &build_options, location)?;
    }

    Ok(())
}
