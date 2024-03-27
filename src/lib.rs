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
mod error;
mod hast;
mod hast_util_to_swc;
mod mdast_util_to_hast;
mod mdx_plugin_recma_document;
mod mdx_plugin_recma_jsx_rewrite;
mod swc;
mod swc_util_build_jsx;
mod swc_utils;

use crate::{
    hast_util_to_swc::hast_util_to_swc,
    mdast_util_to_hast::mdast_util_to_hast,
    mdx_plugin_recma_document::{mdx_plugin_recma_document, Options as DocumentOptions},
    mdx_plugin_recma_jsx_rewrite::{mdx_plugin_recma_jsx_rewrite, Options as RewriteOptions},
    swc::{parse_esm, parse_expression, serialize},
    swc_util_build_jsx::{swc_util_build_jsx, Options as BuildOptions},
};
use error::capture;
use markdown::{to_mdast, Constructs, Location, ParseOptions};

pub use crate::configuration::{MdxConstructs, MdxParseOptions, Options};
pub use crate::error::Error;
pub use crate::mdx_plugin_recma_document::JsxRuntime;

/// Turn MDX into JavaScript.
///
/// ## Examples
///
/// ```
/// use mdxjs::compile;
/// # fn main() -> Result<(), String> {
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
pub fn compile(value: &str, options: &Options) -> Result<String, Error> {
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
    let document_options = DocumentOptions {
        pragma: options.pragma.clone(),
        pragma_frag: options.pragma_frag.clone(),
        pragma_import_source: options.pragma_import_source.clone(),
        jsx_import_source: options.jsx_import_source.clone(),
        jsx_runtime: options.jsx_runtime,
    };
    let rewrite_options = RewriteOptions {
        development: options.development,
        provider_import_source: options.provider_import_source.clone(),
    };
    let build_options = BuildOptions {
        development: options.development,
    };

    let location = Location::new(value.as_bytes());
    let mdast = capture(|| to_mdast(value, &parse_options))?;
    let hast = mdast_util_to_hast(&mdast);
    let mut program = hast_util_to_swc(&hast, options.filepath.clone(), Some(&location))?;
    mdx_plugin_recma_document(&mut program, &document_options, Some(&location))?;
    mdx_plugin_recma_jsx_rewrite(&mut program, &rewrite_options, Some(&location));

    if !options.jsx {
        swc_util_build_jsx(&mut program, &build_options, Some(&location))?;
    }

    Ok(serialize(&mut program.module, Some(&program.comments)))
}
