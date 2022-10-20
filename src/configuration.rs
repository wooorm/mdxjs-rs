//! Configuration.

use crate::mdx_plugin_recma_document::JsxRuntime;

/// Like `Constructs` from `markdown-rs`.
///
/// You can’t use:
///
/// *   `autolink`
/// *   `code_indented`
/// *   `html_flow`
/// *   `html_text`
/// *   `mdx_esm`
/// *   `mdx_expression_flow`
/// *   `mdx_expression_text`
/// *   `mdx_jsx_flow`
/// *   `mdx_jsx_text`
///
// To do: link all docs when `markdown-rs` is stable.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MdxConstructs {
    pub attention: bool,
    pub block_quote: bool,
    pub character_escape: bool,
    pub character_reference: bool,
    pub code_fenced: bool,
    pub code_text: bool,
    pub definition: bool,
    pub frontmatter: bool,
    pub gfm_autolink_literal: bool,
    pub gfm_footnote_definition: bool,
    pub gfm_label_start_footnote: bool,
    pub gfm_strikethrough: bool,
    pub gfm_table: bool,
    pub gfm_task_list_item: bool,
    pub hard_break_escape: bool,
    pub hard_break_trailing: bool,
    pub heading_atx: bool,
    pub heading_setext: bool,
    pub label_start_image: bool,
    pub label_start_link: bool,
    pub label_end: bool,
    pub list_item: bool,
    pub math_flow: bool,
    pub math_text: bool,
    pub thematic_break: bool,
}

impl Default for MdxConstructs {
    /// MDX with `CommonMark`.
    ///
    /// `CommonMark` is a relatively strong specification of how markdown
    /// works.
    /// Most markdown parsers try to follow it.
    ///
    /// For more information, see the `CommonMark` specification:
    /// <https://spec.commonmark.org>.
    fn default() -> Self {
        Self {
            attention: true,
            block_quote: true,
            character_escape: true,
            character_reference: true,
            code_fenced: true,
            code_text: true,
            definition: true,
            frontmatter: false,
            gfm_autolink_literal: false,
            gfm_label_start_footnote: false,
            gfm_footnote_definition: false,
            gfm_strikethrough: false,
            gfm_table: false,
            gfm_task_list_item: false,
            hard_break_escape: true,
            hard_break_trailing: true,
            heading_atx: true,
            heading_setext: true,
            label_start_image: true,
            label_start_link: true,
            label_end: true,
            list_item: true,
            math_flow: false,
            math_text: false,
            thematic_break: true,
        }
    }
}

impl MdxConstructs {
    /// MDX with GFM.
    ///
    /// GFM stands for **GitHub flavored markdown**.
    /// GFM extends `CommonMark` and adds support for autolink literals,
    /// footnotes, strikethrough, tables, and tasklists.
    ///
    /// For more information, see the GFM specification:
    /// <https://github.github.com/gfm/>.
    pub fn gfm() -> Self {
        Self {
            gfm_autolink_literal: true,
            gfm_footnote_definition: true,
            gfm_label_start_footnote: true,
            gfm_strikethrough: true,
            gfm_table: true,
            gfm_task_list_item: true,
            ..Self::default()
        }
    }
}

/// Like `ParseOptions` from `markdown-rs`.
///
/// The constructs you can pass are limited.
///
/// Additionally, you can’t use:
///
/// *   `mdx_expression_parse`
/// *   `mdx_esm_parse`
// To do: link all docs when `markdown-rs` is stable.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MdxParseOptions {
    pub constructs: MdxConstructs,
    pub gfm_strikethrough_single_tilde: bool,
    pub math_text_single_dollar: bool,
}

impl Default for MdxParseOptions {
    /// MDX with `CommonMark` defaults.
    fn default() -> Self {
        Self {
            constructs: MdxConstructs::default(),
            gfm_strikethrough_single_tilde: true,
            math_text_single_dollar: true,
        }
    }
}

impl MdxParseOptions {
    /// MDX with GFM.
    ///
    /// GFM stands for GitHub flavored markdown.
    /// GFM extends `CommonMark` and adds support for autolink literals,
    /// footnotes, strikethrough, tables, and tasklists.
    ///
    /// For more information, see the GFM specification:
    /// <https://github.github.com/gfm/>
    pub fn gfm() -> Self {
        Self {
            constructs: MdxConstructs::gfm(),
            ..Self::default()
        }
    }
}

// To do: use `Format`.
// /// Format the file is in (default: `Format::Detect`).
// #[derive(Clone, Debug, Default, Eq, PartialEq)]
// pub enum Format {
//     /// Use `Format::Markdown` for files with an extension in `md_extensions`
//     /// and `Format::Mdx` otherwise.
//     #[default]
//     Detect,
//     /// Treat file as MDX.
//     Mdx,
//     /// Treat file as plain vanilla markdown.
//     Markdown,
// }

// To do: use `OutputFormat`.
// /// Output format to generate (default: `OutputFormat::Program`).
// #[derive(Clone, Debug, Default, Eq, PartialEq)]
// pub enum OutputFormat {
//     /// The `Program` format will use import statements to import the JSX
//     /// runtime (and optionally provider) and use an export statement to yield
//     /// the `MDXContent` component.
//     #[default]
//     Program,
//     /// The `FunctionBody` format will get the JSX runtime (and optionally
//     /// provider) from `arguments[0]`, rewrite export statements, and use a
//     /// return statement to yield what was exported.
//     /// Normally, this output format will throw on `import` (and
//     /// `export … from`) statements, but you can support them by setting
//     /// `options.useDynamicImport`.
//     FunctionBody,
// }

/// Configuration (optional).
#[derive(Clone, Debug)]
pub struct Options {
    /// Configuration that describes how to parse from markdown.
    pub parse: MdxParseOptions,

    // /// List of markdown extensions, with dot.
    // ///
    // /// Default: `vec![".md".into(), ".markdown".into(), ".mdown".into(), ".mkdn".into(), ".mkd".into(), ".mdwn".into(), ".mkdown".into(), ".ron".into()]`.
    // pub md_extensions: Option<Vec<String>>,
    // /// List of MDX extensions, with dot.
    // ///
    // /// Default: `vec![".mdx".into()]`.
    // pub mdx_extensions: Option<Vec<String>>,
    // /// Format the file is in (default: `Format::Detect`).
    // pub format: Format,
    // /// To do: support `output_format: FunctionBody
    // /// Output format to generate (default: `OutputFormat::Program`).
    // ///
    // /// In most cases `OutputFormat::Program` should be used, as it results in a
    // /// whole program.
    // /// `OutputFormat::FunctionBody` can be used to compile to code that can be
    // /// `eval`ed.
    // /// In some cases, you might want to do that, such as when compiling on the
    // /// server and running on the client.
    // pub output_format: OutputFormat,
    // /// Whether to compile to dynamic import expressions (default:
    // /// `false`).
    // ///
    // /// This option applies when `options.output_format` is
    // /// `OutputFormat::FunctionBody`.
    // ///
    // /// This project can turn import statements (`import x from 'y'`) into
    // /// dynamic imports (`const {x} = await import('y')`).
    // /// This is useful because import statements only work at the top level of
    // /// JavaScript modules, whereas `import()` is available inside function
    // /// bodies.
    // ///
    // /// When you turn `use_dynamic_import` on, you should probably set
    // /// `options.base_url` too.
    // pub use_dynamic_import: bool,
    // /// Resolve `import`s (and `export … from`, `import.meta.url`) from this
    // /// URL (default: `None`, example: `Some("https://example.com/".into())`).
    // ///
    // /// Relative specifiers are non-absolute URLs that start with `/`, `./`, or
    // /// `../`.
    // /// For example: `/index.js`, `./folder/file.js`, or `../main.js`.
    // ///
    // /// This option is useful when code will run in a different place.
    // /// One example is when `.mdx` files are in path *a* but compiled to path
    // /// *b* and imports should run relative the path *b*.
    // /// Another example is when evaluating code, whether in Node or a browser.
    // pub base_url: Option<String>,
    /// Whether to add extra information to error messages in generated code
    /// (default: `false`).
    ///
    /// When in the automatic JSX runtime, this also enabled its development
    /// functionality.
    pub development: bool,

    // To do: some alternative to generate source maps.
    // SourceMapGenerator
    /// Place to import a provider from (default: `None`, example:
    /// `Some("@mdx-js/react").into()`).
    ///
    /// Useful for runtimes that support context (React, Preact).
    /// The provider must export a `useMDXComponents`, which is called to
    /// access an object of components.
    pub provider_import_source: Option<String>,

    /// Whether to keep JSX (default: `false`).
    ///
    /// The default is to compile JSX away so that the resulting file is
    /// immediately runnable.
    pub jsx: bool,

    /// JSX runtime to use (default: `Some(JsxRuntime::Automatic)`).
    ///
    /// The classic runtime compiles to calls such as `h('p')`, the automatic
    /// runtime compiles to
    /// `import _jsx from '$importSource/jsx-runtime'\n_jsx('p')`.
    pub jsx_runtime: Option<JsxRuntime>,

    /// Place to import automatic JSX runtimes from (`Option<String>`, default:
    /// `Some("react".into())`).
    ///
    /// When in the automatic runtime, this is used to define an import for
    /// `_Fragment`, `_jsx`, and `_jsxs`.
    pub jsx_import_source: Option<String>,

    /// Pragma for JSX (default: `Some("React.createElement".into())`).
    ///
    /// When in the classic runtime, this is used as an identifier for function
    /// calls: `<x />` to `React.createElement('x')`.
    ///
    /// You should most probably define `pragma_frag` and `pragma_import_source`
    /// too when changing this.
    pub pragma: Option<String>,

    /// Pragma for JSX fragments (default: `Some("React.Fragment".into())`).
    ///
    /// When in the classic runtime, this is used as an identifier for
    /// fragments: `<>` to `React.createElement(React.Fragment)`.
    ///
    /// You should most probably define `pragma` and `pragma_import_source`
    /// too when changing this.
    pub pragma_frag: Option<String>,

    /// Where to import the identifier of `pragma` from (default:
    /// `Some("react".into())`).
    ///
    /// When in the classic runtime, this is used to import the `pragma`
    /// function.
    /// To illustrate with an example: when `pragma` is `"a.b"` and
    /// `pragma_import_source` is `"c"`, the following will be generated:
    /// `import a from 'c'`.
    pub pragma_import_source: Option<String>,

    // New:
    /// File path to the source file (example:
    /// `Some("path/to/example.mdx".into())`).
    ///
    /// Used when `development: true` to improve error messages.
    pub filepath: Option<String>,
}

impl Default for Options {
    /// Default options to use the automatic JSX runtime with React
    /// and handle MDX according to `CommonMark`.
    fn default() -> Self {
        Self {
            parse: MdxParseOptions::default(),
            development: false,
            provider_import_source: None,
            jsx: false,
            jsx_runtime: Some(JsxRuntime::default()),
            jsx_import_source: None,
            pragma: None,
            pragma_frag: None,
            pragma_import_source: None,
            filepath: None,
        }
    }
}

impl Options {
    /// MDX with GFM.
    ///
    /// GFM stands for GitHub flavored markdown.
    /// GFM extends `CommonMark` and adds support for autolink literals,
    /// footnotes, strikethrough, tables, and tasklists.
    /// On the compilation side, GFM turns on the GFM tag filter.
    /// The tagfilter is useless, but it’s included here for consistency.
    ///
    /// For more information, see the GFM specification:
    /// <https://github.github.com/gfm/>
    pub fn gfm() -> Self {
        Self {
            parse: MdxParseOptions::gfm(),
            ..Self::default()
        }
    }
}
