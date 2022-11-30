extern crate mdxjs;
use mdxjs::{compile, JsxRuntime, Options};
use pretty_assertions::assert_eq;

#[test]
fn simple() -> Result<(), String> {
    assert_eq!(
        compile("", &Options::default())?,
        "import { Fragment as _Fragment, jsx as _jsx } from \"react/jsx-runtime\";
function _createMdxContent(props) {
    return _jsx(_Fragment, {});
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {
        children: _jsx(_createMdxContent, props)
    })) : _createMdxContent(props);
}
export default MDXContent;
",
        "should work",
    );

    Ok(())
}

#[test]
fn development() -> Result<(), String> {
    assert_eq!(
        compile("<A />", &Options {
            development: true,
            filepath: Some("example.mdx".into()),
            ..Default::default()
        })?,
        "import { jsxDEV as _jsxDEV } from \"react/jsx-dev-runtime\";
function _createMdxContent(props) {
    const { A  } = props.components || {};
    if (!A) _missingMdxReference(\"A\", true, \"1:1-1:6\");
    return _jsxDEV(A, {}, undefined, false, {
        fileName: \"example.mdx\",
        lineNumber: 1,
        columnNumber: 1
    }, this);
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? _jsxDEV(MDXLayout, Object.assign({}, props, {
        children: _jsxDEV(_createMdxContent, props, undefined, false, {
            fileName: \"example.mdx\"
        }, this)
    }), undefined, false, {
        fileName: \"example.mdx\"
    }, this) : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component, place) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\" + (place ? \"\\nIt’s referenced in your code at `\" + place + \"` in `example.mdx`\" : \"\"));
}
",
        "should support `options.development: true`",
    );

    Ok(())
}

#[test]
fn provider() -> Result<(), String> {
    assert_eq!(
        compile("<A />",  &Options {
            provider_import_source: Some("@mdx-js/react".into()),
            ..Default::default()
        })?,
        "import { jsx as _jsx } from \"react/jsx-runtime\";
import { useMDXComponents as _provideComponents } from \"@mdx-js/react\";
function _createMdxContent(props) {
    const { A  } = Object.assign({}, _provideComponents(), props.components);
    if (!A) _missingMdxReference(\"A\", true);
    return _jsx(A, {});
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = Object.assign({}, _provideComponents(), props.components);
    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {
        children: _jsx(_createMdxContent, props)
    })) : _createMdxContent(props);
}
export default MDXContent;
function _missingMdxReference(id, component) {
    throw new Error(\"Expected \" + (component ? \"component\" : \"object\") + \" `\" + id + \"` to be defined: you likely forgot to import, pass, or provide it.\");
}
",
        "should support `options.provider_import_source`",
    );

    Ok(())
}

#[test]
fn jsx() -> Result<(), String> {
    assert_eq!(
        compile("", &Options {
            jsx: true,
            ..Default::default()
        })?,
        "function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
        "should support `options.jsx: true`",
    );

    Ok(())
}

#[test]
fn classic() -> Result<(), String> {
    assert_eq!(
        compile("", &Options {
            jsx_runtime: Some(JsxRuntime::Classic),
            ..Default::default()
        })?,
        "import React from \"react\";
function _createMdxContent(props) {
    return React.createElement(React.Fragment);
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? React.createElement(MDXLayout, props, React.createElement(_createMdxContent, props)) : _createMdxContent(props);
}
export default MDXContent;
",
        "should support `options.jsx_runtime: JsxRuntime::Classic`",
    );

    Ok(())
}

#[test]
fn import_source() -> Result<(), String> {
    assert_eq!(
        compile(
            "",
            &Options {
                jsx_import_source: Some("preact".into()),
                ..Default::default()
            }
        )?,
        "import { Fragment as _Fragment, jsx as _jsx } from \"preact/jsx-runtime\";
function _createMdxContent(props) {
    return _jsx(_Fragment, {});
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {
        children: _jsx(_createMdxContent, props)
    })) : _createMdxContent(props);
}
export default MDXContent;
",
        "should support `options.jsx_import_source: Some(\"preact\".into())`",
    );

    Ok(())
}

#[test]
fn pragmas() -> Result<(), String> {
    assert_eq!(
        compile("", &Options {
            jsx_runtime: Some(JsxRuntime::Classic),
            pragma: Some("a.b".into()),
            pragma_frag: Some("a.c".into()),
            pragma_import_source: Some("d".into()),
            ..Default::default()
        })?,
        "import a from \"d\";
function _createMdxContent(props) {
    return a.b(a.c);
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? a.b(MDXLayout, props, a.b(_createMdxContent, props)) : _createMdxContent(props);
}
export default MDXContent;
",
        "should support `options.pragma`, `options.pragma_frag`, `options.pragma_import_source`",
    );

    Ok(())
}

#[test]
fn unravel_elements() -> Result<(), String> {
    assert_eq!(
        compile(
            "<x>a</x>
<x>
  b
</x>
",
            &Default::default()
        )?,
        "import { Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs } from \"react/jsx-runtime\";
function _createMdxContent(props) {
    const _components = Object.assign({
        x: \"x\",
        p: \"p\"
    }, props.components);
    return _jsxs(_Fragment, {
        children: [
            _jsx(\"x\", {
                children: \"a\"
            }),
            \"\\n\",
            _jsx(\"x\", {
                children: _jsx(_components.p, {
                    children: \"b\"
                })
            })
        ]
    });
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {
        children: _jsx(_createMdxContent, props)
    })) : _createMdxContent(props);
}
export default MDXContent;
",
        "should unravel paragraphs (1)",
    );

    Ok(())
}

#[test]
fn unravel_expressions() -> Result<(), String> {
    assert_eq!(
        compile("{1} {2}", &Default::default())?,
        "import { Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs } from \"react/jsx-runtime\";
function _createMdxContent(props) {
    return _jsxs(_Fragment, {
        children: [
            1,
            \"\\n\",
            \" \",
            \"\\n\",
            2
        ]
    });
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {
        children: _jsx(_createMdxContent, props)
    })) : _createMdxContent(props);
}
export default MDXContent;
",
        "should unravel paragraphs (2)",
    );

    Ok(())
}

#[test]
fn explicit_jsx() -> Result<(), String> {
    assert_eq!(
        compile(
            "<h1>asd</h1>
# qwe
",
            &Default::default()
        )?,
        "import { Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs } from \"react/jsx-runtime\";
function _createMdxContent(props) {
    const _components = Object.assign({
        h1: \"h1\"
    }, props.components);
    return _jsxs(_Fragment, {
        children: [
            _jsx(\"h1\", {
                children: \"asd\"
            }),
            \"\\n\",
            _jsx(_components.h1, {
                children: \"qwe\"
            })
        ]
    });
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? _jsx(MDXLayout, Object.assign({}, props, {
        children: _jsx(_createMdxContent, props)
    })) : _createMdxContent(props);
}
export default MDXContent;
",
        "should not support overwriting explicit JSX",
    );

    Ok(())
}

#[test]
fn err_esm_invalid() {
    assert_eq!(
        compile("import 1/1", &Default::default()),
        Err(
            "1:8: Could not parse esm with swc: Expected 'from', got 'numeric literal (1, 1)'"
                .into()
        ),
        "should crash on invalid code in ESM",
    );
}

#[test]
fn err_expression_broken_multiline_comment_a() {
    assert_eq!(
        compile("{x/*}", &Default::default()),
        Err("1:6: Could not parse expression with swc: Unexpected eof".into()),
        "should crash on an unclosed block comment after an expression",
    );
}

#[test]
fn err_expression_broken_multiline_comment_b() {
    assert_eq!(
        compile("{/*x}", &Default::default()),
        Err("1:6: Could not parse expression with swc: Unterminated block comment".into()),
        "should crash on an unclosed block comment in an empty expression",
    );
}

#[test]
fn err_expression_broken_multiline_comment_c() {
    assert!(
        matches!(compile("{/*a*/}", &Default::default()), Ok(_)),
        "should support a valid multiline comment",
    );
}

#[test]
fn err_expression_broken_line_comment_a() {
    assert_eq!(
        compile("{x//}", &Default::default()),
        Err("1:6: Could not parse expression with swc: Unexpected unclosed line comment, expected line ending: `\\n`".into()),
        "should crash on an unclosed line comment after an expression",
    );
}

#[test]
fn err_expression_broken_line_comment_b() {
    assert_eq!(
        compile("{//x}", &Default::default()),
        Err("1:2: Could not parse expression with swc: Unexpected eof".into()),
        "should crash on an unclosed line comment in an empty expression",
    );
}

#[test]
fn err_expression_broken_line_comment_c() {
    assert!(
        matches!(compile("{//a\n}", &Default::default()), Ok(_)),
        "should support a valid line comment",
    );
}

#[test]
fn err_esm_stmt() {
    assert_eq!(
        compile("export let a = 1\nlet b = 2", &Default::default()),
        Err("2:10: Unexpected statement in code: only import/exports are supported".into()),
        "should crash on statements in ESM",
    );
}

#[test]
fn err_expression_invalid() {
    assert_eq!(
        compile("{!}", &Default::default()),
        Err("1:4: Could not parse expression with swc: Unexpected eof".into()),
        "should crash on invalid code in an expression",
    );
}

#[test]
fn err_expression_multi() {
    assert_eq!(
        compile("{x; y}", &Default::default()),
        Err("1:7: Could not parse expression with swc: Unexpected content after expression".into()),
        "should crash on more content after an expression",
    );
}

#[test]
fn err_expression_empty() {
    assert!(
        matches!(compile("a {} b", &Default::default()), Ok(_)),
        "should support an empty expression",
    );
}

#[test]
fn err_expression_comment() {
    assert!(
        matches!(compile("a { /* b */ } c", &Default::default()), Ok(_)),
        "should support a comment in an empty expression",
    );
}

#[test]
fn err_expression_value_empty() {
    assert_eq!(
        compile("<a b={ } />", &Default::default()),
        Err("1:7: Could not parse expression with swc: Unexpected eof".into()),
        "should crash on an empty value expression",
    );
}

#[test]
fn err_expression_value_invalid() {
    assert_eq!(
        compile("<a b={!} />", &Default::default()),
        Err("1:12: Could not parse expression with swc: Unexpected eof".into()),
        "should crash on an invalid value expression",
    );
}

#[test]
fn err_expression_value_comment() {
    assert_eq!(
        compile("<a b={ /*c*/ } />", &Default::default()),
        Err("1:7: Could not parse expression with swc: Unexpected eof".into()),
        "should crash on a value expression with just a comment",
    );
}

#[test]
fn err_expression_value_extra_comment() {
    assert!(
        matches!(compile("<a b={1 /*c*/ } />", &Default::default()), Ok(_)),
        "should support a value expression with a comment",
    );
}

#[test]
fn err_expression_spread_none() {
    assert_eq!(
        compile("<a {x} />", &Default::default()),
        Err("1:5: Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)".into()),
        "should crash on a non-spread",
    );
}

#[test]
fn err_expression_spread_multi_1() {
    assert_eq!(
        compile("<a {...x; y} />", &Default::default()),
        Err("1:9: Could not parse expression with swc: Expected ',', got ';'".into()),
        "should crash on more content after a (spread) expression (1)",
    );
}

#[test]
fn err_expression_spread_multi_2() {
    assert_eq!(
        compile("<a {...x, y} />", &Default::default()),
        Err("1:5: Unexpected extra content in spread (such as `{...x,y}`): only a single spread is supported (such as `{...x}`)".into()),
        "should crash on more content after a (spread) expression (2)",
    );
}

#[test]
fn err_expression_spread_empty() {
    assert_eq!(
        compile("<a {...} />", &Default::default()),
        Err("1:12: Could not parse expression with swc: Expression expected".into()),
        "should crash on an empty spread expression",
    );
}

#[test]
fn err_expression_spread_invalid() {
    assert_eq!(
        compile("<a {...?} />", &Default::default()),
        Err("1:13: Could not parse expression with swc: Expression expected".into()),
        "should crash on an invalid spread expression",
    );
}

#[test]
fn err_expression_spread_extra_comment() {
    assert!(
        matches!(compile("<a {...b /*c*/ } />", &Default::default()), Ok(_)),
        "should support a spread expression with a comment",
    );
}
