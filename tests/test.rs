extern crate mdxjs;
use mdxjs::{compile, JsxRuntime, Options};
use pretty_assertions::assert_eq;

#[test]
fn xxx() -> Result<(), String> {
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
