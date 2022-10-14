extern crate mdxjs;
use pretty_assertions::assert_eq;

#[test]
fn xxx() -> Result<(), String> {
    assert_eq!(
        mdxjs::compile("")?,
        "function _createMdxContent(props) {
    return <></>;
}
function MDXContent(props = {}) {
    const { wrapper: MDXLayout  } = props.components || {};
    return MDXLayout ? <MDXLayout {...props}><_createMdxContent {...props}/></MDXLayout> : _createMdxContent(props);
}
export default MDXContent;
",
        "yyy"
    );

    Ok(())
}
