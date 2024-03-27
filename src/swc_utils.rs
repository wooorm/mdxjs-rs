//! Lots of helpers for dealing with SWC, particularly from unist, and for
//! building its ES AST.

use markdown::{
    id_cont, id_start,
    mdast::Stop,
    unist::{Point, Position},
    Location,
};

use swc_core::common::{BytePos, Span, SyntaxContext, DUMMY_SP};
use swc_core::ecma::ast::{
    BinExpr, BinaryOp, Bool, CallExpr, Callee, ComputedPropName, Expr, ExprOrSpread, Ident,
    JSXAttrName, JSXElementName, JSXMemberExpr, JSXNamespacedName, JSXObject, Lit, MemberExpr,
    MemberProp, Null, Number, ObjectLit, PropName, PropOrSpread, Str,
};
use swc_core::ecma::visit::{noop_visit_mut_type, VisitMut};

use crate::Error;

/// Turn a unist position, into an SWC span, of two byte positions.
///
/// > 👉 **Note**: SWC byte positions are offset by one: they are `0` when they
/// > are missing or incremented by `1` when valid.
pub fn position_to_span(position: Option<&Position>) -> Span {
    position.map_or(DUMMY_SP, |d| Span {
        lo: point_to_bytepos(&d.start),
        hi: point_to_bytepos(&d.end),
        ctxt: SyntaxContext::empty(),
    })
}

/// Turn an SWC span, of two byte positions, into a unist position.
///
/// This assumes the span comes from a fixed tree, or is a dummy.
///
/// > 👉 **Note**: SWC byte positions are offset by one: they are `0` when they
/// > are missing or incremented by `1` when valid.
pub fn span_to_position(span: &Span, location: Option<&Location>) -> Option<Position> {
    let lo = span.lo.0 as usize;
    let hi = span.hi.0 as usize;

    if lo > 0 && hi > 0 {
        if let Some(location) = location {
            if let Some(start) = location.to_point(lo - 1) {
                if let Some(end) = location.to_point(hi - 1) {
                    return Some(Position { start, end });
                }
            }
        }
    }

    None
}

/// Turn a unist point into an SWC byte position.
///
/// > 👉 **Note**: SWC byte positions are offset by one: they are `0` when they
/// > are missing or incremented by `1` when valid.
pub fn point_to_bytepos(point: &Point) -> BytePos {
    BytePos(point.offset as u32 + 1)
}

/// Turn an SWC byte position into a unist point.
///
/// This assumes the byte position comes from a fixed tree, or is a dummy.
///
/// > 👉 **Note**: SWC byte positions are offset by one: they are `0` when they
/// > are missing or incremented by `1` when valid.
pub fn bytepos_to_point(bytepos: BytePos, location: Option<&Location>) -> Option<Point> {
    let pos = bytepos.0 as usize;

    if pos > 0 {
        if let Some(location) = location {
            return location.to_point(pos - 1);
        }
    }

    None
}

/// Prefix an error message with an optional point.
pub fn prefix_error_with_point(mut reason: Error, point: Option<&Point>) -> Error {
    reason.point = point.cloned();
    reason
}

/// Serialize a unist position for humans.
pub fn position_opt_to_string(position: Option<&Position>) -> String {
    if let Some(position) = position {
        position_to_string(position)
    } else {
        "0:0".into()
    }
}

/// Serialize a unist position for humans.
pub fn position_to_string(position: &Position) -> String {
    format!(
        "{}-{}",
        point_to_string(&position.start),
        point_to_string(&position.end)
    )
}

/// Serialize a unist point for humans.
pub fn point_opt_to_string(point: Option<&Point>) -> String {
    if let Some(point) = point {
        point_to_string(point)
    } else {
        "0:0".into()
    }
}

/// Serialize a unist point for humans.
pub fn point_to_string(point: &Point) -> String {
    format!("{}:{}", point.line, point.column)
}

/// Visitor to fix SWC byte positions.
///
/// This assumes the byte position comes from an **unfixed** tree.
///
/// > 👉 **Note**: SWC byte positions are offset by one: they are `0` when they
/// > are missing or incremented by `1` when valid.
#[derive(Debug, Default, Clone)]
pub struct RewriteStopsContext<'a> {
    /// Stops in the original source.
    pub stops: &'a [Stop],
    /// Location info.
    pub location: Option<&'a Location>,
}

impl<'a> VisitMut for RewriteStopsContext<'a> {
    noop_visit_mut_type!();

    /// Rewrite spans.
    fn visit_mut_span(&mut self, span: &mut Span) {
        let mut result = DUMMY_SP;
        let lo_rel = span.lo.0 as usize;
        let hi_rel = span.hi.0 as usize;

        let lo_clean = Location::relative_to_absolute(self.stops, lo_rel - 1);
        let hi_clean = Location::relative_to_absolute(self.stops, hi_rel - 1);
        if let Some(lo_abs) = lo_clean {
            if let Some(hi_abs) = hi_clean {
                result = create_span(lo_abs as u32 + 1, hi_abs as u32 + 1);
            }
        }

        *span = result;
    }
}

/// Visitor to fix SWC byte positions by removing a prefix.
///
/// > 👉 **Note**: SWC byte positions are offset by one: they are `0` when they
/// > are missing or incremented by `1` when valid.
#[derive(Debug, Default, Clone)]
pub struct RewritePrefixContext {
    /// Size of prefix considered outside this tree.
    pub prefix_len: u32,
}

impl VisitMut for RewritePrefixContext {
    noop_visit_mut_type!();

    /// Rewrite spans.
    fn visit_mut_span(&mut self, span: &mut Span) {
        let mut result = DUMMY_SP;
        if span.lo.0 > self.prefix_len && span.hi.0 > self.prefix_len {
            result = create_span(span.lo.0 - self.prefix_len, span.hi.0 - self.prefix_len);
        }

        *span = result;
    }
}

/// Visitor to drop SWC spans.
#[derive(Debug, Default, Clone)]
pub struct DropContext {}

impl VisitMut for DropContext {
    noop_visit_mut_type!();

    /// Rewrite spans.
    fn visit_mut_span(&mut self, span: &mut Span) {
        *span = DUMMY_SP;
    }
}

/// Generate a span.
pub fn create_span(lo: u32, hi: u32) -> Span {
    Span {
        lo: BytePos(lo),
        hi: BytePos(hi),
        ctxt: SyntaxContext::default(),
    }
}

/// Generate an ident.
///
/// ```js
/// a
/// ```
pub fn create_ident(sym: &str) -> Ident {
    Ident {
        sym: sym.into(),
        optional: false,
        span: DUMMY_SP,
    }
}

/// Generate an ident expression.
///
/// ```js
/// a
/// ```
pub fn create_ident_expression(sym: &str) -> Expr {
    Expr::Ident(create_ident(sym))
}

/// Generate a null.
pub fn create_null() -> Null {
    Null {
        span: swc_core::common::DUMMY_SP,
    }
}

/// Generate a null.
pub fn create_null_lit() -> Lit {
    Lit::Null(create_null())
}

/// Generate a null.
pub fn create_null_expression() -> Expr {
    Expr::Lit(create_null_lit())
}

/// Generate a null.
pub fn create_str(value: &str) -> Str {
    value.into()
}

/// Generate a str.
pub fn create_str_lit(value: &str) -> Lit {
    Lit::Str(create_str(value))
}

/// Generate a str.
pub fn create_str_expression(value: &str) -> Expr {
    Expr::Lit(create_str_lit(value))
}

/// Generate a bool.
pub fn create_bool(value: bool) -> Bool {
    value.into()
}

/// Generate a bool.
pub fn create_bool_lit(value: bool) -> Lit {
    Lit::Bool(create_bool(value))
}

/// Generate a bool.
pub fn create_bool_expression(value: bool) -> Expr {
    Expr::Lit(create_bool_lit(value))
}

/// Generate a number.
pub fn create_num(value: f64) -> Number {
    value.into()
}

/// Generate a num.
pub fn create_num_lit(value: f64) -> Lit {
    Lit::Num(create_num(value))
}

/// Generate a num.
pub fn create_num_expression(value: f64) -> Expr {
    Expr::Lit(create_num_lit(value))
}

/// Generate an object.
pub fn create_object_lit(value: Vec<PropOrSpread>) -> ObjectLit {
    ObjectLit {
        props: value,
        span: DUMMY_SP,
    }
}

/// Generate an object.
pub fn create_object_expression(value: Vec<PropOrSpread>) -> Expr {
    Expr::Object(create_object_lit(value))
}

/// Generate a call.
pub fn create_call(callee: Callee, args: Vec<ExprOrSpread>) -> CallExpr {
    CallExpr {
        callee,
        args,
        span: DUMMY_SP,
        type_args: None,
    }
}

/// Generate a call.
pub fn create_call_expression(callee: Callee, args: Vec<ExprOrSpread>) -> Expr {
    Expr::Call(create_call(callee, args))
}

/// Generate a binary expression.
///
/// ```js
/// a + b + c
/// a || b
/// ```
pub fn create_binary_expression(mut exprs: Vec<Expr>, op: BinaryOp) -> Expr {
    exprs.reverse();

    let mut left = None;

    while let Some(right_expr) = exprs.pop() {
        left = Some(if let Some(left_expr) = left {
            Expr::Bin(BinExpr {
                left: Box::new(left_expr),
                right: Box::new(right_expr),
                op,
                span: DUMMY_SP,
            })
        } else {
            right_expr
        });
    }

    left.expect("expected one or more expressions")
}

/// Generate a member expression from a string.
///
/// ```js
/// a.b
/// a
/// ```
pub fn create_member_expression_from_str(name: &str) -> Expr {
    match parse_js_name(name) {
        // `a`
        JsName::Normal(name) => create_ident_expression(name),
        // `a.b.c`
        JsName::Member(parts) => {
            let mut member = create_member(
                create_ident_expression(parts[0]),
                create_member_prop_from_str(parts[1]),
            );
            let mut index = 2;
            while index < parts.len() {
                member = create_member(
                    Expr::Member(member),
                    create_member_prop_from_str(parts[index]),
                );
                index += 1;
            }
            Expr::Member(member)
        }
    }
}

/// Generate a member expression from an object and prop.
pub fn create_member(obj: Expr, prop: MemberProp) -> MemberExpr {
    MemberExpr {
        obj: Box::new(obj),
        prop,
        span: DUMMY_SP,
    }
}

/// Create a member prop from a string.
pub fn create_member_prop_from_str(name: &str) -> MemberProp {
    if is_identifier_name(name) {
        MemberProp::Ident(create_ident(name))
    } else {
        MemberProp::Computed(ComputedPropName {
            expr: Box::new(create_str_expression(name)),
            span: DUMMY_SP,
        })
    }
}

/// Generate a member expression from a string.
///
/// ```js
/// a.b-c
/// a
/// ```
pub fn create_jsx_name_from_str(name: &str) -> JSXElementName {
    match parse_jsx_name(name) {
        // `a`
        JsxName::Normal(name) => JSXElementName::Ident(create_ident(name)),
        // `a:b`
        JsxName::Namespace(ns, name) => JSXElementName::JSXNamespacedName(JSXNamespacedName {
            ns: create_ident(ns),
            name: create_ident(name),
        }),
        // `a.b.c`
        JsxName::Member(parts) => {
            let mut member = create_jsx_member(
                JSXObject::Ident(create_ident(parts[0])),
                create_ident(parts[1]),
            );
            let mut index = 2;
            while index < parts.len() {
                member = create_jsx_member(
                    JSXObject::JSXMemberExpr(Box::new(member)),
                    create_ident(parts[index]),
                );
                index += 1;
            }
            JSXElementName::JSXMemberExpr(member)
        }
    }
}

/// Generate a member expression from an object and prop.
pub fn create_jsx_member(obj: JSXObject, prop: Ident) -> JSXMemberExpr {
    JSXMemberExpr { obj, prop }
}

/// Turn an JSX element name into an expression.
pub fn jsx_element_name_to_expression(node: JSXElementName) -> Expr {
    match node {
        JSXElementName::JSXMemberExpr(member_expr) => {
            jsx_member_expression_to_expression(member_expr)
        }
        JSXElementName::JSXNamespacedName(namespace_name) => create_str_expression(&format!(
            "{}:{}",
            namespace_name.ns.sym, namespace_name.name.sym
        )),
        JSXElementName::Ident(ident) => create_ident_or_literal(&ident),
    }
}

/// Create a JSX attribute name.
pub fn create_jsx_attr_name_from_str(name: &str) -> JSXAttrName {
    match parse_jsx_name(name) {
        JsxName::Member(_) => {
            unreachable!("member expressions in attribute names are not supported")
        }
        // `<a b:c />`
        JsxName::Namespace(ns, name) => JSXAttrName::JSXNamespacedName(JSXNamespacedName {
            ns: create_ident(ns),
            name: create_ident(name),
        }),
        // `<a b />`
        JsxName::Normal(name) => JSXAttrName::Ident(create_ident(name)),
    }
}

/// Turn a JSX member expression name into a member expression.
pub fn jsx_member_expression_to_expression(node: JSXMemberExpr) -> Expr {
    Expr::Member(create_member(
        jsx_object_to_expression(node.obj),
        ident_to_member_prop(&node.prop),
    ))
}

/// Turn an ident into a member prop.
pub fn ident_to_member_prop(node: &Ident) -> MemberProp {
    if is_identifier_name(node.as_ref()) {
        MemberProp::Ident(Ident {
            sym: node.sym.clone(),
            optional: false,
            span: node.span,
        })
    } else {
        MemberProp::Computed(ComputedPropName {
            expr: Box::new(create_str_expression(&node.sym)),
            span: node.span,
        })
    }
}

/// Turn a JSX attribute name into a prop prop.
pub fn jsx_attribute_name_to_prop_name(node: JSXAttrName) -> PropName {
    match node {
        JSXAttrName::JSXNamespacedName(namespace_name) => create_prop_name(&format!(
            "{}:{}",
            namespace_name.ns.sym, namespace_name.name.sym
        )),
        JSXAttrName::Ident(ident) => create_prop_name(&ident.sym),
    }
}

/// Turn a JSX object into an expression.
pub fn jsx_object_to_expression(node: JSXObject) -> Expr {
    match node {
        JSXObject::Ident(ident) => create_ident_or_literal(&ident),
        JSXObject::JSXMemberExpr(member_expr) => jsx_member_expression_to_expression(*member_expr),
    }
}

/// Create either an ident expression or a literal expression.
pub fn create_ident_or_literal(node: &Ident) -> Expr {
    if is_identifier_name(node.as_ref()) {
        create_ident_expression(node.sym.as_ref())
    } else {
        create_str_expression(&node.sym)
    }
}

/// Create a prop name.
pub fn create_prop_name(name: &str) -> PropName {
    if is_identifier_name(name) {
        PropName::Ident(create_ident(name))
    } else {
        PropName::Str(create_str(name))
    }
}

/// Check if a name is a literal tag name or an identifier to a component.
pub fn is_literal_name(name: &str) -> bool {
    matches!(name.as_bytes().first(), Some(b'a'..=b'z')) || !is_identifier_name(name)
}

/// Check if a name is a valid identifier name.
pub fn is_identifier_name(name: &str) -> bool {
    for (index, char) in name.chars().enumerate() {
        if if index == 0 {
            !id_start(char)
        } else {
            !id_cont(char, false)
        } {
            return false;
        }
    }

    true
}

/// Different kinds of JS names.
pub enum JsName<'a> {
    /// Member: `a.b.c`
    Member(Vec<&'a str>),
    /// Name: `a`
    Normal(&'a str),
}

/// Different kinds of JSX names.
pub enum JsxName<'a> {
    /// Member: `a.b.c`
    Member(Vec<&'a str>),
    /// Namespace: `a:b`
    Namespace(&'a str, &'a str),
    /// Name: `a`
    Normal(&'a str),
}

/// Parse a JavaScript member expression or name.
pub fn parse_js_name(name: &str) -> JsName {
    let bytes = name.as_bytes();
    let mut index = 0;
    let mut start = 0;
    let mut parts = vec![];

    while index < bytes.len() {
        if bytes[index] == b'.' {
            parts.push(&name[start..index]);
            start = index + 1;
        }

        index += 1;
    }

    // `a`
    if parts.is_empty() {
        JsName::Normal(name)
    }
    // `a.b.c`
    else {
        parts.push(&name[start..]);
        JsName::Member(parts)
    }
}

/// Parse a JSX name from a string.
pub fn parse_jsx_name(name: &str) -> JsxName {
    match parse_js_name(name) {
        // `<a.b.c />`
        JsName::Member(parts) => JsxName::Member(parts),
        JsName::Normal(name) => {
            // `<a:b />`
            if let Some(colon) = name.as_bytes().iter().position(|d| matches!(d, b':')) {
                JsxName::Namespace(&name[0..colon], &name[(colon + 1)..])
            }
            // `<a />`
            else {
                JsxName::Normal(name)
            }
        }
    }
}

/// Get the identifiers used in a JSX member expression.
///
/// `Foo.Bar` -> `vec!["Foo", "Bar"]`
pub fn jsx_member_to_parts(node: &JSXMemberExpr) -> Vec<&str> {
    let mut parts = vec![];
    let mut member_opt = Some(node);

    while let Some(member) = member_opt {
        parts.push(member.prop.sym.as_ref());
        match &member.obj {
            JSXObject::Ident(d) => {
                parts.push(d.sym.as_ref());
                member_opt = None;
            }
            JSXObject::JSXMemberExpr(node) => {
                member_opt = Some(node);
            }
        }
    }

    parts.reverse();
    parts
}

/// Check if a text value is inter-element whitespace.
///
/// See: <https://github.com/syntax-tree/hast-util-whitespace>.
pub fn inter_element_whitespace(value: &str) -> bool {
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn bytepos_to_point_test() {
        assert_eq!(
            bytepos_to_point(BytePos(123), None),
            None,
            "should support no location"
        );
    }

    #[test]
    fn prefix_error_with_point_test() {
        assert_eq!(
            prefix_error_with_point("aaa", None),
            "0:0: aaa",
            "should support no point"
        );
    }

    #[test]
    fn position_opt_to_string_test() {
        assert_eq!(
            position_opt_to_string(None),
            "0:0",
            "should support no position"
        );
    }

    #[test]
    fn point_opt_to_string_test() {
        assert_eq!(point_opt_to_string(None), "0:0", "should support no point");
    }

    #[test]
    fn jsx_member_to_parts_test() {
        assert_eq!(
            jsx_member_to_parts(&JSXMemberExpr {
                prop: create_ident("a"),
                obj: JSXObject::Ident(create_ident("b"))
            }),
            vec!["b", "a"],
            "should support a member with 2 items"
        );

        assert_eq!(
            jsx_member_to_parts(&JSXMemberExpr {
                prop: create_ident("a"),
                obj: JSXObject::JSXMemberExpr(Box::new(JSXMemberExpr {
                    prop: create_ident("b"),
                    obj: JSXObject::JSXMemberExpr(Box::new(JSXMemberExpr {
                        prop: create_ident("c"),
                        obj: JSXObject::Ident(create_ident("d"))
                    }))
                }))
            }),
            vec!["d", "c", "b", "a"],
            "should support a member with 4 items"
        );
    }
}
