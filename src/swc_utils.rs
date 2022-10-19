//! Lots of helpers for dealing with SWC, particularly from unist.

use markdown::{
    id_cont, id_start,
    mdast::Stop,
    unist::{Point, Position},
    Location,
};

use swc_common::{BytePos, Span, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::{BinExpr, BinaryOp, ComputedPropName, Expr, Ident, MemberExpr, MemberProp};
use swc_ecma_visit::{noop_visit_mut_type, VisitMut};

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
pub fn prefix_error_with_point(reason: String, point: Option<&Point>) -> String {
    if let Some(point) = point {
        format!("{}: {}", point_to_string(point), reason)
    } else {
        reason
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
pub struct RewriteContext<'a> {
    pub prefix_len: usize,
    pub stops: &'a [Stop],
    pub location: Option<&'a Location>,
}

impl<'a> VisitMut for RewriteContext<'a> {
    noop_visit_mut_type!();

    // Rewrite spans.
    fn visit_mut_span(&mut self, span: &mut Span) {
        let mut result = DUMMY_SP;
        let lo_rel = span.lo.0 as usize;
        let hi_rel = span.hi.0 as usize;

        if lo_rel > self.prefix_len && hi_rel > self.prefix_len {
            if let Some(lo_abs) =
                Location::relative_to_absolute(self.stops, lo_rel - 1 - self.prefix_len)
            {
                if let Some(hi_abs) =
                    Location::relative_to_absolute(self.stops, hi_rel - 1 - self.prefix_len)
                {
                    result = Span {
                        lo: BytePos(lo_abs as u32 + 1),
                        hi: BytePos(hi_abs as u32 + 1),
                        ctxt: SyntaxContext::empty(),
                    };
                }
            }
        }

        *span = result;
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
                span: swc_common::DUMMY_SP,
            })
        } else {
            right_expr
        });
    }

    left.expect("expected one or more expressions")
}

/// Generate a member expression.
///
/// ```js
/// a.b
/// a
/// ```
pub fn create_member_expression(name: &str) -> Expr {
    match parse_js_name(name) {
        // `a`
        JsName::Normal(name) => create_ident_expression(name),
        // `a.b.c`
        JsName::Member(parts) => {
            let mut member = MemberExpr {
                obj: Box::new(create_ident_expression(parts[0])),
                prop: create_member_prop(parts[1]),
                span: swc_common::DUMMY_SP,
            };
            let mut index = 2;
            while index < parts.len() {
                member = MemberExpr {
                    obj: Box::new(Expr::Member(member)),
                    prop: create_member_prop(parts[index]),
                    span: swc_common::DUMMY_SP,
                };
                index += 1;
            }
            Expr::Member(member)
        }
    }
}

pub fn create_member_prop(name: &str) -> MemberProp {
    if is_identifier_name(name) {
        MemberProp::Ident(create_ident(name))
    } else {
        MemberProp::Computed(ComputedPropName {
            expr: Box::new(swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Str(
                swc_ecma_ast::Str {
                    value: name.into(),
                    span: swc_common::DUMMY_SP,
                    raw: None,
                },
            ))),
            span: swc_common::DUMMY_SP,
        })
    }
}

/// Check if a name is a literal tag name or an identifier to a component.
pub fn is_literal_name(name: &str) -> bool {
    matches!(name.as_bytes().first(), Some(b'a'..=b'z')) || !is_identifier_name(name)
}

// Check if a name is a valid identifier name.
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
    // `a.b.c`
    Member(Vec<&'a str>),
    // `a`
    Normal(&'a str),
}

/// Different kinds of JSX names.
pub enum JsxName<'a> {
    // `a.b.c`
    Member(Vec<&'a str>),
    // `a:b`
    Namespace(&'a str, &'a str),
    // `a`
    Normal(&'a str),
}

/// To do.
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
