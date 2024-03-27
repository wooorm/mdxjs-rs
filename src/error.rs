use std::{cell::RefCell, fmt::Display};

use markdown::unist::{Point, Position};
use scoped_tls::scoped_thread_local;

use crate::swc_utils::position_opt_to_string;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub point: Option<Point>,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    JsxSpreadNotSupported,
    UnexpectedContentAfterExpr,
    CannotExportTsInterfaceAsDefault,
    CannotSpecifyMultipleLayouts { previous: Option<Position> },
    UnexpectedExtraContentInSpread,
    UnexpectedPropertyInSpread,

    // Msg(String),
    Parser(swc_core::ecma::parser::error::Error),
    OnlyImportExport,
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error { kind, point: None }
    }
}

// impl From<String> for Error {
//     fn from(value: String) -> Self {
//         Error {
//             kind: ErrorKind::Msg(value),
//             point: None,
//         }
//     }
// }
// impl From<&'_ str> for Error {
//     fn from(value: &'_ str) -> Self {
//         Self::from(value.to_string())
//     }
// }

impl From<swc_core::ecma::parser::error::Error> for Error {
    fn from(value: swc_core::ecma::parser::error::Error) -> Self {
        Self::from(ErrorKind::Parser(value))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(point) = &self.point {
            write!(f, "{}:{} ", point.line, point.column)?;
        } else {
            write!(f, "0:0 ")?;
        }

        match self.kind {
            ErrorKind::JsxSpreadNotSupported => write!(
                f,
                "Unexpected spread child, which is not supported in Babel, SWC, or React"
            )?,
            ErrorKind::UnexpectedContentAfterExpr => write!(
                f,
                "Could not parse expression with swc: Unexpected content after expression"
            )?,
            ErrorKind::CannotExportTsInterfaceAsDefault => write!(
                f,
                "Cannot use TypeScript interface declarations as default export in MDX files. The \
                 default export is reserved for a layout, which must be a component"
            )?,
            ErrorKind::CannotSpecifyMultipleLayouts { previous } => write!(
                f,
                "Cannot specify multiple layouts (previous: {})",
                position_opt_to_string(previous.as_ref())
            )?,

            ErrorKind::UnexpectedExtraContentInSpread => write!(
                f,
                "Unexpected extra content in spread (such as `{{...x,y}}`): only a single spread \
                 is supported (such as `{{...x}}`)"
            )?,

            ErrorKind::UnexpectedPropertyInSpread => write!(
                f,
                "Unexpected prop in spread (such as `{{x}}`): only a spread is supported (such as \
                 `{{...x}}`)"
            )?,

            ErrorKind::Parser(err) => write!(f, "{}", err.kind().msg())?,
            ErrorKind::OnlyImportExport => write!(f, "Only import and export are supported")?,
        }
        Ok(())
    }
}

scoped_thread_local!(static ERROR: RefCell<Option<Error>>);

pub(crate) fn capture<F, T, E>(op: F) -> Result<T, Error>
where
    F: FnOnce() -> Result<T, E>,
    Error: From<E>,
{
    let error = RefCell::default();

    let result = ERROR.set(&error, || match op() {
        Ok(value) => Ok(value),
        Err(err) => Err(err),
    });

    match RefCell::into_inner(error) {
        Some(err) => return Err(err),
        None => Ok(result?),
    }
}

pub(crate) fn set_error(error: Error) {
    ERROR.with(|e| {
        *e.borrow_mut() = Some(error);
    });
}
