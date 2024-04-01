use std::{borrow::Cow, fmt::Display};

use markdown::unist::Point;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub msg: String,
    pub point: Option<Point>,
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Self {
            msg: value,
            point: None,
        }
    }
}

impl From<&str> for Error {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

impl From<Cow<'_, str>> for Error {
    fn from(value: Cow<str>) -> Self {
        value.into_owned().into()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(point) = &self.point {
            write!(f, "{}:{} ", point.line, point.column)?;
        } else {
            write!(f, "0:0 ")?;
        }

        write!(f, "{}", self.msg)
    }
}
