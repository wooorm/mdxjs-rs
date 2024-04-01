use std::fmt::Display;

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

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(point) = &self.point {
            write!(f, "{}:{} ", point.line, point.column)?;
        }

        write!(f, "{}", self.msg)
    }
}
