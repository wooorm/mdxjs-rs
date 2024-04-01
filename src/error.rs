use markdown::unist::Point;

#[derive(Debug)]
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
