use core::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FilePos {
    line: u32,
    column: u32,
}

impl FilePos {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    pub fn line(&self) -> u32 {
        self.line
    }
    pub fn column(&self) -> u32 {
        self.column
    }

    pub fn advance_col(&mut self) {
        self.advance_col_by(1);
    }
    pub fn advance_col_by(&mut self, by: u32) {
        self.column += by;
    }

    pub fn new_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

impl fmt::Display for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct Position {
    file_name: Rc<String>,
    start: FilePos,
    end: Option<FilePos>,
}

impl Position {
    pub fn new(file_name: Rc<String>, start: FilePos, end: Option<FilePos>) -> Self {
        Self {
            file_name,
            start,
            end,
        }
    }

    pub fn file_name(&self) -> &Rc<String> {
        &self.file_name
    }
    pub fn start(&self) -> &FilePos {
        &self.start
    }
    pub fn end(&self) -> &Option<FilePos> {
        &self.end
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.end {
            Some(end) => write!(f, "{} ({}:{})", self.file_name, self.start, end),
            None => write!(f, "{} ({})", self.file_name, self.start),
        }
    }
}

pub struct PositionTagged<T>(T, Position);

impl<T> PositionTagged<T> {
    pub fn new(value: T, position: Position) -> Self {
        Self(value, position)
    }

    pub fn new_from_to(value: T, from: Position, to: Position) -> Self {
        Self(
            value,
            Position::new(
                from.file_name().clone(),
                from.start().clone(),
                to.end().clone(),
            ),
        )
    }

    pub fn value(&self) -> &T {
        &self.0
    }

    pub fn position(&self) -> &Position {
        &self.1
    }

    pub fn take(self) -> (T, Position) {
        (self.0, self.1)
    }

    pub fn split(&self) -> (&T, &Position) {
        (&self.0, &self.1)
    }
}

impl<T: Clone> Clone for PositionTagged<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<T> From<T> for PositionTagged<T> {
    fn from(t: T) -> Self {
        Self::new(
            t,
            Position::new(Rc::new(String::new()), FilePos::new(0, 0), None),
        )
    }
}

impl<T: fmt::Debug> fmt::Debug for PositionTagged<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_tuple("PositionTagged")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}

pub fn tag_position<T>(
    t: T,
    file_name: Rc<String>,
    start: FilePos,
    end: Option<FilePos>,
) -> PositionTagged<T> {
    PositionTagged(
        t,
        Position {
            file_name,
            start,
            end,
        },
    )
}
