use core::fmt;

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

#[derive(Debug, Clone, Copy)]
pub struct Position<'a> {
    file_name: &'a str,
    start: FilePos,
    end: Option<FilePos>,
}

impl<'a> Position<'a> {
    pub fn new(file_name: &'a str, start: FilePos, end: Option<FilePos>) -> Self {
        Self {
            file_name,
            start,
            end,
        }
    }

    pub fn file_name(&self) -> &'a str {
        self.file_name
    }
    pub fn start(&self) -> &FilePos {
        &self.start
    }
    pub fn end(&self) -> &Option<FilePos> {
        &self.end
    }
}

impl fmt::Display for Position<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.end {
            Some(end) => write!(f, "{} ({}:{})", self.file_name, self.start, end),
            None => write!(f, "{} ({})", self.file_name, self.start),
        }
    }
}

pub struct PositionTagged<'a, T>(T, Position<'a>);

impl<'a, T> PositionTagged<'a, T> {
    pub fn new(value: T, position: Position<'a>) -> Self {
        Self(value, position)
    }

    pub fn value(&self) -> &T {
        &self.0
    }

    pub fn position(&self) -> &Position<'a> {
        &self.1
    }
}

impl<T: fmt::Debug> fmt::Debug for PositionTagged<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_tuple("PositionTagged")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}

pub fn tag_position<'a, T>(
    t: T,
    file_name: &'a str,
    start: FilePos,
    end: Option<FilePos>,
) -> PositionTagged<'a, T> {
    PositionTagged(
        t,
        Position {
            file_name,
            start,
            end,
        },
    )
}
