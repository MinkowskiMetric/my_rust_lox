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
    end: FilePos,
}

impl Position {
    pub fn new(file_name: Rc<String>, start: FilePos, end: FilePos) -> Self {
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
    pub fn end(&self) -> &FilePos {
        &self.end
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{} ({}:{})", self.file_name, self.start, self.end)
    }
}
