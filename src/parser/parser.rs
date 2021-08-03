use crate::{PositionTagged, Token};

pub struct Parser<'a, Iter: Iterator<Item = PositionTagged<'a, Token>>> {
    tokens: Iter,
}

impl<'a, Iter: Iterator<Item = PositionTagged<'a, Token>>> Parser<'a, Iter> {
    fn new<I: IntoIterator<IntoIter = Iter>>(into: I) -> Self {
        Self {
            tokens: into.into_iter(),
        }
    }
}

pub fn parse<'a, I: IntoIterator<Item = PositionTagged<'a, Token>>>(i: I) {
    let parser = Parser::new(i);
    todo!()
}
