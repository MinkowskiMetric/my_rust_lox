use crate::{LoxError, LoxResult};
use std::iter::FromIterator;

#[derive(Debug)]
pub enum CompilerResult<T> {
    Failed(Vec<LoxError>),
    Partial(Vec<LoxError>),
    Succeeded(T),
}

impl<T> CompilerResult<T> {
    #[cfg(test)]
    pub fn expect(self, msg: &'static str) -> T {
        match self {
            Self::Succeeded(t) => t,
            _ => panic!("{}", msg),
        }
    }
}

#[must_use]
pub trait CollectedErrors: Sized + Iterator {
    fn errors(self) -> Option<Vec<LoxError>>;

    fn result<F: FromIterator<Self::Item>>(mut self) -> CompilerResult<F> {
        let values = F::from_iter(&mut self);

        match self.errors() {
            Some(errors) => {
                if errors
                    .iter()
                    .find(|err| matches!(err, LoxError::UnexpectedEndOfFile(..)))
                    .is_some()
                {
                    CompilerResult::Partial(errors)
                } else {
                    CompilerResult::Failed(errors)
                }
            }

            None => CompilerResult::Succeeded(values),
        }
    }
}

pub trait CollectibleErrors {
    type Item;
    type Collector: CollectedErrors<Item = Self::Item>;

    fn collect_errors(self) -> Self::Collector;
}

#[must_use]
pub struct ErrorCollector<T, Iter: Iterator<Item = LoxResult<T>>> {
    iter: Option<Iter>,
    errors: Option<Vec<LoxError>>,
}

impl<T, Iter: Iterator<Item = LoxResult<T>>> ErrorCollector<T, Iter> {
    pub fn new<Into: IntoIterator<IntoIter = Iter>>(into: Into) -> Self {
        Self {
            iter: Some(into.into_iter()),
            errors: Some(Vec::new()),
        }
    }
}

impl<T, Iter: Iterator<Item = LoxResult<T>>> Iterator for ErrorCollector<T, Iter> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.as_mut() {
            Some(iter) => loop {
                match iter.next() {
                    Some(Ok(item)) => break Some(item),
                    Some(Err(e)) => self.errors.get_or_insert_with(Vec::new).push(e),
                    None => {
                        self.iter = None;
                        break None;
                    }
                }
            },
            None => None,
        }
    }
}

impl<T, Iter: Iterator<Item = LoxResult<T>>> CollectedErrors for ErrorCollector<T, Iter> {
    fn errors(self) -> Option<Vec<LoxError>> {
        match self.iter {
            Some(_) => panic!("Do not collect errors before completing iteration"),
            None => self.errors,
        }
    }
}

impl<Collectible: CollectedErrors> CollectibleErrors for Collectible {
    type Item = Collectible::Item;
    type Collector = Self;

    fn collect_errors(self) -> Self::Collector {
        self
    }
}

pub fn adapt_errors<R, Into: IntoIterator<Item = LoxResult<R>>>(
    iter: Into,
) -> impl CollectedErrors<Item = R> {
    ErrorCollector::new(iter)
}
