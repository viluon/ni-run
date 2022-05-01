
pub trait BooleanAssertions {
    fn expect<E, F: FnOnce() -> E>(&self, err: F) -> Result<(), E>;
}

impl BooleanAssertions for bool {
    fn expect<E, F: FnOnce() -> E>(&self, err: F) -> Result<(), E> {
        match *self {
            true  => Ok(()),
            false => Err(err()),
        }
    }
}
