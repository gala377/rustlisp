use std::iter::Zip;

pub trait JoinedIterator<A, B> {
    fn zip(self) -> Zip<A, B>;
}

impl<A: IntoIterator, B: IntoIterator> JoinedIterator<A::IntoIter, B::IntoIter> for (A, B) {
    fn zip(self) -> Zip<A::IntoIter, B::IntoIter> {
        let (first, second) = self;
        first.into_iter().zip(second.into_iter())
    }
}

pub fn accumulate_vectors<T: Clone>(mut a: Vec<T>, b: &Vec<T>) -> Vec<T> {
    a.extend_from_slice(&b);
    a
}
