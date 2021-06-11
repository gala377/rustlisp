pub mod data;
mod eval;
pub mod gc;
pub mod reader;
pub mod runtime;
pub mod stdlib;
pub mod utils;

pub use reader::{read, ParseError, AST};

pub use eval::Interpreter;
