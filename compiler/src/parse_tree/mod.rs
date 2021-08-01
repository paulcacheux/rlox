mod declaration;
mod expression;
mod statement;

pub use declaration::*;
pub use expression::*;
pub use statement::*;

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}
