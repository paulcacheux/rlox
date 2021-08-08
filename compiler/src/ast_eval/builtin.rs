use std::io::Write;

use super::{EvalError, Evaluator, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinFunction {
    Clock,
}

impl BuiltinFunction {
    pub fn arity(self) -> usize {
        match self {
            BuiltinFunction::Clock => 0,
        }
    }

    pub fn eval<W: Write>(
        self,
        evaluator: &Evaluator<W>,
        _args: &[Value],
    ) -> Result<Value, EvalError> {
        match self {
            BuiltinFunction::Clock => Ok(clock_eval(evaluator)),
        }
    }
}

fn clock_eval<W: Write>(evalutor: &Evaluator<W>) -> Value {
    let elapsed = evalutor.start_eval_time.elapsed().as_secs();
    Value::Number(elapsed as _)
}
