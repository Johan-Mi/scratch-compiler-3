use super::{Op, Program, Value};
use std::collections::HashSet;

pub fn perform(program: &mut Program) {
    parameters(program);
    returns(program);
}

fn parameters(program: &mut Program) {
    let used_parameters: HashSet<_> = program
        .ops
        .iter()
        .flat_map(Op::args)
        .filter_map(|&it| match it {
            Value::FunctionParameter(parameter) => Some(parameter),
            _ => None,
        })
        .collect();

    program
        .parameters
        .retain(|it, _| used_parameters.contains(&it));

    for op in &mut program.ops {
        if let Op::Call { arguments, .. } = op {
            arguments.retain(|it, _| used_parameters.contains(it));
        }
    }
}

fn returns(program: &mut Program) {
    let used_returns: HashSet<_> = program
        .ops
        .iter()
        .flat_map(Op::args)
        .filter_map(|&it| match it {
            Value::Returned { id, .. } => Some(id),
            _ => None,
        })
        .collect();

    for op in &mut program.ops {
        if let Op::Return(values) = op {
            values.retain(|it, _| used_returns.contains(it));
        }
    }
}
