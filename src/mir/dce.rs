use super::{Op, ParameterId, Program, Value};
use slotmap::SecondaryMap;

pub fn perform(program: &mut Program) {
    let used_parameters: SecondaryMap<ParameterId, ()> = program
        .ops
        .values_mut()
        .flat_map(Op::args_mut)
        .filter_map(|&mut it| {
            if let Value::FunctionParameter(parameter) = it {
                Some((parameter, ()))
            } else {
                None
            }
        })
        .collect();

    for function in program.functions.values_mut() {
        function
            .parameters
            .retain(|it, ()| used_parameters.contains_key(it));
    }

    for op in program.ops.values_mut() {
        if let Op::Call { arguments, .. } = op {
            arguments.retain(|it, _| used_parameters.contains_key(it));
        }
    }
}
