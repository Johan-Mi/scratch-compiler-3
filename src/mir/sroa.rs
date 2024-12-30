use super::{
    BasicBlock, BasicBlockId, Either, Op, OpId, ParameterId, Program, TypeId, Value, VariableId,
};
use slotmap::{SecondaryMap, SlotMap};

pub fn perform(program: &mut Program) {
    let split_function_parameters = split_function_parameters(program);
    let split_variables = split_variables(program);
    let constructs = split_sources(program);
    split_sinks(
        program,
        &constructs,
        &split_function_parameters,
        &split_variables,
    );
}

fn split_function_parameters(program: &mut Program) -> SecondaryMap<ParameterId, Vec<ParameterId>> {
    let splits: SecondaryMap<ParameterId, Vec<ParameterId>> = program
        .parameters
        .iter()
        .filter_map(|(param, &r#type)| Some((param, program.struct_types.get(r#type)?)))
        .collect::<Vec<_>>()
        .into_iter()
        .map(|(param, field_types)| {
            let fields = field_types
                .iter()
                .map(|&field_type| program.parameters.insert(field_type))
                .collect();
            (param, fields)
        })
        .collect();

    for function in program.functions.values_mut() {
        function.parameters = function
            .parameters
            .keys()
            .flat_map(|it| {
                splits.get(it).map_or_else(
                    || Either::Left(std::iter::once(it)),
                    |fields| Either::Right(fields.iter().copied()),
                )
            })
            .map(|it| (it, ()))
            .collect();
    }

    splits
}

fn split_variables(program: &mut Program) -> SecondaryMap<VariableId, Vec<VariableId>> {
    program
        .variables
        .iter()
        .filter_map(|(variable, &r#type)| Some((variable, program.struct_types.get(r#type)?)))
        .collect::<Vec<_>>()
        .into_iter()
        .map(|(variable, field_types)| {
            let fields = field_types
                .iter()
                .map(|&field_type| program.variables.insert(field_type))
                .collect();
            (variable, fields)
        })
        .collect()
}

fn split_sinks(
    program: &mut Program,
    constructs: &SecondaryMap<OpId, Vec<Value>>,
    split_function_parameters: &SecondaryMap<ParameterId, Vec<ParameterId>>,
    split_variables: &SecondaryMap<VariableId, Vec<VariableId>>,
) {
    let mut renames = SecondaryMap::<OpId, Value>::new();
    let mut store_projections = SecondaryMap::<OpId, (Value, Vec<Value>)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Store([target, Value::Op(source)]) => {
                if let Some(fields) = constructs.get(*source) {
                    assert!(store_projections
                        .insert(id, (*target, fields.clone()))
                        .is_none());
                }
            }
            Op::Store([target, Value::FunctionParameter(source)]) => {
                if let Some(fields) = split_function_parameters.get(*source) {
                    let fields = fields
                        .iter()
                        .copied()
                        .map(Value::FunctionParameter)
                        .collect();
                    assert!(store_projections.insert(id, (*target, fields)).is_none());
                }
            }
            Op::Extract { r#struct, index } => {
                match r#struct {
                    Value::FunctionParameter(source) => {
                        let fields = &split_function_parameters[*source];
                        assert!(renames
                            .insert(id, Value::FunctionParameter(fields[*index]))
                            .is_none());
                    }
                    Value::Op(source_op) => {
                        if let Some(fields) = constructs.get(*source_op) {
                            assert!(renames.insert(id, fields[*index]).is_none());
                        }
                    }
                    Value::Call { .. } => todo!(),
                    Value::Num(_) | Value::String(_) | Value::Bool(_) | Value::VariableRef(_) => {
                        unreachable!()
                    }
                };
            }
            Op::Project {
                struct_ref: Value::Op(_),
                index,
            } => todo!(),
            Op::Project {
                struct_ref: Value::VariableRef(source),
                index,
            } => {
                assert!(renames
                    .insert(id, Value::VariableRef(split_variables[*source][*index]))
                    .is_none());
            }
            Op::Return(values) => {
                *values = values
                    .iter()
                    .flat_map(|value| {
                        match value {
                            Value::FunctionParameter(source) => {
                                return split_function_parameters[*source]
                                    .iter()
                                    .copied()
                                    .map(Value::FunctionParameter)
                                    .collect();
                            }
                            Value::Op(source_op) => {
                                if let Some(fields) = constructs.get(*source_op) {
                                    return fields.clone();
                                }
                            }
                            _ => {}
                        }
                        Vec::from([*value])
                    })
                    .collect();
            }
            Op::Call { .. } => todo!(),
            _ => {}
        }
    }

    for (before, (target, fields)) in &store_projections {
        let new_ops = fields
            .iter()
            .enumerate()
            .flat_map(|(index, field)| {
                let target_field = program.ops.insert(Op::Project {
                    struct_ref: *target,
                    index,
                });
                let store = program
                    .ops
                    .insert(Op::Store([Value::Op(target_field), *field]));
                [target_field, store]
            })
            .collect();
        insert_before(&mut program.basic_blocks, new_ops, before);
    }

    for op in program.ops.values_mut() {
        for arg in op.args_mut() {
            if let Value::Op(arg_op) = *arg {
                if let Some(&replacement) = renames.get(arg_op) {
                    *arg = replacement;
                }
            }
        }
    }

    program
        .ops
        .retain(|id, _| !(store_projections.contains_key(id) || renames.contains_key(id)));
}

fn split_sources(program: &mut Program) -> SecondaryMap<OpId, Vec<Value>> {
    let mut constructs = SecondaryMap::<OpId, Vec<Value>>::new();
    let mut load_projections = Vec::<(OpId, Value, TypeId)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Load { r#type, source } if r#type.is_struct() => {
                load_projections.push((id, *source, *r#type));
            }
            Op::Construct(fields) => {
                assert!(constructs.insert(id, std::mem::take(fields)).is_none());
            }
            Op::Index { r#type, .. } if r#type.is_struct() => todo!(),
            _ => {}
        }
    }

    for (before, source, r#type) in load_projections {
        let mut fields = Vec::new();
        let new_ops = program.struct_types[r#type]
            .iter()
            .enumerate()
            .flat_map(|(index, &r#type)| {
                let field_ref = program.ops.insert(Op::Project {
                    struct_ref: source,
                    index,
                });
                let field = program.ops.insert(Op::Load {
                    r#type,
                    source: Value::Op(field_ref),
                });
                fields.push(Value::Op(field));
                [field_ref, field]
            })
            .collect();
        assert!(constructs.insert(before, fields).is_none());
        insert_before(&mut program.basic_blocks, new_ops, before);
    }

    program.ops.retain(|id, _| !constructs.contains_key(id));

    constructs
}

fn insert_before(
    basic_blocks: &mut SlotMap<BasicBlockId, BasicBlock>,
    new_ops: Vec<OpId>,
    before: OpId,
) {
    for basic_block in basic_blocks.values_mut() {
        if let Some(index) = basic_block.0.iter().position(|&it| it == before) {
            basic_block.0.splice(index..index, new_ops);
            return;
        }
    }
}
