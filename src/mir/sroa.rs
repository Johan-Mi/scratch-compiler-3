use super::{
    BasicBlock, BasicBlockId, Either, Op, OpId, ParameterId, Program, Ref, RefBase, ReturnId,
    TypeId, Value,
};
use slotmap::{SecondaryMap, SlotMap};

pub fn perform(program: &mut Program) {
    let split_parameters = split_parameters(program);
    let constructs = split_sources(program);
    let split_returns = split_things(&program.struct_types, &mut program.returns);
    split_sinks(program, &constructs, &split_parameters, &split_returns);
    split_projections(program);
}

fn split_parameters(program: &mut Program) -> SecondaryMap<ParameterId, Vec<ParameterId>> {
    let splits = split_things(&program.struct_types, &mut program.parameters);

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

fn split_projections(program: &mut Program) {
    let split_variables = split_things(&program.struct_types, &mut program.variables);
    let split_lists = split_things(&program.struct_types, &mut program.lists);

    for (index, r#ref) in program
        .ops
        .values_mut()
        .flat_map(Op::refs_mut)
        .filter_map(|it| Some((it.projections.pop()?, it)))
    {
        match &mut r#ref.base {
            RefBase::Variable(variable) => *variable = split_variables[*variable][index],
            RefBase::List { list, .. } => *list = split_lists[*list][index],
        }
    }
}

fn split_things<T: slotmap::Key>(
    struct_types: &SlotMap<TypeId, Vec<TypeId>>,
    things: &mut SlotMap<T, TypeId>,
) -> SecondaryMap<T, Vec<T>> {
    things
        .iter()
        .filter_map(|(it, &r#type)| Some((it, struct_types.get(r#type)?)))
        .collect::<Vec<_>>()
        .into_iter()
        .map(|(it, field_types)| {
            let fields = field_types
                .iter()
                .map(|&field_type| things.insert(field_type))
                .collect();
            (it, fields)
        })
        .collect()
}

fn split_sinks(
    program: &mut Program,
    constructs: &SecondaryMap<OpId, Vec<Value>>,
    split_parameters: &SecondaryMap<ParameterId, Vec<ParameterId>>,
    split_returns: &SecondaryMap<ReturnId, Vec<ReturnId>>,
) {
    let mut renames = SecondaryMap::<OpId, Value>::new();
    let mut store_projections = SecondaryMap::<OpId, (Ref, Vec<Value>)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Store { target, value } => {
                if let Some(fields) = match value {
                    Value::Op(source) => constructs.get(*source).cloned(),
                    Value::FunctionParameter(source) => {
                        split_parameters.get(*source).map(|fields| {
                            fields
                                .iter()
                                .copied()
                                .map(Value::FunctionParameter)
                                .collect()
                        })
                    }
                    Value::Returned {
                        call,
                        id: return_id,
                    } => split_returns.get(*return_id).map(|fields| {
                        fields
                            .iter()
                            .map(|&it| Value::Returned {
                                call: *call,
                                id: it,
                            })
                            .collect()
                    }),
                    _ => None,
                } {
                    assert!(store_projections
                        .insert(id, (target.clone(), fields))
                        .is_none());
                }
            }
            Op::Extract { r#struct, index } => {
                let field = match r#struct {
                    Value::FunctionParameter(source) => {
                        Value::FunctionParameter(split_parameters[*source][*index])
                    }
                    Value::Op(source_op) => constructs[*source_op][*index],
                    Value::Returned {
                        call,
                        id: return_id,
                    } => Value::Returned {
                        call: *call,
                        id: split_returns[*return_id][*index],
                    },
                    Value::Num(_) | Value::String(_) | Value::Bool(_) => unreachable!(),
                };
                assert!(renames.insert(id, field).is_none());
            }
            Op::Return(values) => split_other_things(
                values,
                split_returns,
                split_parameters,
                split_returns,
                constructs,
            ),
            Op::Call { arguments, .. } => split_other_things(
                arguments,
                split_parameters,
                split_parameters,
                split_returns,
                constructs,
            ),
            _ => {}
        }
    }

    for (old, (target, fields)) in &store_projections {
        let new = fields
            .iter()
            .enumerate()
            .map(|(index, &field)| {
                let mut target_field = target.clone();
                target_field.projections.insert(0, index);
                program.ops.insert(Op::Store {
                    target: target_field,
                    value: field,
                })
            })
            .collect();
        splice(&mut program.basic_blocks, old, new);
    }

    for arg in program.ops.values_mut().flat_map(Op::args_mut) {
        if let Value::Op(arg_op) = *arg {
            if let Some(&replacement) = renames.get(arg_op) {
                *arg = replacement;
            }
        }
    }

    program.ops.retain(|id, _| !renames.contains_key(id));
}

fn split_other_things<T: slotmap::Key>(
    values: &mut SecondaryMap<T, Value>,
    ids: &SecondaryMap<T, Vec<T>>,
    split_parameters: &SecondaryMap<ParameterId, Vec<ParameterId>>,
    split_returns: &SecondaryMap<ReturnId, Vec<ReturnId>>,
    constructs: &SecondaryMap<OpId, Vec<Value>>,
) {
    *values = values
        .iter()
        .flat_map(|(id, value)| {
            use Either::{Left, Right};

            match value {
                Value::FunctionParameter(source) => {
                    if let Some(fields) = split_parameters.get(*source) {
                        return Left(Left(
                            ids[id]
                                .iter()
                                .copied()
                                .zip(fields.iter().copied().map(Value::FunctionParameter)),
                        ));
                    }
                }
                Value::Op(source_op) => {
                    if let Some(fields) = constructs.get(*source_op) {
                        return Left(Right(ids[id].iter().copied().zip(fields.iter().copied())));
                    }
                }
                Value::Returned {
                    call,
                    id: return_id,
                } => {
                    if let Some(fields) = split_returns.get(*return_id) {
                        return Right(Left(ids[id].iter().copied().zip(fields.iter().map(
                            |&it| Value::Returned {
                                call: *call,
                                id: it,
                            },
                        ))));
                    }
                }
                _ => {}
            }
            Right(Right(std::iter::once((id, *value))))
        })
        .collect();
}

fn split_sources(program: &mut Program) -> SecondaryMap<OpId, Vec<Value>> {
    let mut constructs = SecondaryMap::<OpId, Vec<Value>>::new();
    let mut load_projections = Vec::<(OpId, Ref, TypeId)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Load { r#type, source } if r#type.is_struct() => {
                load_projections.push((id, source.clone(), *r#type));
            }
            Op::Construct(fields) => {
                assert!(constructs.insert(id, std::mem::take(fields)).is_none());
            }
            _ => {}
        }
    }

    for (old, source, r#type) in load_projections {
        let (fields, new) = program.struct_types[r#type]
            .iter()
            .enumerate()
            .map(|(index, &r#type)| {
                let mut field_ref = source.clone();
                field_ref.projections.insert(0, index);
                let field = program.ops.insert(Op::Load {
                    r#type,
                    source: field_ref,
                });
                (Value::Op(field), field)
            })
            .unzip();
        assert!(constructs.insert(old, fields).is_none());
        splice(&mut program.basic_blocks, old, new);
    }

    program.ops.retain(|id, _| !constructs.contains_key(id));

    constructs
}

fn splice(basic_blocks: &mut SlotMap<BasicBlockId, BasicBlock>, old: OpId, new: Vec<OpId>) {
    for basic_block in basic_blocks.values_mut() {
        if let Some(index) = basic_block.0.iter().position(|&it| it == old) {
            _ = basic_block.0.splice(index..=index, new);
            return;
        }
    }
}
