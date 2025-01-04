use super::{
    BasicBlock, BasicBlockId, Either, ListId, Op, OpId, ParameterId, Program, Ref, RefBase, TypeId,
    Value, VariableId,
};
use slotmap::{SecondaryMap, SlotMap};

pub fn perform(program: &mut Program) {
    let split_function_parameters = split_function_parameters(program);
    let constructs = split_sources(program);
    split_sinks(program, &constructs, &split_function_parameters);
    split_projections(program);
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

fn split_projections(program: &mut Program) {
    let split_variables = split_variables(program);
    let split_lists = split_lists(program);

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

fn split_lists(program: &mut Program) -> SecondaryMap<ListId, Vec<ListId>> {
    program
        .lists
        .iter()
        .filter_map(|(list, &r#type)| Some((list, program.struct_types.get(r#type)?)))
        .collect::<Vec<_>>()
        .into_iter()
        .map(|(list, field_types)| {
            let fields = field_types
                .iter()
                .map(|&field_type| program.lists.insert(field_type))
                .collect();
            (list, fields)
        })
        .collect()
}

fn split_sinks(
    program: &mut Program,
    constructs: &SecondaryMap<OpId, Vec<Value>>,
    split_function_parameters: &SecondaryMap<ParameterId, Vec<ParameterId>>,
) {
    let mut renames = SecondaryMap::<OpId, Value>::new();
    let mut store_projections = SecondaryMap::<OpId, (Ref, Vec<Value>)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Store {
                target,
                value: Value::Op(source),
            } => {
                if let Some(fields) = constructs.get(*source) {
                    assert!(store_projections
                        .insert(id, (target.clone(), fields.clone()))
                        .is_none());
                }
            }
            Op::Store {
                target,
                value: Value::FunctionParameter(source),
            } => {
                if let Some(fields) = split_function_parameters.get(*source) {
                    let fields = fields
                        .iter()
                        .copied()
                        .map(Value::FunctionParameter)
                        .collect();
                    assert!(store_projections
                        .insert(id, (target.clone(), fields))
                        .is_none());
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
            .map(|(index, &field)| {
                let mut target_field = target.clone();
                target_field.projections.insert(0, index);
                program.ops.insert(Op::Store {
                    target: target_field,
                    value: field,
                })
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

    for (before, source, r#type) in load_projections {
        let (fields, new_ops) = program.struct_types[r#type]
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
