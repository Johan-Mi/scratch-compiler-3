use super::{BasicBlock, BasicBlockId, Op, OpId, Program, Value};
use slotmap::{SecondaryMap, SlotMap};

pub fn perform(program: &mut Program) {
    let constructs = split_sources(program);
    split_sinks(program, &constructs);
}

fn split_sinks(program: &mut Program, constructs: &SecondaryMap<OpId, Vec<Value>>) {
    let mut kills = SecondaryMap::<OpId, ()>::new();
    let mut renames = SecondaryMap::<OpId, Value>::new();
    let mut store_projections = SecondaryMap::<OpId, (Value, Vec<Value>)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Store([target, Value::Op(source)]) => {
                if let Some(fields) = constructs.get(*source) {
                    assert!(kills.insert(id, ()).is_none());
                    assert!(store_projections
                        .insert(id, (target.clone(), fields.clone()))
                        .is_none());
                }
            }
            Op::Extract { r#struct, index } => {
                match r#struct {
                    Value::FunctionParameter { .. } => todo!(),
                    Value::Op(source_op) => {
                        if let Some(fields) = constructs.get(*source_op) {
                            assert!(kills.insert(id, ()).is_none());
                            assert!(renames.insert(id, fields[*index].clone()).is_none());
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
                        if let Value::Op(source_op) = value {
                            if let Some(fields) = constructs.get(*source_op) {
                                return fields.clone();
                            }
                        }
                        Vec::from([value.clone()])
                    })
                    .collect();
            }
            Op::Call { .. } => todo!(),
            _ => {}
        }
    }

    for (before, (target, fields)) in store_projections {
        let new_ops = fields
            .iter()
            .enumerate()
            .flat_map(|(index, field)| {
                let target_field = program.ops.insert(Op::Project {
                    struct_ref: target.clone(),
                    index,
                });
                let store = program
                    .ops
                    .insert(Op::Store([Value::Op(target_field), field.clone()]));
                [target_field, store]
            })
            .collect();
        insert_before(&mut program.basic_blocks, new_ops, before);
    }

    for op in program.ops.values_mut() {
        for arg in op.args_mut() {
            if let Value::Op(arg_op) = *arg {
                if let Some(replacement) = renames.get(arg_op) {
                    *arg = replacement.clone();
                }
            }
        }
    }

    program
        .ops
        .retain(|id, _| !(constructs.contains_key(id) || kills.contains_key(id)));
}

fn split_sources(program: &mut Program) -> SecondaryMap<OpId, Vec<Value>> {
    let mut constructs = SecondaryMap::<OpId, Vec<Value>>::new();
    let mut load_projections = SecondaryMap::<OpId, (Value, usize)>::new();

    for (id, op) in &mut program.ops {
        match op {
            Op::Load { r#type, source } if r#type.is_struct() => {
                assert!(load_projections
                    .insert(id, (source.clone(), program.struct_types[*r#type].len()))
                    .is_none());
            }
            Op::Construct(fields) => {
                assert!(constructs.insert(id, std::mem::take(fields)).is_none());
            }
            Op::Index { r#type, .. } if r#type.is_struct() => todo!(),
            Op::Project { .. } => todo!(),
            _ => {}
        }
    }

    for (before, (source, field_count)) in load_projections {
        let (fields, new_ops) = (0..field_count)
            .map(|index| {
                let field = program.ops.insert(Op::Project {
                    struct_ref: source.clone(),
                    index,
                });
                (Value::Op(field), field)
            })
            .unzip();
        assert!(constructs.insert(before, fields).is_none());
        insert_before(&mut program.basic_blocks, new_ops, before);
    }

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
