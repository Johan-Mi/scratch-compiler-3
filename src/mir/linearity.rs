use super::{BasicBlock, Op, Program, Ref, Value, Variable};
use map::Id;
use std::collections::HashMap;

pub fn spill(program: &mut Program) {
    let defs: HashMap<Id<Op>, Id<BasicBlock>> = program
        .basic_blocks
        .iter()
        .flat_map(|(basic_block, body)| body.0.iter().map(move |&it| (it, basic_block)))
        .collect();

    let mut uses: Vec<(Id<Op> /*def*/, Id<Op> /*user*/)> = program
        .ops
        .iter()
        .flat_map(|(id, op)| {
            op.args().filter_map(move |&it| match it {
                Value::Op(arg) => Some((arg, id)),
                _ => None,
            })
        })
        .collect();
    uses.sort_unstable_by_key(|it| it.0);

    let mut variable = None;
    let mut prev_def = None;
    for (def, user) in uses {
        if prev_def != Some(def) {
            variable = None;
        }
        prev_def = Some(def);
        let variable = *variable.get_or_insert_with(|| {
            let basic_block = &mut program.basic_blocks[defs[&def]];
            let index = basic_block.0.iter().position(|&it| it == def).unwrap();
            let variable = program.variables.insert(Variable);
            let store = program.ops.insert(Op::Store {
                target: Ref::Variable(variable),
                value: Value::Op(def),
            });
            basic_block.0.insert(index + 1, store);
            variable
        });
        let basic_block = &mut program.basic_blocks[defs[&user]];
        let index = basic_block.0.iter().position(|&it| it == user).unwrap();
        let load = program.ops.insert(Op::Load {
            source: Ref::Variable(variable),
        });
        let arg = program.ops[user]
            .args_mut()
            .find(|&&mut it| matches!(it, Value::Op(op) if op == def))
            .unwrap();
        *arg = Value::Op(load);
        basic_block.0.insert(index, load);
    }
}
