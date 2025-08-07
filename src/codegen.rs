use crate::{ast, mir};
use beach_map::Id;
use sb3_builder::{self as sb3, block};
use std::{collections::HashMap, error::Error, fs::File};

pub fn compile(
    code_map: &codemap::CodeMap,
    asts: &[ast::Document],
    mir: &mir::Program,
    output_path: &str,
) -> Result<(), Box<dyn Error>> {
    let mut project = sb3::Project::default();
    let output_file = File::create(output_path)?;
    for sprite in asts
        .iter()
        .flat_map(|it| it.sprites())
        .filter_map(ast::Sprite::name)
    {
        let span = sprite.span();
        let file = code_map.find_file(span.low());
        let target = project.add_sprite(file.source_slice(span).to_owned());

        let mut compiler = Compiler {
            target,
            ops: HashMap::new(),
        };

        let functions = []; // TODO
        for function in functions {
            compiler.function(function, mir);
        }
    }
    project.finish(output_file)
}

struct Compiler<'a> {
    target: sb3::Target<'a>,
    ops: HashMap<Id<mir::Op>, sb3::Operand>,
}

impl Compiler<'_> {
    fn function(&mut self, body: Id<mir::BasicBlock>, mir: &mir::Program) {
        self.basic_block(body, mir);
    }

    fn basic_block(&mut self, block: Id<mir::BasicBlock>, mir: &mir::Program) {
        for &op in &mir.basic_blocks[block].0 {
            let res = self.op(&mir.ops[op], mir);
            self.ops.extend(Some(op).zip(res));
        }
    }

    fn op(&mut self, op: &mir::Op, mir: &mir::Program) -> Option<sb3::Operand> {
        match *op {
            mir::Op::Load { source } => Some(match source {
                mir::Ref::Variable(_) => (todo!() as sb3::VariableRef).into(),
                mir::Ref::List { list, index } => {
                    let index = self.value(index);
                    self.target.item_num_of_list(todo!(), index)
                }
            }),
            mir::Op::Store { target, value } => {
                let value = self.value(value);
                self.target.put(match target {
                    mir::Ref::Variable(_) => block::set_variable(todo!(), value),
                    mir::Ref::List { list, index } => {
                        block::replace(todo!(), self.value(index), value)
                    }
                });
                None
            }
            mir::Op::Repeat { times, body } => {
                let times = self.value(times);
                let after = self.target.repeat(times);
                self.basic_block(body, mir);
                let _: sb3::InsertionPoint = self.target.insert_at(after);
                None
            }
            mir::Op::Forever(body) => {
                self.target.forever();
                self.basic_block(body, mir);
                None
            }
            mir::Op::If {
                condition,
                then,
                r#else,
            } => {
                let condition = self.value(condition);
                let [else_point, after] = self.target.if_else(condition);
                self.basic_block(then, mir);
                let _: sb3::InsertionPoint = self.target.insert_at(else_point);
                self.basic_block(r#else, mir);
                let _: sb3::InsertionPoint = self.target.insert_at(after);
                None
            }
            mir::Op::Return(_) => todo!(),
            mir::Op::Call { .. } => todo!(),
            mir::Op::Add(args) => {
                let [lhs, rhs] = args.map(|it| self.value(it));
                Some(self.target.add(lhs, rhs))
            }
        }
    }

    fn value(&mut self, value: mir::Value) -> sb3::Operand {
        match value {
            mir::Value::FunctionParameter(_) => todo!(),
            mir::Value::Op(op) => self.ops.remove(&op).unwrap(),
            mir::Value::Returned { .. } => todo!(),
            mir::Value::Num(n) => n.into(),
            mir::Value::String(s) => s.to_owned().into(),
            mir::Value::Bool(b) => b.to_string().into(),
        }
    }
}
