use crate::{ast, mir};
use beach_map::Id;
use sb3_builder as sb3;
use std::{error::Error, fs::File};

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

        let mut compiler = Compiler { target };

        let functions = []; // TODO
        for function in functions {
            compiler.function(function, mir);
        }
    }
    project.finish(output_file)
}

struct Compiler<'a> {
    target: sb3::Target<'a>,
}

impl Compiler<'_> {
    fn function(&mut self, body: Id<mir::BasicBlock>, mir: &mir::Program) {
        self.basic_block(body, mir);
    }

    fn basic_block(&mut self, block: Id<mir::BasicBlock>, mir: &mir::Program) {
        for &op in &mir.basic_blocks[block].0 {
            self.op(&mir.ops[op], mir);
        }
    }

    fn op(&mut self, op: &mir::Op, mir: &mir::Program) {
        match *op {
            mir::Op::Load { .. } => todo!(),
            mir::Op::Store { .. } => todo!(),
            mir::Op::Repeat { times, body } => {
                let times = self.value(times);
                let after = self.target.repeat(times);
                self.basic_block(body, mir);
                let _: sb3::InsertionPoint = self.target.insert_at(after);
            }
            mir::Op::Forever(body) => {
                self.target.forever();
                self.basic_block(body, mir);
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
            }
            mir::Op::Return(_) => todo!(),
            mir::Op::Call { .. } => todo!(),
            mir::Op::Add(_) => todo!(),
        }
    }

    fn value(&mut self, value: mir::Value) -> sb3::Operand {
        match value {
            mir::Value::FunctionParameter(_) => todo!(),
            mir::Value::Op(_) => todo!(),
            mir::Value::Returned { .. } => todo!(),
            mir::Value::Num(n) => n.into(),
            mir::Value::String(s) => s.to_owned().into(),
            mir::Value::Bool(b) => b.to_string().into(),
        }
    }
}
