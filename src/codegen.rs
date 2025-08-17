use crate::{ast, mir};
use map::Id;
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
        let mut target = project.add_sprite(file.source_slice(span).to_owned());

        let variables = mir
            .variables
            .iter()
            .map(|(it, _)| it)
            .zip(std::iter::repeat_with(|| {
                target.add_variable(sb3::Variable {
                    name: String::new(), // TODO
                    value: sb3::Constant::Number(0.0),
                })
            }))
            .collect();

        let lists = mir
            .lists
            .iter()
            .map(|(it, _)| it)
            .zip(std::iter::repeat_with(|| {
                target.add_list(sb3::List {
                    name: String::new(), // TODO
                    items: Vec::new(),
                })
            }))
            .collect();

        let returns = mir
            .returns
            .iter()
            .map(|(it, _)| it)
            .zip(std::iter::repeat_with(|| {
                target.add_variable(sb3::Variable {
                    name: String::new(), // TODO
                    value: sb3::Constant::Number(0.0),
                })
            }))
            .collect();

        let mut compiler = Compiler {
            target,
            ops: HashMap::new(),
            variables,
            lists,
            returns,
        };

        let functions = &mir.basic_blocks; // TODO
        for function in functions.values() {
            compiler.function(function, mir);
        }
    }
    project.finish(output_file)
}

struct Compiler<'a> {
    target: sb3::Target<'a>,
    ops: HashMap<Id<mir::Op>, sb3::Operand>,
    variables: HashMap<Id<mir::Variable>, sb3::VariableRef>,
    lists: HashMap<Id<mir::List>, sb3::ListRef>,
    returns: HashMap<Id<mir::Return>, sb3::VariableRef>,
}

impl Compiler<'_> {
    fn function(&mut self, body: &mir::BasicBlock, mir: &mir::Program) {
        self.target.start_script(block::when_flag_clicked()); // TODO
        for &op in &body.0 {
            let res = self.op(&mir.ops[op], mir);
            self.ops.extend(Some(op).zip(res));
        }
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
                mir::Ref::Variable(variable) => self.variables[&variable].clone().into(),
                mir::Ref::List { list, index } => {
                    let index = self.value(index);
                    self.target
                        .item_num_of_list(self.lists[&list].clone(), index)
                }
            }),
            mir::Op::Store { target, value } => {
                let value = self.value(value);
                let block = match target {
                    mir::Ref::Variable(variable) => {
                        block::set_variable(self.variables[&variable].clone(), value)
                    }
                    mir::Ref::List { list, index } => {
                        block::replace(self.lists[&list].clone(), self.value(index), value)
                    }
                };
                self.target.put(block);
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
            mir::Op::Return(ref returns) => {
                for (id, &value) in returns {
                    let value = self.value(value);
                    let variable = self.returns[id].clone();
                    self.target.put(block::set_variable(variable, value));
                }
                self.target.put(block::stop_this_script());
                None
            }
            mir::Op::Call {
                function,
                ref arguments,
            } => {
                let arguments = arguments.values().map(|&it| self.value(it)).collect();
                self.target.use_custom_block(todo!(), arguments);
                None
            }
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
