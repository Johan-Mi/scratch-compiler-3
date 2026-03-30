use crate::{ast, mir};
use map::Id;
use sb3_builder::{self as sb3, block};
use std::{collections::HashMap, error::Error, fs::File};

pub fn compile(
    code_map: &codemap::CodeMap,
    string_literals: &HashMap<codemap::Pos, String>,
    ast: ast::Program,
    mir: &mir::Program,
    output_path: &str,
) -> Result<(), Box<dyn Error>> {
    let mut project = sb3::Project::default();
    let output_file = File::create(output_path)?;
    for sprite in ast.documents().flat_map(ast::Document::sprites) {
        let name = sprite.name().unwrap();
        let span = name.span();
        let file = code_map.find_file(span.low());
        let mut target = project.add_sprite(file.source_slice(span).to_owned());

        for costume in sprite.costume_lists().flat_map(ast::CostumeList::iter) {
            let name = string_literals[&costume.name().span().low()].clone();
            let path = string_literals[&costume.path().unwrap().span().low()].as_ref();
            target.add_costume(sb3::Costume::from_file(name, path)?);
        }

        let variables = mir
            .variables
            .iter()
            .map(|(it, _)| it)
            .zip((0..).map(|index: usize| {
                target.add_variable(sb3::Variable {
                    name: format!("v{index}"),
                    value: sb3::Constant::Number(0.0),
                })
            }))
            .collect();

        let lists = mir
            .lists
            .iter()
            .map(|(it, _)| it)
            .zip((0..).map(|index: usize| {
                target.add_list(sb3::List {
                    name: format!("l{index}"),
                    items: Vec::new(),
                })
            }))
            .collect();

        let returns = mir
            .functions
            .iter()
            .map(|(&function, it)| {
                (
                    function,
                    std::iter::repeat_with(|| {
                        target.add_variable(sb3::Variable {
                            name: String::new(), // TODO
                            value: sb3::Constant::Number(0.0),
                        })
                    })
                    .take(it.return_value_count)
                    .collect(),
                )
            })
            .collect();

        let mut compiler = Compiler {
            target,
            ops: HashMap::new(),
            variables,
            lists,
            returns,
            string_literals,
        };

        for &function in mir.functions.keys() {
            compiler.function(&mir.basic_blocks[function], mir);
        }
    }
    project.finish(output_file)
}

struct Compiler<'a> {
    target: sb3::Target<'a>,
    ops: HashMap<Id<mir::Op>, sb3::Operand>,
    variables: HashMap<Id<mir::Variable>, sb3::VariableRef>,
    lists: HashMap<Id<mir::List>, sb3::ListRef>,
    returns: HashMap<Id<mir::BasicBlock>, Vec<sb3::VariableRef>>,
    string_literals: &'a HashMap<codemap::Pos, String>,
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
            mir::Op::For {
                variable,
                times,
                body,
            } => {
                let times = self.value(times);
                let after = self.target.for_(self.variables[&variable].clone(), times);
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
            mir::Op::Return {
                function,
                ref values,
            } => {
                for (variable, &value) in self.returns[&function].clone().into_iter().zip(values) {
                    let value = self.value(value);
                    self.target.put(block::set_variable(variable, value));
                }
                self.target.put(block::stop_this_script());
                None
            }
            mir::Op::Call {
                function,
                ref arguments,
            } => {
                let arguments = arguments.iter().map(|&it| self.value(it)).collect();
                self.target.use_custom_block(todo!(), arguments);
                None
            }
            mir::Op::DeleteAll(list) => {
                self.target
                    .put(block::delete_all_of_list(self.lists[&list].clone()));
                None
            }
            mir::Op::Push { list, value } => {
                let value = self.value(value);
                self.target
                    .put(block::append(self.lists[&list].clone(), value));
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
            mir::Value::FunctionParameter { .. } => todo!(),
            mir::Value::Op(op) => self.ops.remove(&op).unwrap(),
            mir::Value::Returned { .. } => todo!(),
            mir::Value::Num(n) => n.into(),
            mir::Value::String(s) => self.string_literals[&s].clone().into(),
            mir::Value::Bool(b) => b.to_string().into(),
        }
    }
}
