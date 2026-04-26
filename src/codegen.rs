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
        let mut target = project.add_sprite(file.source_slice(span));

        for costume in sprite.costume_lists().flat_map(ast::CostumeList::iter) {
            let name = &string_literals[&costume.name().span().low()];
            let path = string_literals[&costume.path().unwrap().span().low()].as_ref();
            target.add_costume(sb3::Costume::from_file(name, path)?);
        }

        let constant = |it: mir::Constant| match it {
            mir::Constant::Num(n) => sb3::Constant::Number(n),
            mir::Constant::String(s) => sb3_builder::Constant::String(&string_literals[&s]),
            mir::Constant::Bool(b) => sb3::Constant::String(if b { "true" } else { "false" }),
        };

        let variables = mir
            .variables
            .iter()
            .enumerate()
            .map(|(index, (id, &mir::Variable { value }))| {
                let name = format!("v{index}");
                let value = constant(value);
                (id, target.add_variable(sb3::Variable { name, value }))
            })
            .collect();

        let lists = mir
            .lists
            .iter()
            .enumerate()
            .map(|(index, (id, mir::List { items }))| {
                let name = format!("l{index}");
                let items = items.iter().map(|&it| constant(it)).collect();
                (id, target.add_list(sb3::List { name, items }))
            })
            .collect();

        let returns = mir
            .functions
            .iter()
            .filter_map(|(big, it)| Some(big).zip(it.return_value_count()))
            .enumerate()
            .map(|(big, (&function, return_value_count))| {
                let variables = (0..return_value_count).map(|little| {
                    let name = format!("r{big}.{little}");
                    let value = sb3::Constant::Number(0.0);
                    target.add_variable(sb3::Variable { name, value })
                });
                (function, variables.collect())
            })
            .collect();

        let (custom_blocks, points) = mir
            .functions
            .iter()
            .filter_map(|(&basic_block, &it)| match it {
                mir::Function::Normal {
                    name,
                    parameter_count,
                    ..
                } => {
                    let parameters = (0..parameter_count).map(|index| sb3::Parameter {
                        name: format!("{name}.{index}"),
                        kind: sb3::ParameterKind::StringOrNumber,
                    });
                    let (custom_block, point) =
                        target.add_custom_block(name.to_owned(), parameters);
                    Some(((basic_block, custom_block), (basic_block, point)))
                }
                _ => None,
            })
            .collect();

        let mut compiler = Compiler {
            target,
            ops: HashMap::new(),
            variables,
            lists,
            returns: &returns,
            custom_blocks: &custom_blocks,
            points,
            code_map,
            mir,
            string_literals,
        };

        for (&body, &it) in &mir.functions {
            compiler.function(it, body);
        }
    }
    project.finish(output_file)
}

struct Compiler<'src, 'project> {
    target: sb3::Target<'src, 'project>,
    ops: HashMap<Id<mir::Op>, sb3::Operand<'src>>,
    variables: HashMap<Id<mir::Variable>, sb3::VariableRef>,
    lists: HashMap<Id<mir::List>, sb3::ListRef>,
    returns: &'project HashMap<Id<mir::BasicBlock>, Vec<sb3::VariableRef>>,
    custom_blocks: &'project HashMap<Id<mir::BasicBlock>, sb3::CustomBlockRef>,
    points: HashMap<Id<mir::BasicBlock>, sb3::InsertionPoint>,
    code_map: &'src codemap::CodeMap,
    mir: &'src mir::Program<'src>,
    string_literals: &'src HashMap<codemap::Pos, String>,
}

impl<'src> Compiler<'src, '_> {
    fn function(&mut self, it: mir::Function, body: Id<mir::BasicBlock>) {
        match it {
            mir::Function::WhenFlagClicked => self.target.start_script(block::when_flag_clicked()),
            mir::Function::WhenKeyPressed { key } => self
                .target
                .start_script(block::when_key_pressed(&self.string_literals[&key])),
            mir::Function::WhenCloned => self.target.start_script(block::when_cloned()),
            mir::Function::WhenReceived { message } => self
                .target
                .start_script(block::when_received(&self.string_literals[&message])),
            mir::Function::Normal { .. } => {
                let _: sb3::InsertionPoint =
                    self.target.insert_at(self.points.remove(&body).unwrap());
            }
        }
        for &op in &self.mir.basic_blocks[body].0 {
            let res = self.op(&self.mir.ops[op], body);
            self.ops.extend(Some(op).zip(res));
        }
    }

    fn basic_block(&mut self, block: Id<mir::BasicBlock>, function: Id<mir::BasicBlock>) {
        for &op in &self.mir.basic_blocks[block].0 {
            let res = self.op(&self.mir.ops[op], function);
            self.ops.extend(Some(op).zip(res));
        }
    }

    fn op(&mut self, op: &mir::Op, function: Id<mir::BasicBlock>) -> Option<sb3::Operand<'src>> {
        match op {
            mir::Op::Load { source } => Some(match *source {
                mir::Ref::Variable(variable) => self.variables[&variable].into(),
                mir::Ref::List { list, index } => {
                    let index = self.value(index, function);
                    self.target.item_num_of_list(self.lists[&list], index)
                }
                mir::Ref::Last { list } => self
                    .target
                    .item_num_of_list(self.lists[&list], "last".into()),
            }),
            mir::Op::Store { target, value } => {
                let value = self.value(*value, function);
                let block = match *target {
                    mir::Ref::Variable(variable) => {
                        block::set_variable(self.variables[&variable], value)
                    }
                    mir::Ref::List { list, index } => {
                        block::replace(self.lists[&list], self.value(index, function), value)
                    }
                    mir::Ref::Last { list } => {
                        block::replace(self.lists[&list], "last".into(), value)
                    }
                };
                self.target.put(block);
                None
            }
            mir::Op::Repeat { times, body } => {
                let times = self.value(*times, function);
                let after = self.target.repeat(times);
                self.basic_block(*body, function);
                let _: sb3::InsertionPoint = self.target.insert_at(after);
                None
            }
            mir::Op::For {
                variable,
                times,
                body,
            } => {
                let times = self.value(*times, function);
                let after = self.target.for_(self.variables[variable], times);
                self.basic_block(*body, function);
                let _: sb3::InsertionPoint = self.target.insert_at(after);
                None
            }
            mir::Op::Forever(body) => {
                self.target.forever();
                self.basic_block(*body, function);
                None
            }
            mir::Op::If {
                condition,
                then,
                r#else,
            } => {
                let condition = self.value(*condition, function);
                let [else_point, after] = self.target.if_else(condition);
                self.basic_block(*then, function);
                let _: sb3::InsertionPoint = self.target.insert_at(else_point);
                self.basic_block(*r#else, function);
                let _: sb3::InsertionPoint = self.target.insert_at(after);
                None
            }
            mir::Op::Return { function, values } => {
                for (&variable, &value) in self.returns[function].iter().zip(values) {
                    let value = self.value(value, *function);
                    self.target.put(block::set_variable(variable, value));
                }
                self.target.put(block::stop_this_script());
                None
            }
            mir::Op::Call {
                function,
                arguments,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|&it| self.value(it, *function))
                    .collect();
                self.target
                    .use_custom_block(self.custom_blocks[function], arguments);
                None
            }
            mir::Op::Delete { list, index } => {
                let (list, index) = (self.lists[list], self.value(*index, function));
                self.target.put(block::delete_of_list(list, index));
                None
            }
            mir::Op::DeleteAll(list) => {
                self.target.put(block::delete_all_of_list(self.lists[list]));
                None
            }
            mir::Op::DeleteLast(list) => {
                self.target
                    .put(block::delete_of_list(self.lists[list], "last".into()));
                None
            }
            mir::Op::Push { list, value } => {
                let value = self.value(*value, function);
                self.target.put(block::append(self.lists[list], value));
                None
            }
            mir::Op::Length(list) => Some(self.target.length_of_list(self.lists[list])),
            mir::Op::Intrinsic { name, arguments } => {
                let arguments = arguments.iter().map(|&it| self.value(it, function));
                let arguments = arguments.collect();
                let name = self.code_map.find_file(name.low()).source_slice(*name);
                self.intrinsic(name, arguments)
            }
        }
    }

    fn value(&mut self, value: mir::Value, function: Id<mir::BasicBlock>) -> sb3::Operand<'src> {
        match value {
            mir::Value::FunctionParameter { index } => self
                .target
                .custom_block_parameter(self.custom_blocks[&function], index),
            mir::Value::Op(op) => self.ops.remove(&op).unwrap(),
            mir::Value::Returned { call, index } => {
                let mir::Op::Call { function, .. } = self.mir.ops[call] else {
                    unreachable!();
                };
                self.returns[&function][index].into()
            }
            mir::Value::Constant(mir::Constant::Num(n)) => n.into(),
            mir::Value::Constant(mir::Constant::String(s)) => (&*self.string_literals[&s]).into(),
            mir::Value::Constant(mir::Constant::Bool(b)) => if b { "true" } else { "false" }.into(),
        }
    }

    fn intrinsic(
        &mut self,
        name: &str,
        mut arguments: Vec<sb3::Operand<'src>>,
    ) -> Option<sb3::Operand<'src>> {
        macro_rules! f {
        ($block:ident($($param:ident),*)) => {
            {
                let [$($param),*] = arguments.try_into().ok().unwrap();
                self.target.put(block::$block($($param),*));
                None
            }
        };

        (= $block:ident($($param:ident),*)) => {
            {
                let [$($param),*] = arguments.try_into().ok().unwrap();
                Some(self.target.$block($($param),*))
            }
        };
    }

        match name {
            "to-string" | "to-num" => arguments.pop(),
            "length" => f! { = length(string) },
            "letter" => f! { = letter_of(string, index) },
            "+" => f! { = add(lhs, rhs) },
            "-" => f! { = sub(lhs, rhs) },
            "*" => f! { = mul(lhs, rhs) },
            "/" => f! { = div(lhs, rhs) },
            "%" => f! { = modulo(lhs, rhs) },
            "<" => f! { = lt(lhs, rhs) },
            "==" => f! { = eq(lhs, rhs) },
            ">" => f! { = gt(lhs, rhs) },
            "abs" => Some(self.mathop("abs", arguments)),
            "floor" => Some(self.mathop("floor", arguments)),
            "ceil" => Some(self.mathop("ceiling", arguments)),
            "sqrt" => Some(self.mathop("sqrt", arguments)),
            "ln" => Some(self.mathop("ln", arguments)),
            "log" => Some(self.mathop("log", arguments)),
            "exp" => Some(self.mathop("e ^", arguments)),
            "exp-10" => Some(self.mathop("10 ^", arguments)),
            "sin" => Some(self.mathop("sin", arguments)),
            "cos" => Some(self.mathop("cos", arguments)),
            "tan" => Some(self.mathop("tan", arguments)),
            "asin" => Some(self.mathop("asin", arguments)),
            "acos" => Some(self.mathop("acos", arguments)),
            "atan" => Some(self.mathop("atan", arguments)),
            "not" => {
                let [operand] = arguments.try_into().ok().unwrap();
                Some(self.target.not(operand))
            }
            "and" => {
                let [lhs, rhs] = arguments.try_into().ok().unwrap();
                Some(self.target.and(lhs, rhs))
            }
            "or" => {
                let [lhs, rhs] = arguments.try_into().ok().unwrap();
                Some(self.target.or(lhs, rhs))
            }
            "join" => f! { = join(lhs, rhs) },
            "answer" => f! { = answer() },
            "ask" => f! { ask(question) },
            "broadcast-and-wait" => f! { broadcast_and_wait(message) },
            "change-x" => f! { change_x(amount) },
            "change-y" => f! { change_y(amount) },
            "clone-self" => {
                self.target.clone_self();
                None
            }
            "erase-all" => f! { erase_all() },
            "go-to" => f! { go_to_xy(x, y) },
            "go-to-back-layer" => f! { go_to_back_layer() },
            "go-to-front-layer" => f! { go_to_front_layer() },
            "hide" => f! { hide() },
            "move" => f! { move_steps(steps) },
            "mouse-x" => f! { = mouse_x() },
            "mouse-y" => f! { = mouse_y() },
            "pen-down" => f! { pen_down() },
            "pen-up" => f! { pen_up() },
            "pressing-key" => f! { = key_is_pressed(key) },
            "random" => f! { = random(low, high) },
            "reset-timer" => f! { reset_timer() },
            "say" if arguments.len() == 1 => f! { say(message) },
            "say" => f! { say_for_seconds(seconds, message) },
            "set-costume" => f! { set_costume(costume) },
            "set-pen-color" => f! { set_pen_color(color) },
            "set-pen-size" => f! { set_pen_size(size) },
            "set-size" => f! { set_size(size) },
            "set-x" => f! { set_x(x) },
            "set-y" => f! { set_y(y) },
            "show" => f! { show() },
            "stamp" => f! { stamp() },
            "stop-all" => f! { stop_all() },
            "timer" => f! { = timer() },
            "wait" => f! { wait(seconds) },
            "x-pos" => f! { = x_position() },
            "y-pos" => f! { = y_position() },
            _ => panic!("invalid intrinsic: {name:?}"),
        }
    }

    fn mathop(
        &mut self,
        operator: &'static str,
        arguments: Vec<sb3::Operand<'src>>,
    ) -> sb3::Operand<'src> {
        let [operand] = arguments.try_into().ok().unwrap();
        self.target.mathop(operator, operand)
    }
}
