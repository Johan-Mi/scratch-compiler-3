use crate::ast::{self, Node};
use crate::parser::K;
use crate::{mir, name, ty};
use map::Id;
use std::{collections::HashMap, ops::Range};

pub fn lower<'src>(
    ast: ast::Program,
    code_map: &'src codemap::CodeMap,
    resolved_variables: &name::S,
    typing: &ty::Ping<'src>,
    layouts: &ty::layout::S,
) -> mir::Program<'src> {
    let function_asts = || {
        ast.documents().flat_map(ast::Document::functions).chain(
            ast.documents()
                .flat_map(ast::Document::sprites)
                .flat_map(ast::Sprite::functions),
        )
    };

    let mut program = mir::Program::default();
    let functions = function_asts()
        .map(ast::Function::unmanaged)
        .zip(std::iter::repeat_with(|| {
            program.basic_blocks.insert(mir::BasicBlock(Vec::new()))
        }))
        .collect();
    let mut context = Context {
        program,
        code_map,
        resolved_variables,
        typing,
        layouts,
        functions,
        variables: HashMap::new(),
        current_function: None,
    };

    for function in function_asts() {
        let body = function.body().unwrap();
        let pos = function.syntax().span().low();
        let basic_block = context.functions[&function.unmanaged()];
        let file = code_map.find_file(pos);
        let name = file.source_slice(function.name().unwrap().span());
        let function = match name {
            "when-flag-clicked" => mir::Function::WhenFlagClicked,
            "when-key-pressed" => mir::Function::WhenKeyPressed {
                key: function.tag().unwrap().span().low(),
            },
            "when-cloned" => mir::Function::WhenCloned,
            "when-received" => mir::Function::WhenReceived {
                message: function.tag().unwrap().span().low(),
            },
            _ => mir::Function::Normal {
                name,
                parameter_count: typing.parameter_types[&function.unmanaged()]
                    .iter()
                    .map(|it| ty::layout::size(it.base, layouts))
                    .sum(),
                return_value_count: ty::layout::size(
                    typing.return_types[&function.unmanaged()].base,
                    layouts,
                ),
            },
        };
        assert!(
            context
                .program
                .functions
                .insert(basic_block, function)
                .is_none()
        );
        context.current_function = Some(basic_block);
        for statement in body.statements() {
            lower_statement(statement, basic_block, &mut context);
        }
    }

    context.program
}

struct Context<'src, 'lower> {
    program: mir::Program<'src>,
    code_map: &'lower codemap::CodeMap,
    resolved_variables: &'lower name::S,
    typing: &'lower ty::Ping<'src>,
    layouts: &'lower ty::layout::S,
    functions: HashMap<ast::FunctionUnmanaged, Id<mir::BasicBlock>>,
    variables: HashMap<ast::VariableDefinitionUnmanaged, Vec<Id<mir::Variable>>>,
    current_function: Option<Id<mir::BasicBlock>>,
}

fn lower_block(block: ast::Block, c: &mut Context) -> Id<mir::BasicBlock> {
    let basic_block = c.program.basic_blocks.insert(mir::BasicBlock(Vec::new()));
    for statement in block.statements() {
        lower_statement(statement, basic_block, c);
    }
    basic_block
}

fn lower_statement(statement: ast::Statement, basic_block: Id<mir::BasicBlock>, c: &mut Context) {
    match statement {
        ast::Statement::Let(it) => {
            let values = lower_expression(it.value().unwrap(), basic_block, c).values();
            let variables = std::iter::repeat_with(|| {
                let value = mir::Constant::PLACEHOLDER;
                c.program.variables.insert(mir::Variable { value })
            })
            .take(values.len())
            .collect();
            let stores = std::iter::zip(&variables, values).map(|(&variable, value)| {
                c.program.ops.insert(mir::Op::Store {
                    target: mir::Ref::Variable(variable),
                    value,
                })
            });
            c.program.basic_blocks[basic_block].0.extend(stores);
            let node = it.variable().unwrap().unmanaged();
            assert!(c.variables.insert(node, variables).is_none());
        }
        ast::Statement::If(it) => {
            let condition = one(lower_expression(it.condition().unwrap(), basic_block, c).values());
            let then = lower_block(it.then().unwrap(), c);
            let r#else = c.program.basic_blocks.insert(mir::BasicBlock(Vec::new()));
            if let Some(else_clause) = it.else_clause() {
                if let Some(else_if) = else_clause.if_() {
                    lower_statement(ast::Statement::If(else_if), r#else, c);
                } else {
                    for statement in else_clause.block().unwrap().statements() {
                        lower_statement(statement, r#else, c);
                    }
                }
            }
            let op = c.program.ops.insert(mir::Op::If {
                condition,
                then,
                r#else,
            });
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Repeat(it) => {
            let times = one(lower_expression(it.times().unwrap(), basic_block, c).values());
            let body = lower_block(it.body().unwrap(), c);
            let op = c.program.ops.insert(mir::Op::Repeat { times, body });
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Forever(it) => {
            let body = lower_block(it.body().unwrap(), c);
            let op = c.program.ops.insert(mir::Op::Forever(body));
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::While(_) => todo!(),
        ast::Statement::Until(_) => todo!(),
        ast::Statement::For(it) => {
            let value = mir::Constant::Num(0.0);
            let variable = c.program.variables.insert(mir::Variable { value });
            let node = it.variable().unwrap().unmanaged();
            assert!(c.variables.insert(node, [variable].into()).is_none());
            let times = one(lower_expression(it.times().unwrap(), basic_block, c).values());
            let body = lower_block(it.body().unwrap(), c);
            let op = c.program.ops.insert(mir::Op::For {
                variable,
                times,
                body,
            });
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Return(it) => {
            let values = lower_expression(it.expression().unwrap(), basic_block, c).values();
            let function = c.current_function.unwrap();
            let op = c.program.ops.insert(mir::Op::Return { function, values });
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Expression(it) => {
            assert!(lower_expression(it, basic_block, c).values().is_empty());
        }
    }
}

fn lower_expression(
    expression: ast::Expression,
    basic_block: Id<mir::BasicBlock>,
    c: &mut Context,
) -> Bundle {
    match expression {
        ast::Expression::Parenthesized(it) => lower_expression(it.inner().unwrap(), basic_block, c),
        ast::Expression::Variable(it) => {
            let basic_block = &mut c.program.basic_blocks[basic_block].0;
            let start = basic_block.len();
            basic_block.extend(
                c.variables[&c.resolved_variables[&it.unmanaged()]]
                    .iter()
                    .map(|&variable| mir::Ref::Variable(variable))
                    .map(|source| c.program.ops.insert(mir::Op::Load { source })),
            );
            let ops = basic_block[start..].iter().map(|&it| mir::Value::Op(it));
            ops.collect::<Vec<_>>().into()
        }
        ast::Expression::FunctionCall(it) => {
            lower_call(expression, &mut it.args().iter(), basic_block, c)
        }
        ast::Expression::BinaryOperation(it) => {
            let mut arguments = [it.lhs().unwrap(), it.rhs().unwrap()].into_iter();
            lower_call(expression, &mut arguments, basic_block, c)
        }
        ast::Expression::Index(it) => {
            let lists = lower_lvalue(it.lhs().unwrap(), basic_block, c).lists();
            let index = one(lower_expression(it.rhs().unwrap(), basic_block, c).values());
            let basic_block = &mut c.program.basic_blocks[basic_block].0;
            let start = basic_block.len();
            basic_block.extend(lists.iter().map(|&list| {
                let source = mir::Ref::List { list, index };
                c.program.ops.insert(mir::Op::Load { source })
            }));
            let ops = basic_block[start..].iter().map(|&it| mir::Value::Op(it));
            ops.collect::<Vec<_>>().into()
        }
        ast::Expression::NamedArgument(it) => lower_expression(it.value().unwrap(), basic_block, c),
        ast::Expression::DecimalNumber(it) => {
            let span = it.syntax().span();
            let file = c.code_map.find_file(span.low());
            let number = file.source_slice(span).parse().unwrap();
            Vec::from([mir::Value::Constant(mir::Constant::Num(number))]).into()
        }
        ast::Expression::BinaryNumber(it) => float(2, b'b', it.syntax(), c.code_map),
        ast::Expression::OctalNumber(it) => float(8, b'o', it.syntax(), c.code_map),
        ast::Expression::HexadecimalNumber(it) => float(16, b'x', it.syntax(), c.code_map),
        ast::Expression::String(it) => Vec::from([mir::Value::Constant(mir::Constant::String(
            it.syntax().span().low(),
        ))])
        .into(),
        ast::Expression::KwFalse(_) => {
            Vec::from([mir::Value::Constant(mir::Constant::Bool(false))]).into()
        }
        ast::Expression::KwTrue(_) => {
            Vec::from([mir::Value::Constant(mir::Constant::Bool(true))]).into()
        }
        ast::Expression::Lvalue(it) => lower_lvalue(it.inner().unwrap(), basic_block, c),
        ast::Expression::ListLiteral(it) => {
            let base = c.typing.expression_types[&expression.unmanaged()].base;
            let size = ty::layout::size(base, c.layouts);

            let lists = std::iter::repeat_with(|| c.program.lists.insert(mir::List::default()))
                .take(size)
                .collect::<Vec<_>>();
            c.program.basic_blocks[basic_block].0.extend(
                lists
                    .iter()
                    .map(|&it| c.program.ops.insert(mir::Op::DeleteAll(it))),
            );
            for item in it.iter() {
                let values = lower_expression(item, basic_block, c).values();
                c.program.basic_blocks[basic_block].0.extend(
                    std::iter::zip(&lists, values)
                        .map(|(&list, value)| c.program.ops.insert(mir::Op::Push { list, value })),
                );
            }

            Bundle::Lists(lists)
        }
        ast::Expression::TypeAscription(it) => {
            lower_expression(it.inner().unwrap(), basic_block, c)
        }
        ast::Expression::MethodCall(it) => {
            let mut arguments = std::iter::once(it.caller()).chain(it.arguments().iter());
            lower_call(expression, &mut arguments, basic_block, c)
        }
        ast::Expression::FieldAccess(it) => lower_field_access(it, basic_block, c),
    }
}

fn float(radix: u32, letter: u8, node: cst::Node<K>, code_map: &codemap::CodeMap) -> Bundle {
    let span = node.span();
    let file = code_map.find_file(span.low());
    let text = file.source_slice(span);
    let (sign, text) = match text.as_bytes() {
        [b'+', b'0', l, ..] if l.eq_ignore_ascii_case(&letter) => (1.0, &text[3..]),
        [b'-', b'0', l, ..] if l.eq_ignore_ascii_case(&letter) => (-1.0, &text[3..]),
        _ => (1.0, &text[2..]),
    };
    #[expect(
        clippy::cast_precision_loss,
        reason = "These are float literals. `u64` is only used as an implementation detail."
    )]
    let number = u64::from_str_radix(text, radix).unwrap() as f64 * sign;
    Vec::from([mir::Value::Constant(mir::Constant::Num(number))]).into()
}

fn lower_field_access(
    it: ast::FieldAccess,
    basic_block: Id<mir::BasicBlock>,
    c: &mut Context,
) -> Bundle {
    let mut values = lower_expression(it.aggregate(), basic_block, c).values();
    let ty = c.typing.expression_types[&ast::Expression::FieldAccess(it).unmanaged()];
    assert_eq!(ty::Shape::Flat, ty.shape);
    let ty::Base::Struct(ty) = ty.base else {
        unreachable!();
    };
    let field_name = c
        .code_map
        .find_file(it.field().span().low())
        .source_slice(it.field().span());
    let file = c.code_map.find_file(ty.syntax().span().low());
    let field_index: usize = ty
        .parameters()
        .unwrap()
        .iter()
        .position(|field| file.source_slice(field.internal_name().syntax().span()) == field_name)
        .unwrap();
    let range = copy_range(&c.layouts[&ty.unmanaged()][field_index]);
    values.drain(range).collect::<Vec<_>>().into()
}

fn lower_lvalue(
    expression: ast::Expression,
    basic_block: Id<mir::BasicBlock>,
    c: &mut Context,
) -> Bundle {
    match expression {
        ast::Expression::Parenthesized(_)
        | ast::Expression::FunctionCall(_)
        | ast::Expression::BinaryOperation(_)
        | ast::Expression::NamedArgument(_)
        | ast::Expression::DecimalNumber(_)
        | ast::Expression::BinaryNumber(_)
        | ast::Expression::OctalNumber(_)
        | ast::Expression::HexadecimalNumber(_)
        | ast::Expression::String(_)
        | ast::Expression::KwFalse(_)
        | ast::Expression::KwTrue(_)
        | ast::Expression::Lvalue(_)
        | ast::Expression::ListLiteral(_)
        | ast::Expression::TypeAscription(_)
        | ast::Expression::MethodCall(_) => unreachable!(),

        ast::Expression::Variable(it) => Bundle::Refs(
            c.variables[&c.resolved_variables[&it.unmanaged()]]
                .iter()
                .map(|&it| mir::Ref::Variable(it))
                .collect(),
        ),
        ast::Expression::Index(it) => {
            let lists = lower_lvalue(it.lhs().unwrap(), basic_block, c).lists();
            let index = one(lower_expression(it.rhs().unwrap(), basic_block, c).values());
            let refs = lists.iter().map(|&list| mir::Ref::List { list, index });
            Bundle::Refs(refs.collect())
        }
        ast::Expression::FieldAccess(it) => {
            let mut refs = lower_lvalue(it.aggregate(), basic_block, c).refs();
            let ty = c.typing.expression_types[&expression.unmanaged()];
            assert_eq!(ty::Shape::Flat, ty.shape);
            let ty::Base::Struct(ty) = ty.base else {
                unreachable!();
            };
            let field_name = c
                .code_map
                .find_file(it.field().span().low())
                .source_slice(it.field().span());
            let file = c.code_map.find_file(ty.syntax().span().low());
            let field_index: usize = ty
                .parameters()
                .unwrap()
                .iter()
                .position(|field| {
                    file.source_slice(field.internal_name().syntax().span()) == field_name
                })
                .unwrap();
            let range = copy_range(&c.layouts[&ty.unmanaged()][field_index]);
            Bundle::Refs(refs.drain(range).collect())
        }
    }
}

fn lower_call(
    expression: ast::Expression,
    arguments: &mut dyn Iterator<Item = ast::Expression>,
    basic_block: Id<mir::BasicBlock>,
    c: &mut Context,
) -> Bundle {
    let arguments: Vec<_> = arguments
        .flat_map(|it| lower_expression(it, basic_block, c).values())
        .collect();

    let function_like: ast::FunctionLike = c.typing.resolved_calls[&expression.unmanaged()];
    let Some(function) = ast::Function::cast(function_like.syntax()) else {
        assert!(ast::Struct::cast(function_like.syntax()).is_some());
        return arguments.into();
    };

    if function.kw_inline().is_some() {
        todo!("lower inline functions to MIR");
    }

    if function.body().is_none() {
        let name = function.name().unwrap().unmanaged();
        let op = c.program.ops.insert(mir::Op::Intrinsic { name, arguments });
        c.program.basic_blocks[basic_block].0.push(op);
        return (c.typing.return_types[&function.unmanaged()] != ty::Base::Unit)
            .then_some(mir::Value::Op(op))
            .into_iter()
            .collect::<Vec<_>>()
            .into();
    }

    let function = c.functions[&function.unmanaged()];
    let call = c.program.ops.insert(mir::Op::Call {
        function,
        arguments,
    });
    c.program.basic_blocks[basic_block].0.push(call);

    (0..c.program.functions[&function]
        .return_value_count()
        .unwrap_or_default())
        .map(|index| mir::Value::Returned { call, index })
        .collect::<Vec<_>>()
        .into()
}

enum Bundle {
    Values(Vec<mir::Value>),
    Lists(Vec<Id<mir::List>>),
    Refs(Vec<mir::Ref>),
}

impl From<Vec<mir::Value>> for Bundle {
    fn from(v: Vec<mir::Value>) -> Self {
        Self::Values(v)
    }
}

impl Bundle {
    fn values(self) -> Vec<mir::Value> {
        match self {
            Self::Values(it) => it,
            _ => unreachable!(),
        }
    }

    fn lists(self) -> Vec<Id<mir::List>> {
        match self {
            Self::Lists(it) => it,
            _ => unreachable!(),
        }
    }

    fn refs(self) -> Vec<mir::Ref> {
        match self {
            Self::Refs(it) => it,
            _ => unreachable!(),
        }
    }
}

fn one(values: Vec<mir::Value>) -> mir::Value {
    <[_; 1]>::try_from(values).unwrap_or_else(|_| unreachable!())[0]
}

const fn copy_range(range: &Range<usize>) -> Range<usize> {
    range.start..range.end
}
