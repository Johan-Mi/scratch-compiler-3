use crate::ast::{self, Node};
use crate::mir;
use crate::ty::{self, Type};
use map::Id;
use std::{collections::HashMap, ops::Range};

pub fn lower(
    ast: ast::Program,
    code_map: &codemap::CodeMap,
    resolved_variables: &HashMap<codemap::Pos, codemap::Pos>,
    expression_types: &HashMap<codemap::Span, Type>,
    resolved_calls: &HashMap<codemap::Span, ast::FunctionLike>,
    return_types: &HashMap<codemap::Pos, Type>,
    layouts: &HashMap<codemap::Pos, Vec<Range<usize>>>,
) -> mir::Program {
    let function_asts = || {
        ast.documents().flat_map(ast::Document::functions).chain(
            ast.documents()
                .flat_map(ast::Document::sprites)
                .flat_map(ast::Sprite::functions),
        )
    };

    let mut program = mir::Program::default();
    let functions = function_asts()
        .map(|it| it.syntax().span().low())
        .zip(std::iter::repeat_with(|| {
            program.basic_blocks.insert(mir::BasicBlock(Vec::new()))
        }))
        .collect();
    let mut context = Context {
        program,
        code_map,
        resolved_variables,
        expression_types,
        resolved_calls,
        layouts,
        functions,
        variables: HashMap::new(),
        current_function: None,
    };

    for function in function_asts() {
        let body = function.body().unwrap();
        let pos = function.syntax().span().low();
        let basic_block = context.functions[&pos];
        let return_value_count = crate::ty::layout::size(return_types[&pos].base, layouts);
        assert!(
            context
                .program
                .functions
                .insert(basic_block, mir::Function { return_value_count })
                .is_none()
        );
        context.current_function = Some(basic_block);
        for statement in body.statements() {
            lower_statement(statement, basic_block, &mut context);
        }
    }

    context.program
}

struct Context<'src> {
    program: mir::Program,
    code_map: &'src codemap::CodeMap,
    resolved_variables: &'src HashMap<codemap::Pos, codemap::Pos>,
    expression_types: &'src HashMap<codemap::Span, Type<'src>>,
    resolved_calls: &'src HashMap<codemap::Span, ast::FunctionLike<'src>>,
    layouts: &'src HashMap<codemap::Pos, Vec<Range<usize>>>,
    functions: HashMap<codemap::Pos, Id<mir::BasicBlock>>,
    variables: HashMap<codemap::Pos, Vec<Id<mir::Variable>>>,
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
            let variables = std::iter::repeat_with(|| c.program.variables.insert(mir::Variable))
                .take(values.len())
                .collect();
            let stores = std::iter::zip(&variables, values).map(|(&variable, value)| {
                c.program.ops.insert(mir::Op::Store {
                    target: mir::Ref::Variable(variable),
                    value,
                })
            });
            c.program.basic_blocks[basic_block].0.extend(stores);
            let pos = it.variable().unwrap().span().low();
            assert!(c.variables.insert(pos, variables).is_none());
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
            let variable = c.program.variables.insert(mir::Variable);
            let pos = it.variable().unwrap().span().low();
            assert!(c.variables.insert(pos, [variable].into()).is_none());
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
            let op = c.program.ops.insert(mir::Op::Return {
                function: c.current_function.unwrap(),
                values,
            });
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
                c.variables[&c.resolved_variables[&it.syntax().span().low()]]
                    .iter()
                    .map(|&variable| mir::Ref::Variable(variable))
                    .map(|source| c.program.ops.insert(mir::Op::Load { source })),
            );
            basic_block[start..]
                .iter()
                .map(|&variable| mir::Value::Op(variable))
                .collect::<Vec<_>>()
                .into()
        }
        ast::Expression::FunctionCall(it) => {
            lower_call(it.name().span(), &mut it.args().iter(), basic_block, c)
        }
        ast::Expression::BinaryOperation(it) => lower_call(
            it.operator().span(),
            &mut [it.lhs().unwrap(), it.rhs().unwrap()].into_iter(),
            basic_block,
            c,
        ),
        ast::Expression::NamedArgument(it) => lower_expression(it.value().unwrap(), basic_block, c),
        ast::Expression::DecimalNumber(it) => {
            let span = it.syntax().span();
            let file = c.code_map.find_file(span.low());
            Vec::from([mir::Value::Num(file.source_slice(span).parse().unwrap())]).into()
        }
        ast::Expression::BinaryNumber(it) => float(2, b'b', it.syntax(), c.code_map),
        ast::Expression::OctalNumber(it) => float(8, b'o', it.syntax(), c.code_map),
        ast::Expression::HexadecimalNumber(it) => float(16, b'x', it.syntax(), c.code_map),
        ast::Expression::String(it) => {
            Vec::from([mir::Value::String(it.syntax().span().low())]).into()
        }
        ast::Expression::KwFalse(_) => Vec::from([mir::Value::Bool(false)]).into(),
        ast::Expression::KwTrue(_) => Vec::from([mir::Value::Bool(true)]).into(),
        ast::Expression::Lvalue(it) => lower_lvalue(it.inner().unwrap(), c),
        ast::Expression::ListLiteral(it) => {
            let base = c.expression_types[&it.syntax().span()].base;
            let size = crate::ty::layout::size(base, c.layouts);

            let lists = std::iter::repeat_with(|| c.program.lists.insert(mir::List))
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
        ast::Expression::MethodCall(it) => lower_call(
            it.name().span(),
            &mut std::iter::once(it.caller()).chain(it.arguments().iter()),
            basic_block,
            c,
        ),
        ast::Expression::FieldAccess(it) => {
            let mut values = lower_expression(it.aggregate(), basic_block, c).values();
            let ty = c.expression_types[&it.syntax().span()];
            assert!(matches!(ty.shape, ty::Shape::Flat));
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
                .position(|field| file.source_slice(field.internal_name().span()) == field_name)
                .unwrap();
            let range = c.layouts[&ty.syntax().span().low()][field_index].clone();
            values.drain(range).collect::<Vec<_>>().into()
        }
    }
}

fn float(
    radix: u32,
    letter: u8,
    node: crate::parser::SyntaxNode,
    code_map: &codemap::CodeMap,
) -> Bundle {
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
    Vec::from([mir::Value::Num(number)]).into()
}

fn lower_lvalue(expression: ast::Expression, c: &mut Context) -> Bundle {
    todo!()
}

fn lower_call(
    name: codemap::Span,
    arguments: &mut dyn Iterator<Item = ast::Expression>,
    basic_block: Id<mir::BasicBlock>,
    c: &mut Context,
) -> Bundle {
    let arguments: Vec<_> = arguments
        .flat_map(|it| lower_expression(it, basic_block, c).values())
        .collect();

    let function_like: ast::FunctionLike = c.resolved_calls[&name];
    let Some(function) = ast::Function::cast(function_like.syntax()) else {
        assert!(ast::Struct::cast(function_like.syntax()).is_some());
        return arguments.into();
    };

    if function.kw_inline().is_some() {
        todo!("lower inline functions to MIR");
    }

    let function = c.functions[&function.syntax().span().low()];
    let call = c.program.ops.insert(mir::Op::Call {
        function,
        arguments,
    });
    c.program.basic_blocks[basic_block].0.push(call);

    (0..c.program.functions[&function].return_value_count)
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
}

fn one(values: Vec<mir::Value>) -> mir::Value {
    <[_; 1]>::try_from(values).unwrap_or_else(|_| unreachable!())[0]
}
