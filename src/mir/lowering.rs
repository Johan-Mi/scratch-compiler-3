use crate::ast::{self, Node};
use crate::mir;
use map::Id;
use std::collections::HashMap;

pub fn lower(
    ast: ast::Program,
    code_map: &codemap::CodeMap,
    resolved_variables: &HashMap<codemap::Pos, codemap::Pos>,
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
        functions,
        variables: HashMap::new(),
    };

    for function in function_asts() {
        let body = function.body().unwrap();
        let basic_block = context.functions[&function.syntax().span().low()];
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
    functions: HashMap<codemap::Pos, Id<mir::BasicBlock>>,
    variables: HashMap<codemap::Pos, Vec<Id<mir::Variable>>>,
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
            let values = lower_expression(it.value().unwrap(), basic_block, c);
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
            let condition = one(lower_expression(it.condition().unwrap(), basic_block, c));
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
            let times = one(lower_expression(it.times().unwrap(), basic_block, c));
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
            let times = one(lower_expression(it.times().unwrap(), basic_block, c));
            let body = lower_block(it.body().unwrap(), c);
            let op = c.program.ops.insert(mir::Op::For {
                variable,
                times,
                body,
            });
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Return(_) => todo!(),
        ast::Statement::Expression(it) => assert!(lower_expression(it, basic_block, c).is_empty()),
    }
}

fn lower_expression(
    expression: ast::Expression,
    basic_block: Id<mir::BasicBlock>,
    c: &mut Context,
) -> Vec<mir::Value> {
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
                .collect()
        }
        ast::Expression::FunctionCall(_) => lower_call(c),
        ast::Expression::BinaryOperation(_) => lower_call(c),
        ast::Expression::NamedArgument(_) => todo!(),
        ast::Expression::DecimalNumber(it) => {
            let span = it.syntax().span();
            let file = c.code_map.find_file(span.low());
            [mir::Value::Num(file.source_slice(span).parse().unwrap())].into()
        }
        ast::Expression::BinaryNumber(it) => float(2, b'b', it.syntax(), c.code_map),
        ast::Expression::OctalNumber(it) => float(8, b'o', it.syntax(), c.code_map),
        ast::Expression::HexadecimalNumber(it) => float(16, b'x', it.syntax(), c.code_map),
        ast::Expression::String(it) => [mir::Value::String(it.syntax().span().low())].into(),
        ast::Expression::KwFalse(_) => [mir::Value::Bool(false)].into(),
        ast::Expression::KwTrue(_) => [mir::Value::Bool(true)].into(),
        ast::Expression::Lvalue(_) => todo!(),
        ast::Expression::ListLiteral(_) => todo!(),
        ast::Expression::TypeAscription(it) => {
            lower_expression(it.inner().unwrap(), basic_block, c)
        }
        ast::Expression::MethodCall(_) => lower_call(c),
        ast::Expression::FieldAccess(_) => todo!(),
    }
}

fn float(
    radix: u32,
    letter: u8,
    node: crate::parser::SyntaxNode,
    code_map: &codemap::CodeMap,
) -> Vec<mir::Value> {
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
    [mir::Value::Num(number)].into()
}

fn lower_call(c: &mut Context) -> Vec<mir::Value> {
    todo!()
}

fn one(values: Vec<mir::Value>) -> mir::Value {
    <[_; 1]>::try_from(values).unwrap_or_else(|_| unreachable!())[0]
}
