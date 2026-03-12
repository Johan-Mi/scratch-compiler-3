use crate::ast::{self, Node};
use crate::{mir, parser::K};
use map::{Id, Map};

pub fn lower(documents: &[cst::Tree<K>]) -> mir::Program {
    let mut program = mir::Program {
        parameters: Map::default(),
        basic_blocks: Map::default(),
        ops: Map::default(),
        variables: Map::default(),
        lists: Map::default(),
        returns: Map::default(),
    };

    for function_body in documents
        .iter()
        .flat_map(|it| ast::Document::cast(it.root()).unwrap().functions())
        .chain(
            documents
                .iter()
                .flat_map(|it| ast::Document::cast(it.root()).unwrap().sprites())
                .flat_map(ast::Sprite::functions),
        )
        .filter_map(ast::Function::body)
    {
        lower_block(function_body, &mut program);
    }

    program
}

fn lower_block(block: ast::Block, program: &mut mir::Program) -> Id<mir::BasicBlock> {
    let basic_block = program.basic_blocks.insert(mir::BasicBlock(Vec::new()));
    for statement in block.statements() {
        lower_statement(statement, basic_block, program);
    }
    basic_block
}

fn lower_statement(
    statement: ast::Statement,
    basic_block: Id<mir::BasicBlock>,
    program: &mut mir::Program,
) {
    match statement {
        ast::Statement::Let(_) => todo!(),
        ast::Statement::If(it) => {
            let condition = one(lower_expression(it.condition().unwrap(), program));
            let then = lower_block(it.then().unwrap(), program);
            let r#else = if let Some(else_clause) = it.else_clause() {
                if let Some(else_if) = else_clause.if_() {
                    let else_block = program.basic_blocks.insert(mir::BasicBlock(Vec::new()));
                    lower_statement(ast::Statement::If(else_if), else_block, program);
                    else_block
                } else {
                    lower_block(else_clause.block().unwrap(), program)
                }
            } else {
                program.basic_blocks.insert(mir::BasicBlock(Vec::new()))
            };
            let op = program.ops.insert(mir::Op::If {
                condition,
                then,
                r#else,
            });
            program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Repeat(it) => {
            let times = one(lower_expression(it.times().unwrap(), program));
            let body = lower_block(it.body().unwrap(), program);
            let op = program.ops.insert(mir::Op::Repeat { times, body });
            program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Forever(it) => {
            let body = lower_block(it.body().unwrap(), program);
            let op = program.ops.insert(mir::Op::Forever(body));
            program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::While(_) => todo!(),
        ast::Statement::Until(_) => todo!(),
        ast::Statement::For(_) => todo!(),
        ast::Statement::Return(_) => todo!(),
        ast::Statement::Expression(it) => assert!(lower_expression(it, program).is_empty()),
    }
}

fn lower_expression(expression: ast::Expression, program: &mut mir::Program) -> Vec<mir::Value> {
    match expression {
        ast::Expression::Parenthesized(it) => lower_expression(it.inner().unwrap(), program),
        ast::Expression::Variable(_) => todo!(),
        ast::Expression::FunctionCall(_) => lower_call(program),
        ast::Expression::BinaryOperation(_) => lower_call(program),
        ast::Expression::NamedArgument(_) => todo!(),
        ast::Expression::DecimalNumber(_) => todo!(),
        ast::Expression::BinaryNumber(_) => todo!(),
        ast::Expression::OctalNumber(_) => todo!(),
        ast::Expression::HexadecimalNumber(_) => todo!(),
        ast::Expression::String(_) => todo!(),
        ast::Expression::KwFalse(_) => [mir::Value::Bool(false)].into(),
        ast::Expression::KwTrue(_) => [mir::Value::Bool(true)].into(),
        ast::Expression::Lvalue(_) => todo!(),
        ast::Expression::ListLiteral(_) => todo!(),
        ast::Expression::TypeAscription(it) => lower_expression(it.inner().unwrap(), program),
        ast::Expression::MethodCall(_) => lower_call(program),
        ast::Expression::FieldAccess(_) => todo!(),
    }
}

fn lower_call(program: &mut mir::Program) -> Vec<mir::Value> {
    todo!()
}

fn one(values: Vec<mir::Value>) -> mir::Value {
    <[_; 1]>::try_from(values).unwrap_or_else(|_| unreachable!())[0]
}
