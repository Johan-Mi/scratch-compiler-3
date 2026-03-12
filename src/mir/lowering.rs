use crate::ast::{self, Node};
use crate::{mir, parser::K};
use map::{Id, Map};

pub fn lower(documents: &[cst::Tree<K>], code_map: &codemap::CodeMap) -> mir::Program {
    let program = mir::Program {
        parameters: Map::default(),
        basic_blocks: Map::default(),
        ops: Map::default(),
        variables: Map::default(),
        lists: Map::default(),
        returns: Map::default(),
    };

    let mut context = Context { program, code_map };

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
        lower_block(function_body, &mut context);
    }

    context.program
}

struct Context<'src> {
    program: mir::Program,
    code_map: &'src codemap::CodeMap,
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
        ast::Statement::Let(_) => todo!(),
        ast::Statement::If(it) => {
            let condition = one(lower_expression(it.condition().unwrap(), c));
            let then = lower_block(it.then().unwrap(), c);
            let r#else = if let Some(else_clause) = it.else_clause() {
                if let Some(else_if) = else_clause.if_() {
                    let else_block = c.program.basic_blocks.insert(mir::BasicBlock(Vec::new()));
                    lower_statement(ast::Statement::If(else_if), else_block, c);
                    else_block
                } else {
                    lower_block(else_clause.block().unwrap(), c)
                }
            } else {
                c.program.basic_blocks.insert(mir::BasicBlock(Vec::new()))
            };
            let op = c.program.ops.insert(mir::Op::If {
                condition,
                then,
                r#else,
            });
            c.program.basic_blocks[basic_block].0.push(op);
        }
        ast::Statement::Repeat(it) => {
            let times = one(lower_expression(it.times().unwrap(), c));
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
        ast::Statement::For(_) => todo!(),
        ast::Statement::Return(_) => todo!(),
        ast::Statement::Expression(it) => assert!(lower_expression(it, c).is_empty()),
    }
}

fn lower_expression(expression: ast::Expression, c: &mut Context) -> Vec<mir::Value> {
    match expression {
        ast::Expression::Parenthesized(it) => lower_expression(it.inner().unwrap(), c),
        ast::Expression::Variable(_) => todo!(),
        ast::Expression::FunctionCall(_) => lower_call(c),
        ast::Expression::BinaryOperation(_) => lower_call(c),
        ast::Expression::NamedArgument(_) => todo!(),
        ast::Expression::DecimalNumber(it) => {
            let span = it.syntax().span();
            let file = c.code_map.find_file(span.low());
            [mir::Value::Num(file.source_slice(span).parse().unwrap())].into()
        }
        ast::Expression::BinaryNumber(_) => todo!(),
        ast::Expression::OctalNumber(_) => todo!(),
        ast::Expression::HexadecimalNumber(_) => todo!(),
        ast::Expression::String(_) => todo!(),
        ast::Expression::KwFalse(_) => [mir::Value::Bool(false)].into(),
        ast::Expression::KwTrue(_) => [mir::Value::Bool(true)].into(),
        ast::Expression::Lvalue(_) => todo!(),
        ast::Expression::ListLiteral(_) => todo!(),
        ast::Expression::TypeAscription(it) => lower_expression(it.inner().unwrap(), c),
        ast::Expression::MethodCall(_) => lower_call(c),
        ast::Expression::FieldAccess(_) => todo!(),
    }
}

fn lower_call(c: &mut Context) -> Vec<mir::Value> {
    todo!()
}

fn one(values: Vec<mir::Value>) -> mir::Value {
    <[_; 1]>::try_from(values).unwrap_or_else(|_| unreachable!())[0]
}
