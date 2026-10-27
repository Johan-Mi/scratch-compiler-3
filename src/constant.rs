use crate::ast::{self, Node};
use crate::{Diagnostics, diagnostics::primary, mir, parser::K, ty};

pub struct S {
    pub global_variables: Vec<(ast::VariableDefinitionUnmanaged, Vec<mir::Constant>)>,
    pub global_lists: Vec<(ast::ListLiteralUnmanaged, Vec<Vec<mir::Constant>>)>,
}

pub fn s(
    ast: ast::Program,
    code_map: &codemap::CodeMap,
    typing: &ty::Ping,
    diagnostics: &mut Diagnostics,
) -> S {
    let mut e = Evaluator {
        code_map,
        typing,
        diagnostics,
    };

    let global_variables = ast
        .documents()
        .flat_map(|it| it.lets().chain(it.sprites().flat_map(ast::Sprite::lets)))
        .filter_map(|it| {
            Some((
                it.variable().unwrap().unmanaged(),
                eval_expression(&mut e, it.value().unwrap()).ok()?,
            ))
        })
        .collect();

    let global_lists = todo!();

    S {
        global_variables,
        global_lists,
    }
}

struct Evaluator<'a> {
    code_map: &'a codemap::CodeMap,
    typing: &'a ty::Ping<'a>,
    diagnostics: &'a mut Diagnostics,
}

fn eval_expression(
    e: &mut Evaluator,
    expression: ast::Expression,
) -> Result<Vec<mir::Constant>, ()> {
    Ok(match expression {
        ast::Expression::Parenthesized(it) => eval_expression(e, it.inner().unwrap())?,
        ast::Expression::FunctionCall(it) => eval_call(e, expression, &mut it.arguments().iter())?,
        ast::Expression::BinaryOperation(it) => {
            let mut arguments = [it.lhs().unwrap(), it.rhs().unwrap()].into_iter();
            eval_call(e, expression, &mut arguments)?
        }
        ast::Expression::MethodCall(it) => {
            let mut arguments = std::iter::once(it.caller()).chain(it.arguments().iter());
            eval_call(e, expression, &mut arguments)?
        }
        ast::Expression::TypeAscription(it) => eval_expression(e, it.inner().unwrap())?,
        ast::Expression::NamedArgument(it) => eval_expression(e, it.value().unwrap())?,
        ast::Expression::DecimalNumber(it) => {
            let span = it.syntax().span();
            let file = e.code_map.find_file(span.low());
            let number = file.source_slice(span).parse().unwrap();
            [mir::Constant::Num(number)].into()
        }
        ast::Expression::String(_)
        | ast::Expression::BinaryNumber(_)
        | ast::Expression::OctalNumber(_)
        | ast::Expression::HexadecimalNumber(_)
        | ast::Expression::KwFalse(_)
        | ast::Expression::KwTrue(_) => eval_expression_simple(expression, e.code_map),
        ast::Expression::Variable(_) => {
            e.diagnostics.error(
                "reading variables is not yet supported at compile time",
                [primary(expression.syntax().span(), "")],
            );
            return Err(());
        }
        ast::Expression::Index(_) => {
            e.diagnostics.error(
                "indexing is not yet supported at compile time",
                [primary(expression.syntax().span(), "")],
            );
            return Err(());
        }
        ast::Expression::Lvalue(_) => {
            e.diagnostics.error(
                "references are not yet supported at compile time",
                [primary(expression.syntax().span(), "")],
            );
            return Err(());
        }
        ast::Expression::ListLiteral(_) => {
            e.diagnostics.error(
                "list literals are not yet supported at compile time",
                [primary(expression.syntax().span(), "")],
            );
            return Err(());
        }
        ast::Expression::FieldAccess(_) => {
            e.diagnostics.error(
                "field access is not yet supported at compile time",
                [primary(expression.syntax().span(), "")],
            );
            return Err(());
        }
    })
}

pub fn eval_expression_simple(
    expression: ast::Expression,
    code_map: &codemap::CodeMap,
) -> Vec<mir::Constant> {
    match expression {
        ast::Expression::Parenthesized(_)
        | ast::Expression::FunctionCall(_)
        | ast::Expression::BinaryOperation(_)
        | ast::Expression::MethodCall(_)
        | ast::Expression::TypeAscription(_)
        | ast::Expression::NamedArgument(_)
        | ast::Expression::Variable(_)
        | ast::Expression::Index(_)
        | ast::Expression::Lvalue(_)
        | ast::Expression::ListLiteral(_)
        | ast::Expression::FieldAccess(_) => unreachable!(),
        ast::Expression::DecimalNumber(it) => {
            let span = it.syntax().span();
            let file = code_map.find_file(span.low());
            let number = file.source_slice(span).parse().unwrap();
            [mir::Constant::Num(number)].into()
        }
        ast::Expression::String(it) => [mir::Constant::String(it.syntax().span().low())].into(),
        ast::Expression::BinaryNumber(it) => float(2, it.syntax(), code_map),
        ast::Expression::OctalNumber(it) => float(8, it.syntax(), code_map),
        ast::Expression::HexadecimalNumber(it) => float(16, it.syntax(), code_map),
        ast::Expression::KwFalse(_) => [mir::Constant::Bool(false)].into(),
        ast::Expression::KwTrue(_) => [mir::Constant::Bool(true)].into(),
    }
}

fn eval_call(
    e: &mut Evaluator,
    expression: ast::Expression,
    arguments: &mut dyn Iterator<Item = ast::Expression>,
) -> Result<Vec<mir::Constant>, ()> {
    let function_like = e.typing.resolved_calls[&expression.unmanaged()];
    if ast::Struct::cast(function_like.syntax()).is_none() {
        e.diagnostics.error(
            "non-constructor function cannot be called at compile time",
            [primary(expression.syntax().span(), "")],
        );
        return Err(());
    }

    let mut res = Ok(Vec::new());
    for it in arguments {
        if let Ok(constants) = eval_expression(e, it)
            && let Ok(res) = &mut res
        {
            res.extend(constants);
        } else {
            res = Err(());
        }
    }
    res
}

fn float(radix: u32, node: cst::Node<K>, code_map: &codemap::CodeMap) -> Vec<mir::Constant> {
    let span = node.span();
    let text = code_map.find_file(span.low()).source_slice(span);
    let sign = if text.starts_with('-') { -1.0 } else { 1.0 };
    let text = &text.strip_prefix(['+', '-']).unwrap_or(text)[2..];
    #[expect(
        clippy::cast_precision_loss,
        reason = "These are float literals. `u64` is only used as an implementation detail."
    )]
    let number = u64::from_str_radix(text, radix).unwrap() as f64 * sign;
    [mir::Constant::Num(number)].into()
}
