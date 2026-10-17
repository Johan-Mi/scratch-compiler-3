use crate::ast::{self, Node};
use crate::{Diagnostics, diagnostics::primary, mir, parser::K, ty};

pub fn s(
    ast: ast::Program,
    code_map: &codemap::CodeMap,
    typing: &ty::Ping,
    diagnostics: &mut Diagnostics,
) -> Vec<(ast::VariableDefinitionUnmanaged, Vec<mir::Constant>)> {
    ast.documents()
        .flat_map(|it| it.lets().chain(it.sprites().flat_map(ast::Sprite::lets)))
        .filter_map(|it| {
            Some((
                it.variable().unwrap().unmanaged(),
                eval_expression(it.value().unwrap(), code_map, typing, Some(diagnostics)).ok()?,
            ))
        })
        .collect()
}

pub fn eval_expression(
    expression: ast::Expression,
    code_map: &codemap::CodeMap,
    typing: &ty::Ping,
    diagnostics: Option<&mut Diagnostics>,
) -> Result<Vec<mir::Constant>, ()> {
    Ok(match expression {
        ast::Expression::Parenthesized(it) => {
            eval_expression(it.inner().unwrap(), code_map, typing, diagnostics)?
        }
        ast::Expression::FunctionCall(it) => eval_call(
            expression,
            &mut it.arguments().iter(),
            code_map,
            typing,
            diagnostics.unwrap(),
        )?,
        ast::Expression::BinaryOperation(it) => {
            let mut arguments = [it.lhs().unwrap(), it.rhs().unwrap()].into_iter();
            eval_call(
                expression,
                &mut arguments,
                code_map,
                typing,
                diagnostics.unwrap(),
            )?
        }
        ast::Expression::MethodCall(it) => {
            let mut arguments = std::iter::once(it.caller()).chain(it.arguments().iter());
            eval_call(
                expression,
                &mut arguments,
                code_map,
                typing,
                diagnostics.unwrap(),
            )?
        }
        ast::Expression::TypeAscription(it) => {
            eval_expression(it.inner().unwrap(), code_map, typing, diagnostics)?
        }
        ast::Expression::NamedArgument(it) => {
            eval_expression(it.value().unwrap(), code_map, typing, diagnostics)?
        }
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
        ast::Expression::Variable(_)
        | ast::Expression::Index(_)
        | ast::Expression::Lvalue(_)
        | ast::Expression::ListLiteral(_)
        | ast::Expression::FieldAccess(_) => {
            diagnostics.unwrap().error(
                "expression cannot be evaluated at compile time",
                [primary(expression.syntax().span(), "")],
            );
            return Err(());
        }
    })
}

fn eval_call(
    expression: ast::Expression,
    arguments: &mut dyn Iterator<Item = ast::Expression>,
    code_map: &codemap::CodeMap,
    typing: &ty::Ping,
    diagnostics: &mut Diagnostics,
) -> Result<Vec<mir::Constant>, ()> {
    let function_like = typing.resolved_calls[&expression.unmanaged()];
    assert!(ast::Struct::cast(function_like.syntax()).is_some());

    let mut res = Ok(Vec::new());
    for it in arguments {
        if let Ok(constants) = eval_expression(it, code_map, typing, Some(diagnostics))
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
