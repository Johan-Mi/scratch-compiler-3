use crate::ast::{self, Node};
use crate::diagnostics::{Diagnostics, primary};
use crate::parser::K;
use codemap::{CodeMap, Pos, Span};
use std::collections::HashMap;

pub type S = HashMap<ast::VariableUnmanaged, ast::VariableDefinitionUnmanaged>;

pub fn resolve(ast: ast::Program, code_map: &CodeMap, diagnostics: &mut Diagnostics) -> S {
    ast.documents()
        .flat_map(|it| crate::name::resolve_document(it.syntax(), code_map))
        .map(|it| it.map_err(|span| diagnostics.error("undefined variable", [primary(span, "")])))
        .filter_map(Result::ok)
        .collect()
}

fn resolve_document(
    document: cst::Node<K>,
    code_map: &CodeMap,
) -> impl Iterator<Item = Result<(ast::VariableUnmanaged, ast::VariableDefinitionUnmanaged), Span>>
{
    let file = code_map.find_file(document.span().low());

    let lets = document
        .pre_order()
        .filter(|it| matches!(it.kind(), K::Document | K::Sprite | K::Block))
        .flat_map(|node| {
            node.children().filter_map(move |it| {
                let identifier = ast::Let::cast(it)?.variable()?;
                let scope = after(node.span(), it.span().high());
                Some(Definition { identifier, scope })
            })
        });
    let parameters = document
        .pre_order()
        .filter_map(ast::Function::cast)
        .filter_map(|it| Some((it.body()?.syntax().span(), it.parameters()?)))
        .flat_map(|(scope, parameters)| {
            parameters.iter().map(move |it| {
                let identifier = it.internal_name();
                Definition { identifier, scope }
            })
        });
    let fors = document.pre_order().filter_map(|node| {
        let it = ast::For::cast(node)?;
        let identifier = it.variable()?;
        let scope = it.body()?.syntax().span();
        Some(Definition { identifier, scope })
    });
    let definitions: Vec<_> = lets.chain(parameters).chain(fors).collect();

    let usages = document.pre_order().filter_map(ast::Variable::cast);
    usages.map(move |usage| {
        let span = usage.syntax().span();
        let text = file.source_slice(span);
        let possible = definitions.iter().filter(|it| {
            it.scope.contains(span) && file.source_slice(it.identifier.syntax().span()) == text
        });
        let definition = possible.min_by_key(|it| it.scope.len()).ok_or(span)?;
        Ok((usage.unmanaged(), definition.identifier.unmanaged()))
    })
}

struct Definition<'src> {
    identifier: ast::VariableDefinition<'src>,
    scope: Span,
}

fn after(span: Span, split: Pos) -> Span {
    span.subspan(split - span.low(), span.len())
}
