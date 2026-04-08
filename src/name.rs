use crate::ast::{self, Node};
use crate::parser::{K, SyntaxNode};
use codemap::{CodeMap, Pos, Span};
use std::collections::HashMap;

pub type S = HashMap<ast::VariableUnmanaged, Pos>;

pub fn resolve(ast: ast::Program, code_map: &CodeMap) -> S {
    ast.documents()
        .flat_map(|it| crate::name::resolve_document(it.syntax(), code_map))
        .collect()
}

fn resolve_document(
    document: SyntaxNode,
    code_map: &CodeMap,
) -> impl Iterator<Item = (ast::VariableUnmanaged, Pos)> {
    let file = code_map.find_file(document.span().low());

    let lets = document
        .pre_order()
        .filter(|it| matches!(it.kind(), K::Document | K::Sprite | K::Block))
        .flat_map(|node| {
            node.children().filter_map(move |it| {
                let identifier = ast::Let::cast(it)?.variable()?.span();
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
                let identifier = it.internal_name().span();
                Definition { identifier, scope }
            })
        });
    let fors = document.pre_order().filter_map(|node| {
        let it = ast::For::cast(node)?;
        let identifier = it.variable()?.span();
        let scope = it.body()?.syntax().span();
        Some(Definition { identifier, scope })
    });
    let definitions: Vec<_> = lets.chain(parameters).chain(fors).collect();

    document.pre_order().filter_map(move |usage| {
        let span = usage.span();
        let usage = ast::Variable::cast(usage)?;
        let text = file.source_slice(span);
        let possible = definitions
            .iter()
            .filter(|it| it.scope.contains(span) && file.source_slice(it.identifier) == text);
        let definition = possible.min_by_key(|it| it.scope.len())?.identifier.low();
        Some((usage.unmanaged(), definition))
    })
}

struct Definition {
    identifier: Span,
    scope: Span,
}

fn after(span: Span, split: Pos) -> Span {
    span.subspan(split - span.low(), span.len())
}
