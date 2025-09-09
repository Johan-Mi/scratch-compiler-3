use crate::ast::{self, Node};
use crate::parser::{K, SyntaxNode};
use codemap::{CodeMap, Pos, Span};

pub fn resolve(document: SyntaxNode, code_map: &CodeMap) -> impl Iterator<Item = Resolution> {
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
            parameters.iter().filter_map(move |it| {
                let identifier = it.internal_name()?.span();
                Some(Definition { identifier, scope })
            })
        });
    let fors = document.pre_order().filter_map(|node| {
        let it = ast::For::cast(node)?;
        let identifier = it.variable()?.span();
        let scope = it.body()?.syntax().span();
        Some(Definition { identifier, scope })
    });
    let definitions: Vec<_> = lets.chain(parameters).chain(fors).collect();

    let usages = document.pre_order().filter(|it| it.kind() == K::Variable);
    usages.map(cst::Node::span).filter_map(move |usage| {
        let text = file.source_slice(usage);
        let definition = definitions
            .iter()
            .filter(|it| it.scope.contains(usage) && file.source_slice(it.identifier) == text)
            .min_by_key(|it| it.scope.len())?
            .identifier
            .low();
        let usage = usage.low();
        Some(Resolution { usage, definition })
    })
}

pub struct Resolution {
    usage: Pos,
    definition: Pos,
}

struct Definition {
    identifier: Span,
    scope: Span,
}

fn after(span: Span, split: Pos) -> Span {
    span.subspan(split - span.low(), span.len())
}
