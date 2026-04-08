use super::{Base, Shape, Type};
use crate::ast::{self, Node};
use crate::diagnostics::{Diagnostics, primary};
use std::collections::{HashMap, hash_map::Entry};
use std::ops::Range;

pub type S = HashMap<ast::StructUnmanaged, Vec<Range<usize>>>;

pub fn s(
    ast: ast::Program,
    type_expressions: &HashMap<ast::TypeExpressionUnmanaged, Type>,
    diagnostics: &mut Diagnostics,
) -> S {
    let mut layouts = S::new();
    for component in scc(ast, type_expressions) {
        let [it] = *component else {
            let labels = component
                .iter()
                .map(|it| primary(it.name().unwrap().span(), ""));
            diagnostics.error(
                "mutually recursive structs would be infinitely large",
                labels.collect::<Vec<_>>(),
            );
            continue;
        };

        if it
            .parameters()
            .unwrap()
            .iter()
            .map(|field| type_expressions[&field.ty().unwrap().unmanaged()])
            .any(|ty| ty.base == Base::Struct(it))
        {
            diagnostics.error(
                "recursive struct would be infinitely large",
                [primary(it.name().unwrap().span(), "")],
            );
            continue;
        }

        let layout = it
            .parameters()
            .unwrap()
            .iter()
            .map(|field| type_expressions[&field.ty().unwrap().unmanaged()])
            .inspect(|ty| assert!(matches!(ty.shape, Shape::Flat)))
            .map(|ty| size(ty.base, &layouts))
            .scan(0, |start, size| Some(*start..(*start += size, *start).1))
            .collect();
        assert!(layouts.insert(it.unmanaged(), layout).is_none());
    }
    layouts
}

pub fn size(base: Base, layouts: &S) -> usize {
    match base {
        Base::Unit => 0,
        Base::Num | Base::String | Base::Bool => 1,
        Base::Struct(it) => layouts
            // Recursive structs default to size 0, which is fine since they emit errors,
            // thereby preventing us from ever using any layouts.
            .get(&it.unmanaged())
            .and_then(|it| it.last())
            .map_or(0, |it| it.end),
        Base::Generic(_) => unreachable!(),
    }
}

fn scc<'src>(
    ast: ast::Program<'src>,
    type_expressions: &HashMap<ast::TypeExpressionUnmanaged, Type<'src>>,
) -> Vec<Vec<ast::Struct<'src>>> {
    struct Tarjan<'a, 'src> {
        type_expressions: &'a HashMap<ast::TypeExpressionUnmanaged, Type<'src>>,
        result: Vec<Vec<ast::Struct<'src>>>,
        stack: Vec<ast::Struct<'src>>,
        low: HashMap<codemap::Pos, usize>,
    }

    impl<'src> Tarjan<'_, 'src> {
        fn visit(&mut self, node: ast::Struct<'src>) {
            let num = self.low.len();
            match self.low.entry(node.syntax().span().low()) {
                Entry::Occupied(_) => return,
                Entry::Vacant(it) => _ = it.insert(num),
            }
            let stack_pos = self.stack.len();
            self.stack.push(node);

            for successor in node.parameters().unwrap().iter().filter_map(|it| {
                match self.type_expressions[&it.ty().unwrap().unmanaged()].base {
                    Base::Struct(it) => Some(it),
                    _ => None,
                }
            }) {
                self.visit(successor);
                let other = self.low[&successor.syntax().span().low()];
                let l = self.low.get_mut(&node.syntax().span().low()).unwrap();
                *l = (*l).min(other);
            }

            if num == self.low[&node.syntax().span().low()] {
                let component = self.stack.split_off(stack_pos);
                let done = component
                    .iter()
                    .map(|it| (it.syntax().span().low(), usize::MAX));
                self.low.extend(done);
                self.result.push(component);
            }
        }
    }

    let mut tarjan = Tarjan {
        type_expressions,
        result: Vec::new(),
        stack: Vec::new(),
        low: HashMap::new(),
    };
    ast.documents()
        .flat_map(ast::Document::structs)
        .for_each(|it| tarjan.visit(it));
    tarjan.result
}
