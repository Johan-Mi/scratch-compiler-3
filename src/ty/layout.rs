use super::{Base, Shape, Type};
use crate::ast::{self, Node};
use std::collections::{HashMap, hash_map::Entry};
use std::ops::Range;

fn of(
    r#struct: ast::Struct,
    type_expressions: &HashMap<codemap::Pos, Type>,
) -> impl Iterator<Item = Range<usize>> {
    r#struct
        .parameters()
        .unwrap()
        .iter()
        .map(|field| type_expressions[&field.ty().unwrap().syntax().span().low()])
        .inspect(|ty| assert!(matches!(ty.shape, Shape::Flat)))
        .map(|ty| match ty.base {
            Base::Unit => 0,
            Base::Num | Base::String | Base::Bool => 1,
            Base::Struct(it) => of(it, type_expressions).last().map_or(0, |it| it.end),
        })
        .scan(0, |start, size| Some(*start..(*start += size, *start).1))
}

fn scc<'src>(
    documents: &'src [cst::Tree<crate::parser::K>],
    type_expressions: &HashMap<codemap::Pos, Type<'src>>,
) -> Vec<Vec<ast::Struct<'src>>> {
    struct Tarjan<'a, 'src> {
        type_expressions: &'a HashMap<codemap::Pos, Type<'src>>,
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
                match self.type_expressions[&it.ty().unwrap().syntax().span().low()].base {
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
    documents
        .iter()
        .flat_map(|it| ast::Document::cast(it.root()).unwrap().structs())
        .for_each(|it| tarjan.visit(it));
    tarjan.result
}
