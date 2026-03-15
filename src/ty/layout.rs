use super::{Base, Shape, Type};
use crate::ast::{self, Node};
use std::{collections::HashMap, ops::Range};

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
