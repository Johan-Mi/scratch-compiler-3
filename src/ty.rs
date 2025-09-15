use crate::ast::{self, Node};
use crate::diagnostics::{Diagnostics, primary};
use crate::parser::K;
use codemap::{CodeMap, Pos, Span};
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq)]
enum Type {
    Unit,
    Num,
    String,
    Bool,
    Struct, // TODO: Which one?
    List,   // TODO: Of what item type?
}

fn check(documents: &[ast::Document], code_map: &CodeMap, diagnostics: &mut Diagnostics) {
    let variable_definitions = documents
        .iter()
        .flat_map(|it| crate::name::resolve(it.syntax(), code_map))
        .map(|it| (it.usage, it.definition))
        .collect();

    let type_expressions: HashMap<Span, Type> = todo!();
    let functions = todo!();

    let mut c = Checker {
        variable_definitions,
        global_variables: HashMap::new(),
        type_expressions,
        diagnostics,
    };

    for (variable, ty) in documents
        .iter()
        .flat_map(|it| it.syntax().pre_order())
        .filter(|it| matches!(it.kind(), K::Document | K::Sprite))
        .flat_map(cst::Node::children)
        .filter_map(ast::Let::cast)
        .filter_map(|it| Some((it.variable()?.span().low(), of(it.value()?, &mut c)?)))
    {
        assert!(c.global_variables.insert(variable, ty).is_none());
    }

    let expressions: HashMap<Span, Type> = todo!();
}

struct Checker<'a> {
    variable_definitions: HashMap<Pos, Pos>,
    global_variables: HashMap<Pos, Type>,
    type_expressions: HashMap<Span, Type>,
    diagnostics: &'a mut Diagnostics,
}

fn of(expression: ast::Expression, c: &mut Checker) -> Option<Type> {
    match expression {
        ast::Expression::Parenthesized(it) => of(it.inner()?, c),
        ast::Expression::Variable(it) => {
            let definition = todo!();
        }
        ast::Expression::FunctionCall(it) => todo!(),
        ast::Expression::BinaryOperation(it) => todo!(),
        ast::Expression::NamedArgument(it) => of(it.value()?, c),
        ast::Expression::Literal(it) => Some(match it.token().kind() {
            K::DecimalNumber | K::BinaryNumber | K::OctalNumber | K::HexadecimalNumber => Type::Num,
            K::String => Type::String,
            K::KwFalse | K::KwTrue => Type::Bool,
            _ => unreachable!(),
        }),
        ast::Expression::Lvalue(it) => todo!(),
        ast::Expression::GenericTypeInstantiation(it) => todo!(),
        ast::Expression::ListLiteral(it) => {
            let span = it.syntax().span();
            let mut items = it.iter();
            let Some(first) = items.next() else {
                c.diagnostics
                    .error("cannot infer type of empty list", [primary(span, "")]);
                return None;
            };
            let first_type = of(first, c);
            for item in items {
                let item_type = of(item, c);
                if first_type.zip(item_type).is_some_and(|(f, i)| f != i) {
                    c.diagnostics.error(
                        "TODO: implement error for list item type mismatch",
                        [primary(item.syntax().span(), "")],
                    );
                }
            }
            Some(Type::List)
        }
        ast::Expression::TypeAscription(it) => {
            let ascribed_type: Option<Type> =
                c.type_expressions.get(&it.ty()?.syntax().span()).copied();
            let inner = it.inner()?;
            let actual_ty = of(inner, c);
            if ascribed_type
                .zip(actual_ty)
                .is_some_and(|(ascribed, actual)| ascribed != actual)
            {
                let span = inner.syntax().span();
                c.diagnostics.error(
                    "type mismatch in ascription",
                    [primary(span, "TODO: show what type this has")],
                );
            }
            actual_ty
        }
        ast::Expression::MethodCall(it) => todo!(),
    }
}
