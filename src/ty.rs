use crate::ast::{self, Node};
use crate::diagnostics::{Diagnostics, primary};
use crate::parser::K;
use codemap::{CodeMap, Pos, Span};
use std::{collections::HashMap, fmt};

#[derive(Clone, Copy, PartialEq)]
enum Type {
    Unit,
    Num,
    String,
    Bool,
    Struct, // TODO: Which one?
    List,   // TODO: Of what item type?
    Ref,    // TODO: To what type?
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("Unit"),
            Self::Num => f.write_str("Num"),
            Self::String => f.write_str("String"),
            Self::Bool => f.write_str("Bool"),
            Self::Struct | Self::List | Self::Ref => todo!(),
        }
    }
}

pub fn check(documents: &[ast::Document], code_map: &CodeMap, diagnostics: &mut Diagnostics) {
    let variable_definitions = documents
        .iter()
        .flat_map(|it| crate::name::resolve(it.syntax(), code_map))
        .map(|it| (it.usage, it.definition))
        .collect();

    let type_expressions: HashMap<Span, Type> = documents
        .iter()
        .flat_map(|it| it.syntax().pre_order())
        .filter_map(|it| {
            None.or_else(|| Node::cast(it).map(ast::FieldDefinition::ty))
                .or_else(|| Node::cast(it).map(ast::Parameter::ty))
                .or_else(|| Node::cast(it).map(ast::TypeAscription::ty))?
        })
        .filter_map(|it| Some((it.syntax().span(), evaluate(it, diagnostics)?)))
        .collect();

    let mut c = Checker {
        variable_definitions,
        global_variables: HashMap::new(),
        type_expressions,
        documents,
        code_map,
        diagnostics,
    };

    for (variable, value) in documents
        .iter()
        .flat_map(|it| it.syntax().pre_order())
        .filter(|it| matches!(it.kind(), K::Document | K::Sprite))
        .flat_map(cst::Node::children)
        .filter_map(ast::Let::cast)
        .filter_map(|it| Some((it.variable()?.span().low(), it.value()?)))
    {
        if let Some(ty) = of(value, &mut c) {
            assert!(c.global_variables.insert(variable, ty).is_none());
        }
    }
}

struct Checker<'a> {
    variable_definitions: HashMap<Pos, Pos>,
    global_variables: HashMap<Pos, Type>,
    type_expressions: HashMap<Span, Type>,
    documents: &'a [ast::Document<'a>],
    code_map: &'a CodeMap,
    diagnostics: &'a mut Diagnostics,
}

fn of(expression: ast::Expression, c: &mut Checker) -> Option<Type> {
    match expression {
        ast::Expression::Parenthesized(it) => of(it.inner()?, c),
        ast::Expression::Variable(it) => {
            let definition = c.variable_definitions.get(&it.syntax().span().low())?;
            c.global_variables.get(definition).copied()
        }
        ast::Expression::FunctionCall(it) => {
            let Some(return_ty) =
                resolve_call(it, c.documents, c.code_map, c.diagnostics)?.return_ty()
            else {
                return Some(Type::Unit);
            };
            c.type_expressions.get(&return_ty.syntax().span()).copied()
        }
        ast::Expression::BinaryOperation(it) => todo!(),
        ast::Expression::NamedArgument(it) => of(it.value()?, c),
        ast::Expression::Literal(it) => Some(match it.token().kind() {
            K::DecimalNumber | K::BinaryNumber | K::OctalNumber | K::HexadecimalNumber => Type::Num,
            K::String => Type::String,
            K::KwFalse | K::KwTrue => Type::Bool,
            _ => unreachable!(),
        }),
        ast::Expression::Lvalue(it) => {
            let inner = of(it.inner()?, c)?;
            Some(Type::Ref)
        }
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
            let ascribed_ty: Option<Type> =
                c.type_expressions.get(&it.ty()?.syntax().span()).copied();
            let inner = it.inner()?;
            let actual_ty = of(inner, c);
            if let Some(actual) = actual_ty
                && let Some(ascribed) = ascribed_ty
                && actual != ascribed
            {
                let span = inner.syntax().span();
                c.diagnostics.error(
                    "type mismatch in ascription",
                    [primary(span, format!("this has type `{actual}`"))],
                );
            }
            actual_ty
        }
        ast::Expression::MethodCall(it) => todo!(),
    }
}

fn evaluate(expression: ast::Expression, diagnostics: &mut Diagnostics) -> Option<Type> {
    let span = expression.syntax().span();
    let mut err = |message| {
        diagnostics.error(message, [primary(span, "")]);
        None
    };

    match expression {
        ast::Expression::Parenthesized(_) => {
            err("parenthesized expression cannot be used as a type")
        }
        ast::Expression::Variable(_) => err("variable cannot be used as a type"),
        ast::Expression::FunctionCall(_) => err("function call cannot be used as a type"),
        ast::Expression::BinaryOperation(_) => err("binary operation cannot be used as a type"),
        ast::Expression::NamedArgument(_) => err("named argument cannot be used as a type"),
        ast::Expression::Literal(_) => err("literal cannot be used as a type"),
        ast::Expression::Lvalue(_) => err("lvalue cannot be used as a type"),
        ast::Expression::GenericTypeInstantiation(_) => {
            err("generic type instantiation cannot be used as a type")
        }
        ast::Expression::ListLiteral(_) => err("list literal cannot be used as a type"),
        ast::Expression::TypeAscription(_) => err("type ascription cannot be used as a type"),
        ast::Expression::MethodCall(_) => err("method call cannot be used as a type"),
    }
}

fn resolve_call<'src>(
    call: ast::FunctionCall,
    documents: &[ast::Document<'src>],
    code_map: &CodeMap,
    diagnostics: &mut Diagnostics,
) -> Option<ast::Function<'src>> {
    let name = call.name().span();
    let name_text = code_map.find_file(name.low()).source_slice(name);

    let all_in_scope = {
        let global_functions = documents.iter().flat_map(|it| it.functions());
        let sprite_functions = documents
            .iter()
            .flat_map(|it| it.sprites())
            .filter(|it| it.syntax().span().contains(call.syntax().span()))
            .flat_map(ast::Sprite::functions);
        global_functions.chain(sprite_functions)
    };

    let all_overloads: Vec<ast::Function> = all_in_scope
        .filter(|function| {
            let span = function.name().map(cst::Node::span);
            span.is_some_and(|it| code_map.find_file(it.low()).source_slice(it) == name_text)
        })
        .collect();
    if all_overloads.is_empty() {
        diagnostics.error("undefined function", [primary(name, "")]);
        return None;
    }

    let viable_overloads: Vec<ast::Function> =
        all_overloads.iter().copied().filter(|_| todo!()).collect();
    match *viable_overloads {
        [] => {
            diagnostics.error("function call has no viable overload", [primary(name, "")]);
            None
        }
        [it] => Some(it),
        _ => {
            let spans: Vec<_> = all_overloads
                .iter()
                .filter_map(|it| Some(primary(it.name()?.span(), "")))
                .collect();
            diagnostics.note("following are all of the non-viable overloads:", spans);
            None
        }
    }
}
