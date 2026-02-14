use crate::ast::{self, Node};
use crate::diagnostics::{Diagnostics, primary};
use crate::parser::K;
use codemap::{CodeMap, Pos, Span};
use std::{collections::HashMap, fmt};

#[derive(Clone, Copy, PartialEq)]
struct Type<'src> {
    shape: Shape,
    base: Base<'src>,
}

#[derive(Clone, Copy, PartialEq)]
enum Shape {
    Flat,
    List,
    Ref,
}

#[derive(Clone, Copy, PartialEq)]
enum Base<'src> {
    Unit,
    Num,
    String,
    Bool,
    Struct(ast::Struct<'src>),
}

impl<'src> From<Base<'src>> for Type<'src> {
    fn from(value: Base<'src>) -> Self {
        Self {
            shape: Shape::Flat,
            base: value,
        }
    }
}

impl PartialEq<Base<'_>> for Type<'_> {
    fn eq(&self, other: &Base) -> bool {
        self.shape == Shape::Flat && self.base == *other
    }
}

impl<'src> Type<'src> {
    fn display(self, c: &'src Checker) -> impl fmt::Display + 'src {
        std::fmt::from_fn(move |f| {
            match self.shape {
                Shape::Flat => {}
                Shape::List => f.write_str("[]")?,
                Shape::Ref => f.write_str("*")?,
            }
            match self.base {
                Base::Unit => f.write_str("Unit"),
                Base::Num => f.write_str("Num"),
                Base::String => f.write_str("String"),
                Base::Bool => f.write_str("Bool"),
                Base::Struct(s) => {
                    let name = s.name().unwrap().span();
                    let name = c.code_map.find_file(name.low()).source_slice(name);
                    f.write_str(name)
                }
            }
        })
    }
}

pub fn check(documents: &[cst::Tree<K>], code_map: &CodeMap, diagnostics: &mut Diagnostics) {
    let variable_definitions = documents
        .iter()
        .flat_map(|it| crate::name::resolve(it.root(), code_map))
        .map(|it| (it.usage, it.definition))
        .collect();

    let type_expressions: HashMap<Span, Type> = documents
        .iter()
        .flat_map(|it| it.root().pre_order())
        .filter_map(|it| {
            None.or_else(|| Node::cast(it).map(ast::FieldDefinition::ty))
                .or_else(|| Node::cast(it).map(ast::Parameter::ty))
                .or_else(|| Node::cast(it).map(ast::TypeAscription::ty))?
        })
        .filter_map(|it| {
            Some((
                it.syntax().span(),
                evaluate(it, documents, code_map, diagnostics)?,
            ))
        })
        .collect();

    let mut c = Checker {
        variable_definitions,
        variable_types: HashMap::new(),
        type_expressions,
        documents,
        code_map,
        diagnostics,
        return_ty: None,
    };

    for (variable, value) in documents
        .iter()
        .flat_map(|it| it.root().pre_order())
        .filter(|it| matches!(it.kind(), K::Document | K::Sprite))
        .flat_map(cst::Node::children)
        .filter_map(ast::Let::cast)
        .filter_map(|it| Some((it.variable()?.span().low(), it.value()?)))
    {
        if let Some(ty) = of(value, None, &mut c) {
            assert!(c.variable_types.insert(variable, ty).is_none());
        }
    }

    for function in documents
        .iter()
        .flat_map(|it| it.root().pre_order())
        .filter_map(ast::Function::cast)
    {
        if let Some(body) = function.body() {
            c.return_ty = return_ty(function, &c);
            check_block(body, &mut c);
            if c.return_ty.is_some_and(|it| it != Base::Unit) && !body.diverges() {
                c.diagnostics.error(
                    "function reaches end without returning",
                    [primary(function.syntax().span(), "")],
                );
            }
        }
    }
}

struct Checker<'src> {
    variable_definitions: HashMap<Pos, Pos>,
    variable_types: HashMap<Pos, Type<'src>>,
    type_expressions: HashMap<Span, Type<'src>>,
    documents: &'src [cst::Tree<K>],
    code_map: &'src CodeMap,
    diagnostics: &'src mut Diagnostics,
    return_ty: Option<Type<'src>>,
}

fn check_block<'src>(block: ast::Block<'src>, c: &mut Checker<'src>) {
    for statement in block.statements() {
        check_statement(statement, c);
    }
}

fn check_statement<'src>(statement: ast::Statement<'src>, c: &mut Checker<'src>) {
    match statement {
        ast::Statement::Let(it) => {
            if let Some(value) = it.value()
                && let Some(ty) = of(value, None, c)
                && let Some(variable) = it.variable()
            {
                assert!(c.variable_types.insert(variable.span().low(), ty).is_none());
            }
        }
        ast::Statement::If(it) => {
            if let Some(condition) = it.condition() {
                expect(condition, Base::Bool.into(), c);
            }
            if let Some(then) = it.then() {
                check_block(then, c);
            }
            if let Some(else_clause) = it.else_clause() {
                if let Some(r#else) = else_clause.block() {
                    check_block(r#else, c);
                } else if let Some(else_if) = else_clause.if_() {
                    check_statement(ast::Statement::If(else_if), c);
                }
            }
        }
        ast::Statement::Repeat(it) => {
            if let Some(times) = it.times() {
                expect(times, Base::Num.into(), c);
            }
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::Forever(it) => {
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::While(it) => {
            if let Some(condition) = it.condition() {
                expect(condition, Base::Bool.into(), c);
            }
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::Until(it) => {
            if let Some(condition) = it.condition() {
                expect(condition, Base::Bool.into(), c);
            }
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::For(it) => {
            if let Some(times) = it.times() {
                expect(times, Base::Num.into(), c);
            }
            if let Some(variable) = it.variable() {
                let variable = variable.span().low();
                assert!(
                    c.variable_types
                        .insert(variable, Base::Num.into())
                        .is_none()
                );
            }
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::Return(it) => {
            if let Some(returned) = it.expression()
                && let Some(ty) = of(returned, None, c)
                && let Some(expected) = c.return_ty
                && ty != expected
            {
                c.diagnostics.error(
                    "function return type mismatch",
                    [primary(it.syntax().span(), "")],
                );
            }
        }
        ast::Statement::Expression(it) => {
            if let Some(ty) = of(it, None, c)
                && ty != Base::Unit
            {
                c.diagnostics.error(
                    format!("value of type `{}` is ignored", ty.display(c)),
                    [primary(it.syntax().span(), "")],
                );
            }
        }
    }
}

fn of<'src>(
    expression: ast::Expression<'src>,
    ascribed: Option<Type<'src>>,
    c: &mut Checker<'src>,
) -> Option<Type<'src>> {
    match expression {
        ast::Expression::Parenthesized(it) => of(it.inner()?, ascribed, c),
        ast::Expression::Variable(it) => {
            let definition = c.variable_definitions.get(&it.syntax().span().low())?;
            c.variable_types.get(definition).copied()
        }
        ast::Expression::FunctionCall(it) => {
            return_ty(resolve_call(it.name().span(), &mut it.args().iter(), c)?, c)
        }
        ast::Expression::BinaryOperation(it) => return_ty(
            resolve_call(
                it.operator().span(),
                &mut [it.lhs()?, it.rhs()?].into_iter(),
                c,
            )?,
            c,
        ),
        ast::Expression::NamedArgument(it) => of(it.value()?, ascribed, c),
        ast::Expression::Literal(it) => Some(match it.token().kind() {
            K::DecimalNumber | K::BinaryNumber | K::OctalNumber | K::HexadecimalNumber => {
                Base::Num.into()
            }
            K::String => Base::String.into(),
            K::KwFalse | K::KwTrue => Base::Bool.into(),
            _ => unreachable!(),
        }),
        ast::Expression::Lvalue(it) => {
            let inner = of(it.inner()?, None, c)?;
            match inner.shape {
                Shape::Flat => Some(Type {
                    shape: Shape::Ref,
                    ..inner
                }),
                Shape::List => {
                    c.diagnostics.error(
                        "references to lists are not supported",
                        [primary(it.syntax().span(), "")],
                    );
                    None
                }
                Shape::Ref => {
                    c.diagnostics.error(
                        "references to references are not supported",
                        [primary(it.syntax().span(), "")],
                    );
                    None
                }
            }
        }
        ast::Expression::ListLiteral(it) => {
            let span = it.syntax().span();
            let mut items = it.iter();
            let Some(first) = items.next() else {
                return ascribed.filter(|it| it.shape == Shape::List).or_else(|| {
                    c.diagnostics
                        .error("cannot infer type of empty list", [primary(span, "")]);
                    None
                });
            };
            let first_type = of(first, None, c);
            for item in items {
                let item_type = of(item, None, c);
                if first_type.zip(item_type).is_some_and(|(f, i)| f != i) {
                    c.diagnostics
                        .error("type mismatch", [primary(item.syntax().span(), "")]);
                }
            }
            let first_type = first_type?;
            match first_type.shape {
                Shape::Flat => Some(Type {
                    shape: Shape::List,
                    ..first_type
                }),
                Shape::List => {
                    c.diagnostics.error(
                        "lists of lists are not supported",
                        [primary(it.syntax().span(), "")],
                    );
                    None
                }
                Shape::Ref => {
                    c.diagnostics.error(
                        "lists of references are not supported",
                        [primary(it.syntax().span(), "")],
                    );
                    None
                }
            }
        }
        ast::Expression::TypeAscription(it) => {
            let ascribed_ty = c.type_expressions.get(&it.ty()?.syntax().span()).copied();
            let inner = it.inner()?;
            if let Some(actual) = of(inner, ascribed_ty, c)
                && let Some(ascribed) = ascribed_ty
                && actual != ascribed
            {
                let span = inner.syntax().span();
                c.diagnostics.error(
                    "type mismatch in ascription",
                    [primary(
                        span,
                        format!("this has type `{}`", actual.display(c)),
                    )],
                );
            }
            ascribed_ty
        }
        ast::Expression::MethodCall(it) => return_ty(
            resolve_call(
                it.name().span(),
                &mut std::iter::once(it.caller()).chain(it.arguments().iter()),
                c,
            )?,
            c,
        ),
        ast::Expression::FieldAccess(it) => of_field_access(it, c),
    }
}

fn of_field_access<'src>(it: ast::FieldAccess<'src>, c: &mut Checker<'src>) -> Option<Type<'src>> {
    let aggregate_ty = of(it.aggregate(), None, c)?;
    let name = it.field().span();
    let name = c.code_map.find_file(name.low()).source_slice(name);
    let Type {
        shape: Shape::Flat,
        base: Base::Struct(r#struct),
    } = aggregate_ty
    else {
        c.diagnostics.error(
            format!("type `{}` has no fields", aggregate_ty.display(c)),
            [primary(it.syntax().span(), "")],
        );
        return None;
    };
    let file = c.code_map.find_file(r#struct.syntax().span().low());
    let Some(field) = r#struct
        .fields()
        .find(|it| file.source_slice(it.name().span()) == name)
    else {
        c.diagnostics.error(
            format!(
                "type `{}` has no field named `{name}`",
                aggregate_ty.display(c)
            ),
            [primary(it.syntax().span(), "")],
        );
        return None;
    };
    c.type_expressions
        .get(&field.ty()?.syntax().span())
        .copied()
}

fn return_ty<'src>(function: ast::Function<'src>, c: &Checker<'src>) -> Option<Type<'src>> {
    let Some(ty) = function.return_ty() else {
        return Some(Base::Unit.into());
    };
    c.type_expressions.get(&ty.syntax().span()).copied()
}

fn evaluate<'src>(
    expression: ast::TypeExpression,
    documents: &'src [cst::Tree<K>],
    code_map: &CodeMap,
    diagnostics: &mut Diagnostics,
) -> Option<Type<'src>> {
    Some(Type::from(match expression {
        ast::TypeExpression::TypeVariable(variable) => {
            let span = variable.syntax().span();
            let variable = code_map.find_file(span.low()).source_slice(span);
            match variable {
                "Unit" => Base::Unit,
                "Num" => Base::Num,
                "String" => Base::String,
                "Bool" => Base::Bool,
                _ => Base::Struct(
                    documents
                        .iter()
                        .find_map(|document| {
                            let file = code_map.find_file(document.root().span().low());
                            ast::Document::cast(document.root())
                                .unwrap()
                                .structs()
                                .find(|it| {
                                    it.name()
                                        .is_some_and(|it| file.source_slice(it.span()) == variable)
                                })
                        })
                        .or_else(|| {
                            diagnostics.error("undefined type variable", [primary(span, "")]);
                            None
                        })?,
                ),
            }
        }
    }))
}

fn resolve_call<'src>(
    name: Span,
    arguments: &mut dyn Iterator<Item = ast::Expression<'src>>,
    c: &mut Checker<'src>,
) -> Option<ast::Function<'src>> {
    let file = c.code_map.find_file(name.low());
    let name_text = file.source_slice(name);

    let all_in_scope = {
        let global_functions = c
            .documents
            .iter()
            .flat_map(|it| ast::Document::cast(it.root()).unwrap().functions());
        let sprite_functions = c
            .documents
            .iter()
            .flat_map(|it| ast::Document::cast(it.root()).unwrap().sprites())
            .filter(|it| it.syntax().span().contains(name))
            .flat_map(ast::Sprite::functions);
        global_functions.chain(sprite_functions)
    };

    let all_overloads: Vec<ast::Function> = all_in_scope
        .filter(|function| {
            let span = function.name().map(cst::Node::span);
            span.is_some_and(|it| c.code_map.find_file(it.low()).source_slice(it) == name_text)
        })
        .collect();
    if all_overloads.is_empty() {
        c.diagnostics
            .error("undefined function", [primary(name, "")]);
        return None;
    }

    let (labels, argument_types): (Vec<_>, Vec<_>) = arguments
        .map(|it| {
            (
                if let ast::Expression::NamedArgument(it) = it {
                    Some(file.source_slice(it.name().span()))
                } else {
                    None
                },
                of(it, None, c),
            )
        })
        .collect();

    let viable_overloads: Vec<ast::Function> = all_overloads
        .iter()
        .copied()
        .filter(|it| {
            it.parameters().is_none_or(|it| {
                let file = c.code_map.find_file(it.syntax().span().low());
                it.iter()
                    .map(|it| Some(file.source_slice(it.external_name()?.syntax().span())))
                    .eq(labels.iter().copied())
                    && it
                        .iter()
                        .map(|it| c.type_expressions.get(&it.ty()?.syntax().span()).copied())
                        .zip(argument_types.iter().copied())
                        .filter_map(|(a, b)| a.zip(b))
                        .all(|(a, b)| a == b)
            })
        })
        .collect();
    match *viable_overloads {
        [] => {
            c.diagnostics
                .error("function call has no viable overload", [primary(name, "")]);
            let spans: Vec<_> = all_overloads
                .iter()
                .filter_map(|it| Some(primary(it.name()?.span(), "")))
                .collect();
            c.diagnostics
                .note("following are all of the non-viable overloads:", spans);
            None
        }
        [it] => Some(it),
        _ => {
            c.diagnostics.error(
                "function call has multiple viable overloads",
                [primary(name, "")],
            );
            let spans: Vec<_> = viable_overloads
                .iter()
                .filter_map(|it| Some(primary(it.name()?.span(), "")))
                .collect();
            c.diagnostics
                .note("following are all of the viable overloads:", spans);
            None
        }
    }
}

fn expect<'src>(expression: ast::Expression<'src>, expected_ty: Type, c: &mut Checker<'src>) {
    if let Some(ty) = of(expression, None, c)
        && ty != expected_ty
    {
        c.diagnostics.error(
            "type mismatch",
            [primary(
                expression.syntax().span(),
                format!(
                    "expected `{}`, got `{}`",
                    expected_ty.display(c),
                    ty.display(c)
                ),
            )],
        );
    }
}

impl ast::Block<'_> {
    fn diverges(self) -> bool {
        self.statements().any(ast::Statement::diverges)
    }
}

impl ast::Statement<'_> {
    const fn diverges(self) -> bool {
        matches!(self, Self::Forever(_) | Self::Return(_))
    }
}
