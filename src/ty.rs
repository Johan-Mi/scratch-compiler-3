use crate::ast::{self, Node};
use crate::diagnostics::{Diagnostics, primary};
use crate::parser::K;
use codemap::{CodeMap, Pos, Span};
use std::{collections::HashMap, fmt};

#[derive(Clone, Copy, PartialEq)]
enum Type<'src> {
    Unit,
    Num,
    String,
    Bool,
    Struct(ast::Struct<'src>),
    List(Id),
    Ref(Id),
}

impl<'src> Type<'src> {
    fn display(self, c: &'src Checker) -> impl fmt::Display + 'src {
        struct Display<'a> {
            ty: Type<'a>,
            code_map: &'a CodeMap,
            interner: &'a Interner<'a>,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut next = Some(self.ty);
                let mut nesting = 0;
                while let Some(ty) = next.take() {
                    match ty {
                        Type::Unit => f.write_str("Unit")?,
                        Type::Num => f.write_str("Num")?,
                        Type::String => f.write_str("String")?,
                        Type::Bool => f.write_str("Bool")?,
                        Type::Struct(s) => {
                            let name = s.name().unwrap().span();
                            let name = self.code_map.find_file(name.low()).source_slice(name);
                            f.write_str(name)?;
                        }
                        Type::List(inner) => {
                            f.write_str("List[")?;
                            nesting += 1;
                            next = Some(self.interner.get(inner));
                        }
                        Type::Ref(inner) => {
                            f.write_str("Ref[")?;
                            nesting += 1;
                            next = Some(self.interner.get(inner));
                        }
                    }
                }
                for _ in 0..nesting {
                    f.write_str("]")?;
                }
                Ok(())
            }
        }

        Display {
            ty: self,
            code_map: c.code_map,
            interner: &c.interner,
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
        variable_types: HashMap::new(),
        type_expressions,
        interner: Interner(Vec::new()),
        documents,
        code_map,
        diagnostics,
        return_ty: None,
    };

    for (variable, value) in documents
        .iter()
        .flat_map(|it| it.syntax().pre_order())
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
        .flat_map(|it| it.syntax().pre_order())
        .filter_map(ast::Function::cast)
    {
        if let Some(body) = function.body() {
            c.return_ty = return_ty(function, &c);
            check_block(body, &mut c);
            if c.return_ty.is_some_and(|it| it != Type::Unit) && !body.diverges() {
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
    interner: Interner<'src>,
    documents: &'src [ast::Document<'src>],
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
                expect(condition, Type::Bool, c);
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
                expect(times, Type::Num, c);
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
                expect(condition, Type::Bool, c);
            }
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::Until(it) => {
            if let Some(condition) = it.condition() {
                expect(condition, Type::Bool, c);
            }
            if let Some(body) = it.body() {
                check_block(body, c);
            }
        }
        ast::Statement::For(it) => {
            if let Some(times) = it.times() {
                expect(times, Type::Num, c);
            }
            if let Some(variable) = it.variable() {
                let variable = variable.span().low();
                assert!(c.variable_types.insert(variable, Type::Num).is_none());
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
                && ty != Type::Unit
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
        ast::Expression::FunctionCall(it) => return_ty(
            resolve_call(it.name().span(), c.documents, c.code_map, c.diagnostics)?,
            c,
        ),
        ast::Expression::BinaryOperation(it) => return_ty(
            resolve_call(it.operator().span(), c.documents, c.code_map, c.diagnostics)?,
            c,
        ),
        ast::Expression::NamedArgument(it) => of(it.value()?, ascribed, c),
        ast::Expression::Literal(it) => Some(match it.token().kind() {
            K::DecimalNumber | K::BinaryNumber | K::OctalNumber | K::HexadecimalNumber => Type::Num,
            K::String => Type::String,
            K::KwFalse | K::KwTrue => Type::Bool,
            _ => unreachable!(),
        }),
        ast::Expression::Lvalue(it) => {
            let inner = of(it.inner()?, None, c)?;
            Some(Type::Ref(c.interner.intern(inner)))
        }
        ast::Expression::GenericTypeInstantiation(it) => todo!(),
        ast::Expression::ListLiteral(it) => {
            let span = it.syntax().span();
            let mut items = it.iter();
            let Some(first) = items.next() else {
                return ascribed
                    .filter(|it| matches!(it, Type::List(_)))
                    .or_else(|| {
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
            Some(Type::List(c.interner.intern(first_type?)))
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
            resolve_call(it.name().span(), c.documents, c.code_map, c.diagnostics)?,
            c,
        ),
        ast::Expression::FieldAccess(it) => of_field_access(it, c),
    }
}

fn of_field_access<'src>(it: ast::FieldAccess<'src>, c: &mut Checker<'src>) -> Option<Type<'src>> {
    let aggregate_ty = of(it.aggregate(), None, c)?;
    let name = it.field().span();
    let name = c.code_map.find_file(name.low()).source_slice(name);
    let Type::Struct(r#struct) = aggregate_ty else {
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
        return Some(Type::Unit);
    };
    c.type_expressions.get(&ty.syntax().span()).copied()
}

fn evaluate<'src>(
    expression: ast::Expression<'src>,
    diagnostics: &mut Diagnostics,
) -> Option<Type<'src>> {
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
        ast::Expression::FieldAccess(_) => err("field access cannot be used as a type"),
    }
}

fn resolve_call<'src>(
    name: Span,
    documents: &[ast::Document<'src>],
    code_map: &CodeMap,
    diagnostics: &mut Diagnostics,
) -> Option<ast::Function<'src>> {
    let name_text = code_map.find_file(name.low()).source_slice(name);

    let all_in_scope = {
        let global_functions = documents.iter().flat_map(|it| it.functions());
        let sprite_functions = documents
            .iter()
            .flat_map(|it| it.sprites())
            .filter(|it| it.syntax().span().contains(name))
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

struct Interner<'src>(Vec<Type<'src>>);

impl<'src> Interner<'src> {
    fn intern(&mut self, ty: Type<'src>) -> Id {
        Id(self.0.iter().position(|&it| it == ty).unwrap_or_else(|| {
            let index = self.0.len();
            self.0.push(ty);
            index
        }))
    }

    fn get(&self, id: Id) -> Type<'src> {
        self.0[id.0]
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Id(usize);

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
