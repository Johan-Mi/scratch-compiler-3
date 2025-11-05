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
    List(Id),
    Ref(Id),
}

impl Type {
    fn display(self, interner: &Interner) -> impl fmt::Display {
        struct Display<'a> {
            ty: Type,
            interner: &'a Interner,
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
                        Type::Struct => todo!(),
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

        Display { ty: self, interner }
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
            assert!(c.variable_types.insert(variable, ty).is_none());
        }
    }

    for function in documents
        .iter()
        .flat_map(|it| it.syntax().pre_order())
        .filter_map(ast::Function::cast)
    {
        if let Some(body) = function.body()
            && let Some(body_ty) = of_block(body, &mut c)
            && let Some(return_ty) = return_ty(function, &c)
            && body_ty != return_ty
        {
            let span = function.syntax().span();
            c.diagnostics
                .error("function return type mismatch", [primary(span, "")]);
        }
    }
}

struct Checker<'a> {
    variable_definitions: HashMap<Pos, Pos>,
    variable_types: HashMap<Pos, Type>,
    type_expressions: HashMap<Span, Type>,
    interner: Interner,
    documents: &'a [ast::Document<'a>],
    code_map: &'a CodeMap,
    diagnostics: &'a mut Diagnostics,
}

fn of_block(block: ast::Block, c: &mut Checker) -> Option<Type> {
    block
        .statements()
        .fold(
            (block.syntax().span(), Some(Type::Unit)),
            |(prev_span, prev_ty), statement| {
                do_not_ignore(prev_ty, prev_span, c);
                (statement.syntax().span(), of_statement(statement, c))
            },
        )
        .1
}

fn of_statement(statement: ast::Statement, c: &mut Checker) -> Option<Type> {
    match statement {
        ast::Statement::Let(it) => {
            if let Some(value) = it.value()
                && let Some(ty) = of(value, c)
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
                do_not_ignore(of_block(then, c), then.syntax().span(), c);
            }
            if let Some(else_clause) = it.else_clause() {
                if let Some(r#else) = else_clause.block() {
                    do_not_ignore(of_block(r#else, c), r#else.syntax().span(), c);
                } else if let Some(else_if) = else_clause.if_() {
                    do_not_ignore(
                        of_statement(ast::Statement::If(else_if), c),
                        else_if.syntax().span(),
                        c,
                    );
                }
            }
        }
        ast::Statement::Repeat(it) => {
            if let Some(times) = it.times() {
                expect(times, Type::Num, c);
            }
            if let Some(body) = it.body() {
                do_not_ignore(of_block(body, c), body.syntax().span(), c);
            }
        }
        ast::Statement::Forever(it) => {
            if let Some(body) = it.body() {
                do_not_ignore(of_block(body, c), body.syntax().span(), c);
            }
        }
        ast::Statement::While(it) => {
            if let Some(condition) = it.condition() {
                expect(condition, Type::Bool, c);
            }
            if let Some(body) = it.body() {
                do_not_ignore(of_block(body, c), body.syntax().span(), c);
            }
        }
        ast::Statement::Until(it) => {
            if let Some(condition) = it.condition() {
                expect(condition, Type::Bool, c);
            }
            if let Some(body) = it.body() {
                do_not_ignore(of_block(body, c), body.syntax().span(), c);
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
                do_not_ignore(of_block(body, c), body.syntax().span(), c);
            }
        }
        ast::Statement::Return(_) => todo!(),
        ast::Statement::Expression(it) => return of(it, c),
    }
    Some(Type::Unit)
}

fn of(expression: ast::Expression, c: &mut Checker) -> Option<Type> {
    match expression {
        ast::Expression::Parenthesized(it) => of(it.inner()?, c),
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
        ast::Expression::NamedArgument(it) => of(it.value()?, c),
        ast::Expression::Literal(it) => Some(match it.token().kind() {
            K::DecimalNumber | K::BinaryNumber | K::OctalNumber | K::HexadecimalNumber => Type::Num,
            K::String => Type::String,
            K::KwFalse | K::KwTrue => Type::Bool,
            _ => unreachable!(),
        }),
        ast::Expression::Lvalue(it) => {
            let inner = of(it.inner()?, c)?;
            Some(Type::Ref(c.interner.intern(inner)))
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
            Some(Type::List(c.interner.intern(first_type?)))
        }
        ast::Expression::TypeAscription(it) => {
            let ascribed_ty = c.type_expressions.get(&it.ty()?.syntax().span()).copied();
            let inner = it.inner()?;
            if let Some(actual) = of(inner, c)
                && let Some(ascribed) = ascribed_ty
                && actual != ascribed
            {
                let span = inner.syntax().span();
                c.diagnostics.error(
                    "type mismatch in ascription",
                    [primary(
                        span,
                        format!("this has type `{}`", actual.display(&c.interner)),
                    )],
                );
            }
            ascribed_ty
        }
        ast::Expression::MethodCall(it) => return_ty(
            resolve_call(it.name().span(), c.documents, c.code_map, c.diagnostics)?,
            c,
        ),
        ast::Expression::FieldAccess(it) => {
            let aggregate_ty = of(it.aggregate(), c)?;
            if aggregate_ty == Type::Struct {
                todo!()
            } else {
                c.diagnostics.error(
                    format!("type `{}` has no fields", aggregate_ty.display(&c.interner)),
                    [primary(it.syntax().span(), "")],
                );
                None
            }
        }
    }
}

fn return_ty(function: ast::Function, c: &Checker) -> Option<Type> {
    let Some(ty) = function.return_ty() else {
        return Some(Type::Unit);
    };
    c.type_expressions.get(&ty.syntax().span()).copied()
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

fn do_not_ignore(ty: Option<Type>, span: Span, c: &mut Checker) {
    if let Some(ty) = ty
        && ty != Type::Unit
    {
        c.diagnostics.error(
            format!("value of type `{}` is ignored", ty.display(&c.interner)),
            [primary(span, "")],
        );
    }
}

fn expect(expression: ast::Expression, expected_ty: Type, c: &mut Checker) {
    if let Some(ty) = of(expression, c)
        && ty != expected_ty
    {
        c.diagnostics.error(
            "type mismatch",
            [primary(
                expression.syntax().span(),
                format!(
                    "expected `{}`, got `{}`",
                    expected_ty.display(&c.interner),
                    ty.display(&c.interner)
                ),
            )],
        );
    }
}

struct Interner(Vec<Type>);

impl Interner {
    fn intern(&mut self, ty: Type) -> Id {
        if let Some(index) = self.0.iter().position(|&it| it == ty) {
            Id(index)
        } else {
            let index = self.0.len();
            self.0.push(ty);
            Id(index)
        }
    }

    fn get(&self, id: Id) -> Type {
        self.0[id.0]
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Id(usize);
