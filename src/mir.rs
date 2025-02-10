mod dce;
mod sroa;

use beach_map::{BeachMap, Id};
use std::collections::HashMap;

struct Program {
    parameters: BeachMap<Parameter>,
    parameter_owners: HashMap<Id<Parameter>, Id<BasicBlock>>,
    returns: BeachMap<Return>,
    struct_types: BeachMap<Type>,
    basic_blocks: BeachMap<BasicBlock>,
    ops: BeachMap<Op>,
    variables: BeachMap<Variable>,
    lists: BeachMap<List>,
}

#[derive(Clone, Copy)]
struct Parameter {
    r#type: Id<Type>,
}

impl From<Parameter> for Id<Type> {
    fn from(value: Parameter) -> Self {
        value.r#type
    }
}

impl From<Id<Type>> for Parameter {
    fn from(value: Id<Type>) -> Self {
        Self { r#type: value }
    }
}

#[derive(Clone, Copy)]
struct Return {
    r#type: Id<Type>,
}

impl From<Return> for Id<Type> {
    fn from(value: Return) -> Self {
        value.r#type
    }
}

impl From<Id<Type>> for Return {
    fn from(value: Id<Type>) -> Self {
        Self { r#type: value }
    }
}

struct Type {
    fields: Vec<Id<Type>>,
}

struct BasicBlock(Vec<Id<Op>>);

enum Op {
    Load {
        r#type: Id<Type>,
        source: Ref,
    },
    Store {
        target: Ref,
        value: Value,
    },

    Construct(Vec<Value>),
    Extract {
        r#struct: Value,
        index: usize,
    },

    Repeat {
        times: Value,
        body: Id<BasicBlock>,
    },
    Forever(Id<BasicBlock>),
    If {
        condition: Value,
        then: Id<BasicBlock>,
        r#else: Id<BasicBlock>,
    },
    Return(HashMap<Id<Return>, Value>),

    Call {
        function: Id<BasicBlock>,
        arguments: HashMap<Id<Parameter>, Value>,
    },

    Add([Value; 2]),
}

impl Op {
    fn args_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        use Either::{Left, Right};

        match self {
            Self::Store {
                target: Ref::List { index, .. },
                value,
            } => Right(Right([index, value].into_iter())),
            Self::Store { value: arg, .. }
            | Self::Load {
                r#type: _,
                source: Ref::List { index: arg, .. },
            }
            | Self::Extract {
                r#struct: arg,
                index: _,
            }
            | Self::Repeat {
                times: arg,
                body: _,
            }
            | Self::If {
                condition: arg,
                then: _,
                r#else: _,
            } => Left(Left(std::slice::from_mut(arg).iter_mut())),
            Self::Load { .. } | Self::Forever(_) => Left(Left(std::slice::IterMut::default())),
            Self::Add(args) => Left(Left(args.iter_mut())),
            Self::Construct(args) => Left(Left(args.iter_mut())),
            Self::Return(args) => Left(Right(args.values_mut())),
            Self::Call {
                function: _,
                arguments,
            } => Right(Left(arguments.values_mut())),
        }
    }

    fn refs_mut(&mut self) -> impl Iterator<Item = &mut Ref> {
        match self {
            Self::Load { source, .. } => Some(source),
            Self::Store { target, .. } => Some(target),
            _ => None,
        }
        .into_iter()
    }
}

#[derive(Clone, Copy)]
enum Value {
    FunctionParameter(Id<Parameter>),
    Op(Id<Op>),
    Returned { call: Id<Op>, id: Id<Return> },
    Num(f64),
    String(&'static str),
    Bool(bool),
}

#[derive(Clone, Copy)]
enum Ref {
    Variable(Id<Variable>),
    List { list: Id<List>, index: Value },
}

#[derive(Clone, Copy)]
struct Variable {
    r#type: Id<Type>,
}

impl From<Variable> for Id<Type> {
    fn from(value: Variable) -> Self {
        value.r#type
    }
}

impl From<Id<Type>> for Variable {
    fn from(value: Id<Type>) -> Self {
        Self { r#type: value }
    }
}

#[derive(Clone, Copy)]
struct List {
    r#type: Id<Type>,
}

impl From<List> for Id<Type> {
    fn from(value: List) -> Self {
        value.r#type
    }
}

impl From<Id<Type>> for List {
    fn from(value: Id<Type>) -> Self {
        Self { r#type: value }
    }
}

enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<I, L: Iterator<Item = I>, R: Iterator<Item = I>> Iterator for Either<L, R> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Left(it) => it.next(),
            Self::Right(it) => it.next(),
        }
    }
}
