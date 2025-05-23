mod dce;
mod linearity;
mod lowering;

pub use lowering::lower;

use beach_map::{BeachMap, Id};
use std::collections::HashMap;

pub struct Program {
    parameters: BeachMap<Parameter>,
    basic_blocks: BeachMap<BasicBlock>,
    ops: BeachMap<Op>,
    variables: BeachMap<Variable>,
}

#[derive(Clone, Copy)]
struct Parameter {
    owner: Id<BasicBlock>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Return(u8);

struct BasicBlock(Vec<Id<Op>>);

enum Op {
    Load {
        source: Ref,
    },
    Store {
        target: Ref,
        value: Value,
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
    Return(HashMap<Return, Value>),

    Call {
        function: Id<BasicBlock>,
        arguments: HashMap<Id<Parameter>, Value>,
    },

    Add([Value; 2]),
}

impl Op {
    fn args(&self) -> impl Iterator<Item = &Value> {
        use Either::{Left, Right};

        match self {
            Self::Store {
                target: Ref::List { index, .. },
                value,
            } => Right(Right([index, value].into_iter())),
            Self::Store { value: arg, .. }
            | Self::Load {
                source: Ref::List { index: arg, .. },
            }
            | Self::Repeat {
                times: arg,
                body: _,
            }
            | Self::If {
                condition: arg,
                then: _,
                r#else: _,
            } => Left(Left(std::slice::from_ref(arg).iter())),
            Self::Load { .. } | Self::Forever(_) => Left(Left(std::slice::Iter::default())),
            Self::Add(args) => Left(Left(args.iter())),
            Self::Return(args) => Left(Right(args.values())),
            Self::Call {
                function: _,
                arguments,
            } => Right(Left(arguments.values())),
        }
    }

    fn args_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        use Either::{Left, Right};

        match self {
            Self::Store {
                target: Ref::List { index, .. },
                value,
            } => Right(Right([index, value].into_iter())),
            Self::Store { value: arg, .. }
            | Self::Load {
                source: Ref::List { index: arg, .. },
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
            Self::Return(args) => Left(Right(args.values_mut())),
            Self::Call {
                function: _,
                arguments,
            } => Right(Left(arguments.values_mut())),
        }
    }
}

#[derive(Clone, Copy)]
enum Value {
    FunctionParameter(Id<Parameter>),
    Op(Id<Op>),
    Returned { call: Id<Op>, id: Return },
    Num(f64),
    String(&'static str),
    Bool(bool),
}

#[derive(Clone, Copy)]
enum Ref {
    Variable(Id<Variable>),
    List { list: List, index: Value },
}

struct Variable;

#[derive(Clone, Copy)]
struct List(u16);

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
