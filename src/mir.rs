pub mod linearity;
mod lowering;

pub use lowering::lower;

use crate::either::Either;
use map::{Id, Map};
use std::collections::HashMap;

#[derive(Default)]
pub struct Program<'src> {
    pub functions: HashMap<Id<BasicBlock>, Function<'src>>,
    pub basic_blocks: Map<BasicBlock>,
    pub ops: Map<Op>,
    pub variables: Map<Variable>,
    pub lists: Map<List>,
}

#[derive(Clone, Copy)]
pub enum Function<'src> {
    WhenFlagClicked,
    WhenKeyPressed {
        key: codemap::Pos,
    },
    WhenCloned,
    WhenReceived {
        message: codemap::Pos,
    },
    Normal {
        name: &'src str,
        parameter_count: usize,
        return_value_count: usize,
    },
}

impl Function<'_> {
    pub const fn return_value_count(self) -> Option<usize> {
        match self {
            Self::Normal {
                return_value_count, ..
            } => Some(return_value_count),
            _ => None,
        }
    }
}

pub struct BasicBlock(pub Vec<Id<Op>>);

pub enum Op {
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
    For {
        variable: Id<Variable>,
        times: Value,
        body: Id<BasicBlock>,
    },
    Forever(Id<BasicBlock>),
    If {
        condition: Value,
        then: Id<BasicBlock>,
        r#else: Id<BasicBlock>,
    },
    Return {
        function: Id<BasicBlock>,
        values: Vec<Value>,
    },

    Call {
        function: Id<BasicBlock>,
        arguments: Vec<Value>,
    },

    DeleteAll(Id<List>),
    Push {
        list: Id<List>,
        value: Value,
    },

    Intrinsic {
        name: codemap::Span,
        arguments: Vec<Value>,
    },
}

impl Op {
    fn arguments(&self) -> impl Iterator<Item = &Value> {
        use Either::{Left, Right};

        match self {
            Self::Store {
                target: Ref::List { index, .. },
                value,
            } => Right([index, value].into_iter()),
            Self::Store { value: arg, .. }
            | Self::Load {
                source: Ref::List { index: arg, .. },
            }
            | Self::Repeat {
                times: arg,
                body: _,
            }
            | Self::For {
                variable: _,
                times: arg,
                body: _,
            }
            | Self::If {
                condition: arg,
                then: _,
                r#else: _,
            }
            | Self::Push {
                list: _,
                value: arg,
            } => Left(std::slice::from_ref(arg).iter()),
            Self::Load { .. } | Self::Forever(_) | Self::DeleteAll(_) => {
                Left(std::slice::Iter::default())
            }
            Self::Return {
                function: _,
                values,
            } => Left(values.iter()),
            Self::Call {
                function: _,
                arguments,
            }
            | Self::Intrinsic { name: _, arguments } => Left(arguments.iter()),
        }
    }

    fn arguments_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        use Either::{Left, Right};

        match self {
            Self::Store {
                target: Ref::List { index, .. },
                value,
            } => Right([index, value].into_iter()),
            Self::Store { value: arg, .. }
            | Self::Load {
                source: Ref::List { index: arg, .. },
            }
            | Self::Repeat {
                times: arg,
                body: _,
            }
            | Self::For {
                variable: _,
                times: arg,
                body: _,
            }
            | Self::If {
                condition: arg,
                then: _,
                r#else: _,
            }
            | Self::Push {
                list: _,
                value: arg,
            } => Left(std::slice::from_mut(arg).iter_mut()),
            Self::Load { .. } | Self::Forever(_) | Self::DeleteAll(_) => {
                Left(std::slice::IterMut::default())
            }
            Self::Return {
                function: _,
                values,
            } => Left(values.iter_mut()),
            Self::Call {
                function: _,
                arguments,
            }
            | Self::Intrinsic { name: _, arguments } => Left(arguments.iter_mut()),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Value {
    FunctionParameter { index: usize },
    Op(Id<Op>),
    Returned { call: Id<Op>, index: usize },
    Constant(Constant),
}

#[derive(Clone, Copy)]
pub enum Constant {
    Num(f64),
    String(codemap::Pos),
    Bool(bool),
}

impl Constant {
    const PLACEHOLDER: Self = Self::Num(0.0);
}

#[derive(Clone, Copy)]
pub enum Ref {
    Variable(Id<Variable>),
    List { list: Id<List>, index: Value },
}

pub struct Variable {
    pub value: Constant,
}

#[derive(Default)]
pub struct List {
    pub items: Vec<Constant>,
}
