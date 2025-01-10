mod dce;
mod sroa;

use slotmap::{Key as _, SecondaryMap, SlotMap};

#[derive(Debug)]
struct Program {
    functions: SlotMap<FunctionId, Function>,
    parameters: SlotMap<ParameterId, TypeId>,
    returns: SlotMap<ReturnId, TypeId>,
    struct_types: SlotMap<TypeId, Vec<TypeId>>,
    basic_blocks: SlotMap<BasicBlockId, BasicBlock>,
    ops: SlotMap<OpId, Op>,
    variables: SlotMap<VariableId, TypeId>,
    lists: SlotMap<ListId, TypeId>,
}

slotmap::new_key_type! {
    struct FunctionId;
}

#[derive(Debug)]
struct Function {
    parameters: SecondaryMap<ParameterId, ()>,
    body: BasicBlockId,
}

slotmap::new_key_type! {
    struct ParameterId;

    struct ReturnId;
}

slotmap::new_key_type! {
    struct TypeId;
}

impl TypeId {
    fn scalar() -> Self {
        Self::null()
    }

    fn is_struct(self) -> bool {
        !self.is_null()
    }
}

slotmap::new_key_type! {
    struct BasicBlockId;
}

#[derive(Debug)]
struct BasicBlock(Vec<OpId>);

slotmap::new_key_type! {
    struct OpId;
}

#[derive(Debug)]
enum Op {
    Load {
        r#type: TypeId,
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
        body: BasicBlockId,
    },
    Forever(BasicBlockId),
    If {
        condition: Value,
        then: BasicBlockId,
        r#else: BasicBlockId,
    },
    Return(SecondaryMap<ReturnId, Value>),

    Call {
        function: FunctionId,
        arguments: SecondaryMap<ParameterId, Value>,
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

#[derive(Debug, Clone, Copy)]
enum Value {
    FunctionParameter(ParameterId),
    Op(OpId),
    Returned { call: OpId, id: ReturnId },
    Num(f64),
    String(&'static str),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
enum Ref {
    Variable(VariableId),
    List { list: ListId, index: Value },
}

slotmap::new_key_type! {
    struct VariableId;

    struct ListId;
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
