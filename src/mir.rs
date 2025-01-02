mod dce;
mod sroa;

use slotmap::{Key as _, SecondaryMap, SlotMap};

#[derive(Debug)]
struct Program {
    functions: SlotMap<FunctionId, Function>,
    parameters: SlotMap<ParameterId, TypeId>,
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
        source: Value,
    },
    /// target, value
    Store([Value; 2]),

    Construct(Vec<Value>),
    Extract {
        r#struct: Value,
        index: usize,
    },

    /// Reference for `Op::Load` and `Op::Store`.
    Index {
        list: ListId,
        index: Value,
    },
    /// Reference for `Op::Load` and `Op::Store`.
    Project {
        struct_ref: Value,
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
    Return(Vec<Value>),

    Call {
        function: FunctionId,
        arguments: SecondaryMap<ParameterId, Value>,
    },

    Add([Value; 2]),
}

impl Op {
    fn args_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        match self {
            Self::Forever(_) => Either::Left(std::slice::IterMut::default()),
            Self::Load {
                r#type: _,
                source: arg,
            }
            | Self::Extract {
                r#struct: arg,
                index: _,
            }
            | Self::Index {
                list: _,
                index: arg,
            }
            | Self::Project {
                struct_ref: arg,
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
            } => Either::Left(std::slice::from_mut(arg).iter_mut()),
            Self::Store(args) | Self::Add(args) => Either::Left(args.iter_mut()),
            Self::Construct(args) | Self::Return(args) => Either::Left(args.iter_mut()),
            Self::Call {
                function: _,
                arguments,
            } => Either::Right(arguments.values_mut()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Value {
    FunctionParameter(ParameterId),
    Op(OpId),
    Call {
        op: OpId,
        index: usize,
    },
    Num(f64),
    String(&'static str),
    Bool(bool),
    /// Reference for `Op::Load` and `Op::Store`.
    VariableRef(VariableId),
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
