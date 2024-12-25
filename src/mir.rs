mod sroa;

use slotmap::{Key as _, SlotMap};

#[derive(Debug)]
struct Program {
    functions: SlotMap<FunctionId, Function>,
    struct_types: SlotMap<TypeId, Vec<TypeId>>,
    basic_blocks: SlotMap<BasicBlockId, BasicBlock>,
    ops: SlotMap<OpId, Op>,
}

slotmap::new_key_type! {
    struct FunctionId;
}

#[derive(Debug)]
struct Function {
    body: BasicBlockId,
}

slotmap::new_key_type! {
    struct TypeId;
}

impl TypeId {
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
        r#type: TypeId,
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
        arguments: Vec<Value>,
    },

    Add([Value; 2]),
}
impl Op {
    fn args_mut(&mut self) -> &mut [Value] {
        match self {
            Self::Forever(_) => &mut [],
            Self::Load {
                r#type: _,
                source: arg,
            }
            | Self::Extract {
                r#struct: arg,
                index: _,
            }
            | Self::Index {
                r#type: _,
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
            } => std::slice::from_mut(arg),
            Self::Store(args) | Self::Add(args) => args,
            Self::Construct(args)
            | Self::Return(args)
            | Self::Call {
                function: _,
                arguments: args,
            } => args,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Value {
    FunctionParameter {
        index: usize,
    },
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

#[derive(Debug, Clone, Copy)]
struct VariableId(&'static str);

#[derive(Debug)]
struct ListId(&'static str);
