use slotmap::SlotMap;

#[derive(Debug)]
struct Program {
    functions: SlotMap<FunctionId, Function>,
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
    struct BasicBlockId;
}

#[derive(Debug)]
struct BasicBlock(Vec<OpId>);

slotmap::new_key_type! {
    struct OpId;
}

#[derive(Debug)]
enum Op {
    Load(Value),
    Store {
        target: Value,
        value: Value,
    },

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
        arguments: Vec<Value>,
    },

    Add(Value, Value),
}

#[derive(Debug)]
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
    String(String),
    Bool(bool),
    /// Reference for `Op::Load` and `Op::Store`.
    VariableRef(VariableId),
}

#[derive(Debug)]
struct VariableId(&'static str);

#[derive(Debug)]
struct ListId(&'static str);
