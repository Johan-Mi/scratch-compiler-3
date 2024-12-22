use slotmap::SlotMap;

#[derive(Debug)]
struct Program {
    functions: Vec<Function>,
    basic_blocks: SlotMap<BasicBlockId, BasicBlock>,
    ops: SlotMap<OpId, Op>,
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

    /// Reference for `Op::Load` and `Op::Store`.
    Index {
        list: ListId,
        index: Value,
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
    Return(Value),

    Add(Value, Value),
}

#[derive(Debug)]
enum Value {
    Op(OpId),
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
