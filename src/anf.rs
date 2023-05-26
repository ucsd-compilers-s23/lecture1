pub enum FlatVal {
    Num(i32),
    True,
    False,
    Id(String),
}

pub enum FlatOp {
    Add1(Box<FlatVal>),
    Sub1(Box<FlatVal>),
    Plus(Box<FlatVal>, Box<FlatVal>),
    Minus(Box<FlatVal>, Box<FlatVal>),
    Eq(Box<FlatVal>, Box<FlatVal>),
    Lt(Box<FlatVal>, Box<FlatVal>),

    Print(Box<FlatVal>),
    Set(String, Box<FlatVal>),

    Call1(String, Box<FlatVal>),
    Call2(String, Box<FlatVal>, Box<FlatVal>),

    Pair(Box<FlatVal>, Box<FlatVal>),
    Fst(Box<FlatVal>),
    Snd(Box<FlatVal>),
    SetFst(Box<FlatVal>, Box<FlatVal>),
    SetSnd(Box<FlatVal>, Box<FlatVal>),

    Break(Box<FlatVal>),
    Loop(Box<FlatBlock>),

    If(Box<FlatVal>, Box<FlatBlock>, Box<FlatBlock>),

    Val(Box<FlatVal>),
}

pub enum FlatBlock {
    Let(String, Box<FlatOp>, Box<FlatBlock>),
    Block(Vec<FlatBlock>),
    Op(Box<FlatOp>),
}

pub struct FlatProgram {
    pub defs: Vec<FlatDefinition>,
    pub main: FlatBlock,
}

pub enum FlatDefinition {
    Fun1(String, String, FlatBlock),
    Fun2(String, String, String, FlatBlock),
}
