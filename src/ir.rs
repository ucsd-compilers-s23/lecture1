#[path = "anf.rs"] mod anf;
use anf::*;

pub enum Val {
    Num(i32),
    True,
    False,
    Id(String)
}

pub enum Expr {
    Add1(Box<Val>),
    Sub1(Box<Val>),
    Plus(Box<Val>, Box<Val>),
    Minus(Box<Val>, Box<Val>),
    Eq(Box<Val>, Box<Val>),
    Lt(Box<Val>, Box<Val>),

    Print(Box<Val>),
    Set(String, Box<Val>),

    Call1(String, Box<Val>),
    Call2(String, Box<Val>, Box<Val>),

    Pair(Box<Val>, Box<Val>),
    Fst(Box<Val>),
    Snd(Box<Val>),
    SetFst(Box<Val>, Box<Val>),
    SetSnd(Box<Val>, Box<Val>),

    Val(Box<Val>)
}
pub enum Step {
    Label(String),
    If(Box<Val>, String),
    Break(String),
    Do(Expr),
    Set(String, Expr)
}


pub struct Block {
    pub steps: Vec<Step>
}

pub enum Def {
    Fun1(String, String, Block),
    Fun2(String, String, String ,Block)
}

pub struct Prog {
    pub defs: Vec<Def>,
    pub main: Block
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

pub fn anf_to_ir(p : &FlatProgram, i : &mut i32) -> Prog {
    let mut defs = Vec::new();
    for def in &p.defs {
        defs.push(match def {
            FlatDefinition::Fun1(name, arg, body) => Def::Fun1(name.clone(), arg.clone(), Block { steps: anf_to_ir_block(body, i) }),
            FlatDefinition::Fun2(name, arg1, arg2, body) => Def::Fun2(name.clone(), arg1.clone(), arg2.clone(), Block { steps: anf_to_ir_block(body, i) }),
        });
    }
    Prog {
        defs: defs,
        main: Block { steps: anf_to_ir_block(&p.main, i) }
    }
}

pub fn anf_to_ir_block(b : &FlatBlock, i : &mut i32) -> Vec<Step> {
    match b {
        FlatBlock::Let(name, op, body) => {
            let (step, mut steps) = anf_to_ir_expr(op, i);
            let mut body = anf_to_ir_block(body, i);
            steps.push(Step::Set(name.clone(), step));
            steps.append(&mut body);
            steps
        },
        FlatBlock::Block(bs) => {
            let mut steps = Vec::new();
            for b in bs {
                let mut innersteps = anf_to_ir_block(b, i);
                steps.append(&mut innersteps);
            }
            steps
        },
        FlatBlock::Op(op) => {
            let (expr, mut steps) = anf_to_ir_expr(op, i);
            steps.insert(0, Step::Do(expr));
            steps
        }
    }
}

fn assign_to_last(steps : &mut Vec<Step>, name : &str) {
    let mut last = steps.pop().unwrap();
    match last {
        Step::Do(expr) => {
            steps.push(Step::Set(name.to_string(), expr));
        },
        _ => panic!("Expected last step to be Do")
    }
}

pub fn anf_to_ir_expr(op : &FlatOp, i : &mut i32) -> (Expr, Vec<Step>) {
    match op {
        FlatOp::If(v, b1, b2) => {
            /* 
                This is the most interesting case of the ANF to IR translation.

                The key case we are considering is

                (let (x (if v b1 b2)) body)

                We want this resulting structure (the inversion of b1 and b2 is
                intentional because the semantics of if is "jump if true"):

                if v thn
                    b2
                    break end
                thn:
                    b1
                end:
                x = ***the answer of either b1 or b2 somehow***
                body

                A key idea is that blocks from our ANF representation always end
                in a FlatBlock::Op (because that's where the inductive structure
                ends), which turns into a Step::Do() in the IR. So we can just
                assign the answer to the last step in b1 and b2, and then use
                that as the answer for the whole block.

                We call it "rax" to be cute, because that dedicated register was
                doing that job for us.
                
             */
            let v = anf_to_ir_val(v);
            let mut b1 = anf_to_ir_block(b1, i);
            let mut b2 = anf_to_ir_block(b2, i);
            let end = new_label(i, "ifend");
            let thn = new_label(i, "thn");
            let ans = new_label(i, "rax");

            let mut steps = Vec::new();
            steps.push(Step::If(Box::new(v), thn.clone()));
            assign_to_last(&mut b1, ans.as_str());
            steps.push(Step::Label(thn.clone()));
            steps.append(&mut b2);
            steps.push(Step::Break(end.clone()));
            steps.append(&mut b1);
            assign_to_last(&mut b2, ans.as_str());
            steps.push(Step::Label(end.clone()));
            (Expr::Val(Box::new(Val::Id(ans))), steps)
        },
        FlatOp::Pair(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            (Expr::Pair(Box::new(v1), Box::new(v2)), vec![])
        },
        _ => todo!()
    }
}

pub fn anf_to_ir_val(v : &FlatVal) -> Val {
    match v {
        FlatVal::Num(n) => Val::Num(*n),
        FlatVal::True => Val::True,
        FlatVal::False => Val::False,
        FlatVal::Id(id) => Val::Id(id.clone())
    }
}