use crate::{
    anf::*
};

pub enum Val {
    Num(i32),
    True,
    False,
    Id(String),
}

pub enum Expr {
    Add1(Box<Val>),
    Sub1(Box<Val>),
    Plus(Box<Val>, Box<Val>),
    Minus(Box<Val>, Box<Val>),
    Eq(Box<Val>, Box<Val>),
    Lt(Box<Val>, Box<Val>),

    Print(Box<Val>),

    Call1(String, Box<Val>),
    Call2(String, Box<Val>, Box<Val>),

    Pair(Box<Val>, Box<Val>),
    Fst(Box<Val>),
    Snd(Box<Val>),
    SetFst(Box<Val>, Box<Val>),
    SetSnd(Box<Val>, Box<Val>),

    Val(Box<Val>),
}
pub enum Step {
    Label(String),
    If(Box<Val>, String, String),
    Goto(String),
    Do(Expr),
    Set(String, Expr),
}

pub struct Block {
    pub steps: Vec<Step>,
}

pub enum Def {
    Fun1(String, String, Block),
    Fun2(String, String, String, Block),
}

pub struct Prog {
    pub defs: Vec<Def>,
    pub main: Block,
}


pub fn ir_to_string(p : &Prog) -> String {
    let mut s = String::new();
    for def in &p.defs {
        match def {
            Def::Fun1(name, arg, body) => {
                s.push_str(&format!("{}({}) {{\n", name, arg));
                s.push_str(&block_to_string(body));
                s.push_str("}\n\n");
            }
            Def::Fun2(name, arg1, arg2, body) => {
                s.push_str(&format!("{}({},{}) {{\n", name, arg1, arg2));
                s.push_str(&block_to_string(body));
                s.push_str("}\n\n");
            }
        }
    }
    s.push_str(&block_to_string(&p.main));
    s 
}

pub fn block_to_string(b : &Block) -> String {
    let mut s = String::new();
    for step in &b.steps {
        match step {
            Step::Label(l) => {
                s.push_str(&format!("\n{}:\n", l));
            }
            Step::If(v, l, r) => {
                s.push_str(&format!("if\t{} {} {}\n", val_to_string(v), l, r));
            }
            Step::Goto(l) => {
                s.push_str(&format!("goto\t{}\n", l));
            }
            Step::Do(e) => {
                s.push_str(&format!("{}\n", expr_to_string(e)));
            }
            Step::Set(name, e) => {
                s.push_str(&format!("{}\t<- {}\n", name, expr_to_string(e)));
            }
        }
    }
    s
}

fn expr_to_string(e : &Expr) -> String {
    match e {
        Expr::Add1(v) => format!("add1 {}", val_to_string(v)),
        Expr::Sub1(v) => format!("sub1 {}", val_to_string(v)),
        Expr::Plus(v1, v2) => format!("{} + {}", val_to_string(v1), val_to_string(v2)),
        Expr::Minus(v1, v2) => format!("{} - {}", val_to_string(v1), val_to_string(v2)),
        Expr::Eq(v1, v2) => format!("{} == {}", val_to_string(v1), val_to_string(v2)),
        Expr::Lt(v1, v2) => format!("{} < {}", val_to_string(v1), val_to_string(v2)),
        Expr::Print(v) => format!("print {}", val_to_string(v)),
        Expr::Call1(name, v) => format!("{}({})", name, val_to_string(v)),
        Expr::Call2(name, v1, v2) => format!("{}({},{})", name, val_to_string(v1), val_to_string(v2)),
        Expr::Pair(v1, v2) => format!("pair({}, {})", val_to_string(v1), val_to_string(v2)),
        Expr::Fst(v) => format!("fst {}", val_to_string(v)),
        Expr::Snd(v) => format!("snd {}", val_to_string(v)),
        Expr::SetFst(v1, v2) => format!("setfst {} {}", val_to_string(v1), val_to_string(v2)),
        Expr::SetSnd(v1, v2) => format!("setsnd {} {}", val_to_string(v1), val_to_string(v2)),
        Expr::Val(v) => val_to_string(v),
    }
}

fn val_to_string(v : &Val) -> String {
    match v {
        Val::Num(n) => format!("{}", n),
        Val::True => format!("true"),
        Val::False => format!("false"),
        Val::Id(s) => s.to_string(),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

pub fn anf_to_ir(p: &FlatProgram) -> Prog {
    let mut defs = Vec::new();
    for def in &p.defs {
        let mut i = 0;
        defs.push(match def {
            FlatDefinition::Fun1(name, arg, body) => Def::Fun1(
                name.clone(),
                arg.clone(),
                Block {
                    steps: anf_to_ir_block(body, "rax", "", &mut i),
                },
            ),
            FlatDefinition::Fun2(name, arg1, arg2, body) => Def::Fun2(
                name.clone(),
                arg1.clone(),
                arg2.clone(),
                Block {
                    steps: anf_to_ir_block(body, "rax", "", &mut i),
                },
            ),
        });
    }
    let mut i = 0;
    Prog {
        defs: defs,
        main: Block {
            steps: anf_to_ir_block(&p.main, "rax", "", &mut i),
        },
    }
}

pub fn anf_to_ir_block(b: &FlatBlock, target: &str, brake: &str, i: &mut i32) -> Vec<Step> {
    match b {
        FlatBlock::Let(name, op, body) => {
            let mut steps = anf_to_ir_expr(op, &name[..], brake, i);
            let mut body = anf_to_ir_block(body, target, brake, i);
            steps.append(&mut body);
            steps
        }
        FlatBlock::Block(bs) => {
            let mut steps = Vec::new();
            let mut index = 0;
            for b in bs {
                let target = if index == bs.len() - 1 { target } else { "" };
                index += 1;
                let mut innersteps = anf_to_ir_block(b, target, brake, i);
                steps.append(&mut innersteps);
            }
            steps
        }
        FlatBlock::Op(op) => anf_to_ir_expr(op, target, brake, i),
    }
}

pub fn anf_to_ir_expr(op: &FlatOp, target: &str, brake: &str, i: &mut i32) -> Vec<Step> {
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

               What we end up doing is relying on x being the target of *both*
               subexpressions of the if, so if either evaluates it will assign
               into that variable at the end.

            */
            let v = anf_to_ir_val(v);
            let mut b1 = anf_to_ir_block(b1, target, brake, i);
            let mut b2 = anf_to_ir_block(b2, target, brake, i);
            let end = new_label(i, "ifend");
            let thn = new_label(i, "thn");
            let els = new_label(i, "els");

            let mut steps = Vec::new();
            steps.push(Step::If(Box::new(v), thn.clone(), els.clone()));
            steps.push(Step::Label(thn.clone()));
            steps.append(&mut b1);
            steps.push(Step::Goto(end.clone()));
            steps.push(Step::Label(els.clone()));
            steps.append(&mut b2);
            steps.push(Step::Goto(end.clone()));
            steps.push(Step::Label(end.clone()));
            steps
        }
        FlatOp::Break(v) => {
            let v = anf_to_ir_val(v);
            vec![
                target_step(target, Expr::Val(Box::new(v))),
                Step::Goto(brake.to_string()),
            ]
        }
        FlatOp::Loop(e) => {
            let loop_label = new_label(i, "loop");
            let end_label = new_label(i, "end");
            let mut steps = anf_to_ir_block(e, target, &end_label[..], i);
            steps.insert(0, Step::Label(loop_label.clone()));
            steps.push(Step::Goto(loop_label.clone()));
            steps.push(Step::Label(end_label.clone()));
            steps
        }
        FlatOp::Pair(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(target, Expr::Pair(Box::new(v1), Box::new(v2)))]
        }
        FlatOp::Add1(v) => {
            let v = anf_to_ir_val(v);
            vec![target_step(target, Expr::Add1(Box::new(v)))]
        }
        FlatOp::Sub1(v) => {
            let v = anf_to_ir_val(v);
            vec![target_step(target, Expr::Sub1(Box::new(v)))]
        }
        FlatOp::Plus(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(target, Expr::Plus(Box::new(v1), Box::new(v2)))]
        }
        FlatOp::Minus(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(target, Expr::Minus(Box::new(v1), Box::new(v2)))]
        }
        FlatOp::Eq(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(target, Expr::Eq(Box::new(v1), Box::new(v2)))]
        }
        FlatOp::Lt(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(target, Expr::Lt(Box::new(v1), Box::new(v2)))]
        }
        FlatOp::Set(name, v) => {
            let v = anf_to_ir_val(v);
            vec![
                Step::Set(name.to_string(), Expr::Val(Box::new(v))),
                target_step(target, Expr::Val(Box::new(Val::Id(name.to_string())))),
            ]
        }
        FlatOp::Call1(name, v) => {
            let v = anf_to_ir_val(v);
            vec![target_step(
                target,
                Expr::Call1(name.to_string(), Box::new(v)),
            )]
        }
        FlatOp::Call2(name, v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(
                target,
                Expr::Call2(name.to_string(), Box::new(v1), Box::new(v2)),
            )]
        }
        FlatOp::SetFst(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(
                target,
                Expr::SetFst(Box::new(v1), Box::new(v2)),
            )]
        }
        FlatOp::SetSnd(v1, v2) => {
            let v1 = anf_to_ir_val(v1);
            let v2 = anf_to_ir_val(v2);
            vec![target_step(
                target,
                Expr::SetSnd(Box::new(v1), Box::new(v2)),
            )]
        }
        FlatOp::Fst(v) => {
            let v = anf_to_ir_val(v);
            vec![target_step(target, Expr::Fst(Box::new(v)))]
        }
        FlatOp::Snd(v) => {
            let v = anf_to_ir_val(v);
            vec![target_step(target, Expr::Snd(Box::new(v)))]
        }
        FlatOp::Print(v) => {
            let v = anf_to_ir_val(v);
            vec![target_step(target, Expr::Print(Box::new(v)))]
        }
        FlatOp::Val(v) => vec![target_step(target, Expr::Val(Box::new(anf_to_ir_val(v))))],
    }
}

fn target_step(target: &str, e: Expr) -> Step {
    if target == "" {
        Step::Do(e)
    } else {
        Step::Set(target.to_string(), e)
    }
}

pub fn anf_to_ir_val(v: &FlatVal) -> Val {
    match v {
        FlatVal::Num(n) => Val::Num(*n),
        FlatVal::True => Val::True,
        FlatVal::False => Val::False,
        FlatVal::Id(id) => Val::Id(id.clone()),
    }
}
