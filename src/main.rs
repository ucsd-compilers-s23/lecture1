use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;

enum Expr {
    Num(i32),
    True,
    False,
    Add1(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Id(String),
    Eq(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Block(Vec<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>)
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::Add1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::Plus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "break" => {
                Expr::Break(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::Eq(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(keyword)), Sexp::List(vec), body] if keyword == "let" => match &vec[..] {
                [Sexp::Atom(S(name)), val] => Expr::Let(
                    name.to_string(),
                    Box::new(parse_expr(val)),
                    Box::new(parse_expr(body)),
                ),
                _ => panic!("parse error"),
            },
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),

            _ => panic!("parse error: {}", s),
        },
        _ => panic!("parse error"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

#[derive(Clone,Copy)]
enum REG {
  RAX,
  RSP,
  RDI,
}

#[derive(Clone,Copy)]
enum Loc {
    LReg(REG),
    LStack(i32)
}


#[derive(Clone,Copy)]
enum Val {
    VReg(REG),
    VStack(i32),
    VImm(i32)
}

use REG::*;
use Loc::*;
use Val::*;

struct Context<'a> {
    si: i32,
    env: &'a HashMap<String, Loc>,
    brake: &'a str,
    target: Loc
}

fn reg_to_str(r : &REG) -> String {
    match r {
        RAX => String::from("rax"),
        RSP => String::from("rsp"),
        RDI => String::from("rdi")
    }
}

fn val_to_str(v : &Val) -> String {
    match v {
        VStack(n) => {
            let offset = n * 8;
            format!("qword [rsp - {offset}]")
        }
        VReg(r) => reg_to_str(r),
        VImm(n) => format!("{}", n)
    }
}

fn mov_target(dest : &Loc, source : &Val) -> String {
    match (dest, source) {
        (LStack(n), VStack(_m)) => {
            format!("
              mov rax, {}
              mov {}, rax
            ",
            val_to_str(source), val_to_str(&VStack(*n)))
        },
        (LReg(r1), _) => format!("mov {}, {}", reg_to_str(r1), val_to_str(source)),
        (LStack(n), _) => format!("mov {}, {}", val_to_str(&VStack(*n)), val_to_str(source))
    }
}

fn compile_expr(e: &Expr, c : &Context, l: &mut i32) -> String {
    match e {
        Expr::Num(n) => mov_target(&c.target, &VImm(*n << 1)),
        Expr::True => mov_target(&c.target, &VImm(3)),
        Expr::False => mov_target(&c.target, &VImm(1)),
        Expr::Id(s) if s == "input" => mov_target(&c.target, &VReg(RDI)),
        Expr::Id(s) => {
            match c.env.get(s).unwrap() {
                LReg(reg) => format!("mov rax, {}", reg_to_str(reg)),
                LStack(offset) => {
                    mov_target(&c.target, &VStack(*offset))
                }
            }
        }
        Expr::Set(name, val) => {
            let target = c.env.get(name).unwrap();
            let nctxt = Context { target: LReg(RAX), ..*c };
            let val_is = compile_expr(val, &nctxt, l);
            let save = mov_target(&target, &VReg(RAX));
            format!("
              {val_is}
              {save}
              ")
        }
        Expr::Add1(subexpr) => compile_expr(subexpr, c, l) + "\nadd rax, 1",
        Expr::Break(e) => {
            let nctxt = Context { target: LReg(RAX), ..*c };
            let e_is = compile_expr(e, &nctxt, l);
            format!("
              {e_is}
              jmp {}
            ", c.brake)
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_expr(e, &Context { brake: &endloop, ..*c }, l);
            let save = mov_target(&c.target, &VReg(RAX));
            format!("
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
              {save}
            ")
        }
        Expr::Block(es) => {
            // could consider writing all but last into RAX or something
            es.into_iter().map(|e| { compile_expr(e, c, l) }).collect::<Vec<String>>().join("\n")
        }
        Expr::Eq(e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let offset = c.si * 8;
            format!(
                "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
            "
            )
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_ctxt = Context { target: LReg(RAX), ..*c };
            let cond_instrs = compile_expr(cond, &cond_ctxt, l);
            let thn_instrs = compile_expr(thn, c, l); // note original context, so store to wherever caller wants
            let els_instrs = compile_expr(els, c, l);
            format!(
                "
              {cond_instrs}
              cmp rax, 1
              je {else_label}
                {thn_instrs}
              {else_label}:
                {els_instrs}
              {end_label}:
           "
            )
        }
        Expr::Plus(e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let stack_offset = c.si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              jnz error
              mov [rsp - {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              jnz error
              add rax, [rsp - {stack_offset}]
          "
            )
        }
        Expr::Let(name, val, body) => {
            let val_ctxt = Context { target: LStack(c.si), ..*c };
            let val_is = compile_expr(val, &val_ctxt, l);
            let body_is = compile_expr(body, &Context { si: c.si + 1, env: &c.env.update(name.to_string(), LStack(c.si)), ..*c}, l);
            let offset = c.si * 8;
            format!(
                "
              {val_is}
              {body_is}
          "
            )
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let mut labels = 0;
    let context = Context { si: 2, env: &HashMap::new(), brake: "", target: LReg(RAX) };
    let result = compile_expr(&expr, &context, &mut labels);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern error
throw_error:
  mov rdi, rbx
  push rsp
  call error
  ret
our_code_starts_here:
  {}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
