use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;

enum Expr {
    Num(i32),
    True, False,
    Add1(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Id(String),
    Eq(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
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
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => {
                Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(thn)), Box::new(parse_expr(els)))
            },

            _ => panic!("parse error: {}", s),
        },
        _ => panic!("parse error"),
    }
}

fn new_label(l : &mut i32, s : &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}")
}

fn compile_expr(e: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n << 1),
        Expr::True => format!("mov rax, {}", 3),
        Expr::False => format!("mov rax, {}", 1),
        Expr::Id(s) => {
            let offset = env.get(s).unwrap() * 8;
            format!("mov rax, [rsp - {offset}]")
        }
        Expr::Add1(subexpr) => compile_expr(subexpr, si, env, l) + "\nadd rax, 1",
        Expr::Eq(e1, e2) => {
            format!("mov rax, 1")
        },
        Expr::If(cond, thn, els) => {
           let end_label = new_label(l, "ifend"); 
           let else_label = new_label(l, "ifelse"); 
           let cond_instrs = compile_expr(cond, si, env, l);
           let thn_instrs = compile_expr(thn, si, env, l);
           let els_instrs = compile_expr(els, si, env, l);
           format!("
              {cond_instrs}
              cmp rax, 1
              je {else_label}
                {thn_instrs}
              {else_label}:
                {els_instrs}
              {end_label}:
           ")
        }
        Expr::Plus(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, l);
            let e2_instrs = compile_expr(e2, si + 1, env, l);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              mov [rsp - {stack_offset}], rax
              {e2_instrs}
              add rax, [rsp - {stack_offset}]
          "
            )
        }
        Expr::Let(name, val, body) => {
            let val_is = compile_expr(val, si, env, l);
            let body_is = compile_expr(body, si + 1, &env.update(name.to_string(), si), l);
            let offset = si * 8;
            format!(
                "
              {val_is}
              mov [rsp - {offset}], rax
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
    let result = compile_expr(&expr, 2, &HashMap::new(), &mut labels);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
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
