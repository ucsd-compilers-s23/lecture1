use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::{hashmap, HashMap};

struct Program {
    defs: Vec<Definition>,
    main: Expr,
}

enum Definition {
    Fun1(String, String, Expr),
    Fun2(String, String, String, Expr),
}

use Definition::*;

enum Expr {
    Num(i32),
    True,
    False,
    Add1(Box<Expr>),
    Sub1(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Id(String),
    Eq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Block(Vec<Expr>),
    Break(Box<Expr>),
    Print(Box<Expr>),
    Set(String, Box<Expr>),

    Call1(String, Box<Expr>),
    Call2(String, Box<Expr>, Box<Expr>),

    Pair(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),
    SetFst(Box<Expr>, Box<Expr>),
    SetSnd(Box<Expr>, Box<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::Add1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::Sub1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "fst" => Expr::Fst(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "snd" => Expr::Snd(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::Plus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::Minus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "pair" => {
                Expr::Pair(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "setfst!" => {
                Expr::SetFst(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "setsnd!" => {
                Expr::SetSnd(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::Eq(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::Lt(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
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
            [Sexp::Atom(S(funname)), arg] => {
                Expr::Call1(funname.to_string(), Box::new(parse_expr(arg)))
            }
            [Sexp::Atom(S(funname)), arg1, arg2] => Expr::Call2(
                funname.to_string(),
                Box::new(parse_expr(arg1)),
                Box::new(parse_expr(arg2)),
            ),

            _ => panic!("parse error: {}", s),
        },
        _ => panic!("parse error"),
    }
}

fn is_def(s: &Sexp) -> bool {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_definition(s: &Sexp) -> Definition {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => {
                match &name_vec[..] {
                    [Sexp::Atom(S(funname)), Sexp::Atom(S(arg))] => {
                        Fun1(funname.to_string(), arg.to_string(), parse_expr(body))
                    }
                    [Sexp::Atom(S(funname)), Sexp::Atom(S(arg1)), Sexp::Atom(S(arg2))] => Fun2(
                        funname.to_string(),
                        arg1.to_string(),
                        arg2.to_string(),
                        parse_expr(body),
                    ),
                    _ => panic!("Bad fundef"),
                }
            }
            _ => panic!("Bad fundef"),
        },
        _ => panic!("Bad fundef"),
    }
}

fn parse_program(s: &Sexp) -> Program {
    match s {
        Sexp::List(vec) => {
            let mut defs: Vec<Definition> = vec![];
            for def_or_exp in vec {
                if is_def(def_or_exp) {
                    defs.push(parse_definition(def_or_exp));
                } else {
                    return Program {
                        defs: defs,
                        main: parse_expr(def_or_exp),
                    };
                }
            }
            panic!("Only found definitions");
        }
        _ => panic!("Program should be a list"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    brake: &String,
    l: &mut i32,
) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n << 1),
        Expr::True => format!("mov rax, {}", 3),
        Expr::False => format!("mov rax, {}", 1),
        Expr::Id(s) if s == "input" => format!("mov rax, rdi"),
        Expr::Id(s) if s == "nil" => format!("mov rax, 0x1"),
        Expr::Id(s) => {
            let offset = env.get(s).unwrap() * 8;
            format!("mov rax, [rsp + {offset}]")
        }
        Expr::Print(e) => {
            let e_is = compile_expr(e, si, env, brake, l);
            let index = if si % 2 == 1 { si + 2 } else { si + 1 };
            let offset = index * 8;
            format!(
                "
            {e_is}
            sub rsp, {offset}
            mov [rsp], rdi
            mov rdi, rax
            call snek_print
            mov rdi, [rsp]
            add rsp, {offset}
          "
            )
        }
        Expr::Set(name, val) => {
            let offset = env.get(name).unwrap() * 8;

            let save = format!("mov [rsp + {offset}], rax");
            let val_is = compile_expr(val, si, env, brake, l);
            format!(
                "
              {val_is}
              {save}
              "
            )
        }
        Expr::Add1(subexpr) => compile_expr(subexpr, si, env, brake, l) + "\nadd rax, 2",
        Expr::Sub1(subexpr) => compile_expr(subexpr, si, env, brake, l) + "\nsub rax, 2",
        Expr::Break(e) => {
            let e_is = compile_expr(e, si, env, brake, l);
            format!(
                "
              {e_is}
              jmp {brake}
            "
            )
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_expr(e, si, env, &endloop, l);
            format!(
                "
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
            "
            )
        }
        Expr::Block(es) => es
            .into_iter()
            .map(|e| compile_expr(e, si, env, brake, l))
            .collect::<Vec<String>>()
            .join("\n"),
        Expr::Lt(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp + {offset}], rax
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp + {offset}]
                test rbx, 1
                mov rbx, 7
                jne throw_error
                cmp rax, [rsp + {offset}]
                mov rbx, 3
                mov rax, 1
                cmovg rax, rbx
            "
            )
        }
        Expr::Eq(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp + {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp + {offset}]
                test rbx, 1
                mov rbx, 7
                jne throw_error
                cmp rax, [rsp + {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
            "
            )
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_expr(cond, si, env, brake, l);
            let thn_instrs = compile_expr(thn, si, env, brake, l);
            let els_instrs = compile_expr(els, si, env, brake, l);
            format!(
                "
              {cond_instrs}
              cmp rax, 1
              je {else_label}
                {thn_instrs}
                jmp {end_label}
              {else_label}:
                {els_instrs}
              {end_label}:
           "
            )
        }
        Expr::Minus(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              mov [rsp + {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              sub rax, [rsp + {stack_offset}]
          "
            )
        }
        Expr::Plus(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              mov [rsp + {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              add rax, [rsp + {stack_offset}]
          "
            )
        }
        Expr::Let(name, val, body) => {
            let val_is = compile_expr(val, si, env, brake, l);
            let body_is = compile_expr(body, si + 1, &env.update(name.to_string(), si), brake, l);
            let offset = si * 8;
            format!(
                "
              {val_is}
              mov [rsp + {offset}], rax
              {body_is}
          "
            )
        }
        Expr::Call1(name, arg) => {
            let arg_is = compile_expr(arg, si, env, brake, l);
            let offset = 2 * 8; // one extra word for rdi saving, one for arg
            format!(
                "
                {arg_is}
                sub rsp, {offset}
                mov [rsp], rax
                mov [rsp+8], rdi
                call {name}
                mov rdi, [rsp+8]
                add rsp, {offset}
            "
            )
        }
        Expr::Call2(name, arg1, arg2) => {
            let arg1_is = compile_expr(arg1, si, env, brake, l);
            let arg2_is = compile_expr(arg2, si + 1, env, brake, l);
            let curr_word = si * 8;
            let offset = 3 * 8;
            let curr_word_after_sub = offset + curr_word;
            // With this setup, the current word will be at [rsp+16], which is where arg1 is stored
            // We then want to get rdi at [rsp+16], arg2 at [rsp+8], and arg1 at [rsp], then call
            format!(
                "
                {arg1_is}
                mov [rsp + {curr_word}], rax
                {arg2_is}
                sub rsp, {offset}
                mov rbx, [rsp+{curr_word_after_sub}]
                mov [rsp], rbx
                mov [rsp+8], rax
                mov [rsp+16], rdi
                call {name}
                mov rdi, [rsp+16]
                add rsp, {offset}
            "
            )
        }

        Expr::Pair(e1, e2) => {
            let e1is = compile_expr(e1, si, env, brake, l);
            let e2is = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
                {e1is}
                mov [rsp + {stack_offset}], rax
                {e2is}
                mov [r15+8], rax
                mov rax, [rsp + {stack_offset}]
                mov [r15], rax
                mov rax, r15
                add rax, 1
                add r15, 16
            "
            )
        }

        Expr::Fst(e) => {
            let eis = compile_expr(e, si, env, brake, l);
            format!(
                "
                {eis}
                mov rax, [rax-1]
            "
            )
        }

        Expr::Snd(e) => {
            let eis = compile_expr(e, si, env, brake, l);
            format!(
                "
                {eis}
                mov rax, [rax+7]
            "
            )
        }

        Expr::SetFst(e1, e2) => {
            let e1is = compile_expr(e1, si, env, brake, l);
            let e2is = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
                {e1is}
                mov [rsp + {stack_offset}], rax
                {e2is}
                mov rbx, [rsp + {stack_offset}]
                mov [rbx-1], rax
                mov rax, [rsp + {stack_offset}]
            "
            )
        }

        Expr::SetSnd(e1, e2) => {
            let e1is = compile_expr(e1, si, env, brake, l);
            let e2is = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
                {e1is}
                mov [rsp + {stack_offset}], rax
                {e2is}
                mov rbx, [rsp + {stack_offset}]
                mov [rbx+7], rax
                mov rax, [rsp + {stack_offset}]
            "
            )
        }
    }
}

// Generated by ChatGPT
fn depth(e: &Expr) -> i32 {
    match e {
        Expr::Num(_) => 0,
        Expr::True => 0,
        Expr::False => 0,
        Expr::Add1(expr) => depth(expr),
        Expr::Sub1(expr) => depth(expr),
        Expr::Plus(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Minus(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Let(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Id(_) => 0,
        Expr::Lt(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Eq(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::If(expr1, expr2, expr3) => depth(expr1).max(depth(expr2)).max(depth(expr3)),
        Expr::Loop(expr) => depth(expr),
        Expr::Block(exprs) => exprs.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Break(expr) => depth(expr),
        Expr::Print(expr) => depth(expr),
        Expr::Set(_, expr) => depth(expr),
        Expr::Call1(_, expr) => depth(expr),
        Expr::Call2(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Pair(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Fst(expr) => depth(expr),
        Expr::Snd(expr) => depth(expr),
        Expr::SetFst(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::SetSnd(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
    }
}

fn compile_program(p: &Program) -> (String, String) {
    let mut labels: i32 = 0;
    let mut defs: String = String::new();
    for def in &p.defs[..] {
        defs.push_str(&compile_definition(&def, &mut labels));
    }
    let depth = depth(&p.main);
    let offset = depth * 8;
    let main = compile_expr(&p.main, 0, &HashMap::new(), &String::from(""), &mut labels);
    let main_with_offsetting = format!(
        "
        sub rsp, {offset}
        {main}
        add rsp, {offset}
    "
    );
    (defs, main_with_offsetting)
}

fn compile_definition(d: &Definition, labels: &mut i32) -> String {
    match d {
        Fun1(name, arg, body) => {
            let depth = depth(body);
            let offset = depth * 8;
            let body_env = hashmap! {
                arg.to_string() => depth + 1
            };
            let body_is = compile_expr(body, 0, &body_env, &String::from(""), labels);
            format!(
                "{name}:
                sub rsp, {offset}
                {body_is}
                add rsp, {offset}
                ret
            "
            )
        }
        Fun2(name, arg1, arg2, body) => {
            let depth = depth(body);
            let offset = depth * 8;
            let body_env = hashmap! {
                arg1.to_string() => depth + 1,
                arg2.to_string() => depth + 2
            };
            let body_is = compile_expr(body, 0, &body_env, &String::from(""), labels);
            format!(
                "{name}:
                sub rsp, {offset}
                {body_is}
                add rsp, {offset}
                ret"
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

    let prog = "(".to_owned() + &in_contents + ")";

    let prog = parse_program(&parse(&prog).unwrap());
    let (defs, main) = compile_program(&prog);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  push rsp
  mov rdi, rbx
  call snek_error
  ret
{}
our_code_starts_here:
  mov r15, rsi
  {}
  ret
",
        defs, main
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
