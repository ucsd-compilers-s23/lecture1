use std::env;
#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input : i64, memory : *mut i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub fn snek_error(errcode : i64) {
  eprintln!("An error occurred {}", errcode);
  std::process::exit(1);
}

// let's change snek_str to print ... when it visits a cyclic value
fn snek_str(val : i64, seen : &mut Vec<i64>) -> String {
  if val == 7 { "true".to_string() }
  else if val == 3 { "false".to_string() }
  else if val % 2 == 0 { format!("{}", val >> 1) }
  else if val == 1 { "nil".to_string() }
  else if val & 1 == 1 {
    if seen.contains(&val)  { return "(pair <cyclic>)".to_string() }
    seen.push(val);
    let addr = (val - 1) as *const i64;
    let fst = unsafe { *addr };
    let snd = unsafe { *addr.offset(1) };
    let result = format!("(pair {} {})", snek_str(fst, seen), snek_str(snd, seen));
    seen.pop();
    return result;
  }
  else {
    format!("Unknown value: {}", val)
  }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : i64) -> i64 {
  let mut seen = Vec::<i64>::new();
  println!("{}", snek_str(val, &mut seen));
  return val;
}
fn parse_arg(v : &Vec<String>) -> i64 {
  if v.len() < 2 { return 1 }
  let s = &v[1];
  if s == "true" { 3 }
  else if s == "false" { 1 }
  else { s.parse::<i64>().unwrap() << 1 }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_arg(&args);

    let mut memory = Vec::<i64>::with_capacity(1000000);
    let buffer :*mut i64 = memory.as_mut_ptr();
    
    let i : i64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i);
}


