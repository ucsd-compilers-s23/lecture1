#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here() -> i64;
}

fn print(val : i64) {
  if val == 3 { println!("true"); }
  if val == 1 { println!("false"); }
  if val % 2 == 0 { println!("{}", val >> 1); }
  else {
    println!("Unknown value: {}", val);
  }
}

fn main() {
    let i : i64 = unsafe { our_code_starts_here()  };
    println!("{}", i);
}


