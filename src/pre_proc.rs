use crate::Span;
use regex::Regex;
use std::{collections::VecDeque, fmt};

#[derive(Debug, Clone)]
pub struct ProcStr<'t> {
    pub input: &'t str,
    inner: VecDeque<Span>,
}

impl<'t> ProcStr<'t> {
    pub fn comments(input: &'t str) -> Self {
        let mut prev_end = 0;
        let mut processed = VecDeque::new();
        for comment in Regex::new(r"//.*").unwrap().captures_iter(input) {
            let m = comment.get(0).unwrap();
            let cut_input = &input[prev_end..m.start()];
            if !cut_input.trim().is_empty() {
                processed.push_back(prev_end..m.start());
            }
            prev_end = m.end();
        }
        let cut_input = &input[prev_end..];
        if !cut_input.trim().is_empty() {
            processed.push_back(prev_end..input.len());
        }
        Self {
            input,
            inner: processed,
        }
    }
}

impl<'t> From<&'t str> for ProcStr<'t> {
    fn from(input: &'t str) -> Self {
        Self {
            input,
            inner: vec![0..input.len()].into(),
        }
    }
}

impl<'t> Iterator for ProcStr<'t> {
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.pop_front()
    }
}

impl<'t> fmt::Display for ProcStr<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.inner {
            write!(f, "{}", &self.input[s.clone()])?;
        }
        Ok(())
    }
}

#[cfg(test)]
#[test]
fn lexer_test() {
    let source = "OPENQASM 2.0;
    gate majority a,b,c 
    { 
      cx c,b; 
      cx c,a; 
      ccx a,b,c; 
    }
    gate unmaj a,b,c 
    { 
      ccx a,b,c; 
      cx c,a; 
      cx a,b; 
    }
    qreg cin[1];
    qreg a[4];
    qreg b[4];
    qreg cout[1];
    creg ans[5];
    x a[0];
    x b;
    majority cin[0],b[0],a[0];
    majority a[0],b[1],a[1];
    majority a[1],b[2],a[2];
    majority a[2],b[3],a[3];
    cx a[3],cout[0];
    unmaj a[2],b[3],a[3];
    unmaj a[1],b[2],a[2];
    unmaj a[0],b[1],a[1];
    unmaj cin[0],b[0],a[0];
    measure b[0] -> ans[0];
    measure b[1] -> ans[1];
    measure b[2] -> ans[2];
    measure b[3] -> ans[3];
    measure cout[0] -> ans[4];";

    let no_comments = crate::lex(source);

    let source = "OPENQASM 2.0;
    // comment
    gate majority a,b,c 
    { 
      cx c,b; 
      cx c,a; 
      ccx a,b,c; 
    }
    gate unmaj a,b,c 
    { 
      ccx a,b,c; 
      cx c,a; 
      cx a,b; 
    }
    qreg cin[1];
    qreg a[4];
    qreg b[4];
    qreg cout[1];
    creg ans[5];
    x a[0];
    x b;

// comment line 1
// comment line 2

    majority cin[0],b[0],a[0];
    majority a[0],b[1],a[1];
    majority a[1],b[2],a[2];
    majority a[2],b[3],a[3];
    cx a[3],cout[0];
    unmaj a[2],b[3],a[3];
    unmaj a[1],b[2],a[2];
    unmaj a[0],b[1],a[1];
    unmaj cin[0],b[0],a[0];
    measure b[0] -> ans[0];
    measure b[1] -> ans[1];
    measure b[2] -> ans[2];
    measure b[3] -> ans[3];
    measure cout[0] -> ans[4];";

    let source = crate::process_comments(source);
    let comments = crate::lex(source);

    assert_eq!(comments, no_comments);
}
