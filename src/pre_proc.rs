use crate::Span;
use regex::Regex;
use std::{collections::VecDeque, fmt, iter::FromIterator};

#[derive(Debug, Clone)]
pub struct ProcStr<'t> {
    pub input: &'t str,
    inner: VecDeque<Span>,
}

impl<'t> ProcStr<'t> {
    pub fn combine<F: Fn(&'t str) -> Self>(self, f: F) -> Self {
        let Self { input, inner } = self;

        let inner = inner.into_iter().flat_map(|span| {
            f(&input[span.clone()]).inner.into_iter().map(move |new_span| {
                Span { start: new_span.start + span.start, end: new_span.end + span.start }
            })
        }).collect();

        Self { input, inner }
    }

    pub fn comments(input: &'t str) -> Self {
        Self::split_by_regex(input, Regex::new(r"//.*").unwrap())
    }

    pub fn includes(input: &'t str) -> Self {
        Self::split_by_regex(input, Regex::new("include\\s+\"(.*)\";").unwrap())
    }

    fn split_by_regex(input: &'t str, regex: Regex) -> Self {
        let mut prev_end = 0;
        let mut processed = VecDeque::new();
        for comment in regex.captures_iter(input) {
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

    let source = crate::pre_process(source);
    let comments = crate::lex(source);

    assert_eq!(comments, no_comments);
}
