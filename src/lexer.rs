use token::{Token, TokenType};

use crate::Span;

#[derive(Debug, Clone)]
pub struct Lexer<'t> {
    input: &'t str,
    idx: Option<usize>,
    end: usize,
}

impl<'t> Lexer<'t> {
    pub fn new(input: &'t str) -> Self {
        Lexer {
            input,
            idx: Some(0),
            end: input.len(),
        }
    }

    pub fn new_spanned(input: &'t str, span: Span) -> Self {
        Lexer {
            input,
            idx: Some(span.start),
            end: span.end,
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        let span = self.idx?;
        self.idx = self
            .input
            .get(span..)?
            .char_indices()
            .nth(1)
            .map(|(idx, _)| span + idx);
        Some(c)
    }

    fn peek_char(&mut self) -> Option<char> {
        self.input[self.idx?..].chars().next()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    fn read_id(&mut self) -> Option<&'t str> {
        let mut end = self.idx?;
        let start = end;

        for (idx, ch) in self.input[start..].char_indices() {
            if !ch.is_alphanumeric() && ch != '_' {
                end = start + idx;
                break;
            }
        }

        match self.input.get(start..end) {
            Some(id) => {
                self.idx = Some(end);
                Some(id)
            }
            None => {
                self.idx = None;
                None
            }
        }
    }

    fn read_number(&mut self) -> Option<&'t str> {
        let mut end = self.idx?;
        let start = end;

        for (idx, ch) in self.input[start..].char_indices() {
            if !is_number(ch) {
                end = start + idx;
                break;
            }
        }

        match self.input.get(start..end) {
            Some(num) => {
                self.idx = Some(end);
                Some(num)
            }
            None => {
                self.idx = None;
                None
            }
        }
    }
}

impl<'t> Iterator for Lexer<'t> {
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let idx = self.idx?;
        if idx >= self.end {
            return None;
        }

        let mut chars = self.input[idx..].char_indices().peekable();
        let ch = chars.peek()?.1;

        if let Some(mut tok) = Token::from_char(ch, 0..0) {
            chars.next()?;
            self.idx = chars.peek().map(|(idy, _)| idx + idy);
            let span = match self.idx {
                Some(end) => idx..end,
                None => idx..self.input.len(),
            };
            *tok.span_mut() = span;
            Some(tok)
        } else if is_ident(ch) {
            let start = self.idx.unwrap_or_else(|| self.input.len());
            let id = self.read_id()?;
            let end = self.idx.unwrap_or_else(|| self.input.len());
            Some(Token::lookup_ident(id, start..end))
        } else if is_number(ch) {
            let start = self.idx.unwrap_or_else(|| self.input.len());
            let num = self.read_number()?;
            let end = self.idx.unwrap_or_else(|| self.input.len());
            if let Ok(i) = num.parse::<i32>() {
                Some(Token {
                    token_type: TokenType::Int(i),
                    span: start..end,
                })
            } else if let Ok(f) = num.parse::<f32>() {
                Some(Token {
                    token_type: TokenType::Real(f),
                    span: start..end,
                })
            } else {
                Some(TokenType::Unknown.into())
            }
        } else {
            None
        }
    }
}

fn is_ident(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_number(ch: char) -> bool {
    ch.is_numeric() || ch == '.'
}

#[test]
fn is_letter_test() {
    assert!(is_ident('_'));
    assert!(is_ident('a'));
    assert!(is_ident('Z'));

    assert!(!is_ident('*'));
    assert!(!is_ident('1'));
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

    println!("{:?}", Lexer::new(source).collect::<Vec<_>>());
}
