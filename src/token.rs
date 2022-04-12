use crate::Span;
use std::fmt;
use std::iter::Peekable;

/// Tokens returned from lexing. Represents a small amount of the source code.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType<'t> {
    /// This token represents an illegal token. This is usually an error in the source code.
    Unknown,

    // Literals
    /// Represents a Real Number
    Real(f32),
    /// Represents an integer
    Int(i32),
    /// Represents an identifier
    Id(&'t str),

    // Other Tokens
    /// The OPENQASM statement
    OpenQASM,
    /// A Semicolon
    Semicolon,
    /// A Comma
    Comma,
    /// A Left Paren `(`
    LParen,
    /// A Left Square Paren `[`
    LSParen,
    /// A Left Curly Paren `{`
    LCParen,
    /// A Right Paren `)`
    RParen,
    /// A Right Square Paren `]`
    RSParen,
    /// A Right Curly Paren `}`
    RCParen,
    /// An Arrow `>`
    Arrow,
    /// An Equals `=`
    Equals,

    // Mathematical Expressions
    /// Plus Sign `+`
    Plus,
    /// Minus Sign `-`
    Minus,
    /// Times Sign `*`
    Times,
    /// Divide Sign `/`
    Divide,
    /// Power Sign `^`
    Power,

    // Built In Gates

    // Operators
    /// Reserved word, `qreg`
    QReg,
    /// Reserved word, `creg`
    CReg,
    /// Reserved word, `barrier`
    Barrier,
    /// Reserved word, `gate`
    Gate,
    /// Reserved word, `measure`
    Measure,
    /// Reserved word, `reset`
    Reset,
    /// Reserved word, `include`
    Include,
    /// Reserved word, `opaque`
    Opaque,
    /// Reserved word, `if`
    If,
}

#[derive(Debug, Clone)]
pub struct Token<'t> {
    pub(crate) token_type: TokenType<'t>,
    pub(crate) span: Span,
}

impl<'t> Token<'t> {
    pub(crate) fn to_tup(self) -> (TokenType<'t>, Span) {
        (self.token_type, self.span)
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }

    pub fn from_char(ch: char, span: Span) -> Option<Self> {
        let token_type = match ch {
            '=' => TokenType::Equals,
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Times,
            '/' => TokenType::Divide,
            '^' => TokenType::Power,
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '(' => TokenType::LParen,
            '[' => TokenType::LSParen,
            '{' => TokenType::LCParen,
            ')' => TokenType::RParen,
            ']' => TokenType::RSParen,
            '}' => TokenType::RCParen,
            '>' => TokenType::Arrow,
            _ => return None,
        };

        Some(Self { token_type, span })
    }

    pub fn lookup_ident(ident: &'t str, span: Span) -> Self {
        let token_type = match ident {
            "qreg" => TokenType::QReg,
            "creg" => TokenType::CReg,
            "barrier" => TokenType::Barrier,
            "gate" => TokenType::Gate,
            "measure" => TokenType::Measure,
            "reset" => TokenType::Reset,
            "include" => TokenType::Include,
            "opaque" => TokenType::Opaque,
            "if" => TokenType::If,
            "OPENQASM" => TokenType::OpenQASM,
            ident => TokenType::Id(ident),
        };

        Self { token_type, span }
    }
}

impl<'t> PartialEq for Token<'t> {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
    }
}

impl<'t> From<TokenType<'t>> for Token<'t> {
    fn from(token_type: TokenType<'t>) -> Self {
        Self {
            token_type,
            span: 0..0,
        }
    }
}

impl<'t> Default for Token<'t> {
    /// Choose the Illegal token as default
    fn default() -> Self {
        TokenType::Unknown.into()
    }
}

#[derive(Clone)]
pub struct TokenTree<'t, I: Iterator<Item = Token<'t>>> {
    pub(crate) input: &'t str,
    pub(crate) tree: Peekable<I>,
}

impl<'t, I: Iterator<Item = Token<'t>>> TokenTree<'t, I> {
    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }
}

impl<'t, 's, I, J> PartialEq<TokenTree<'s, J>> for TokenTree<'t, I>
where
    I: Clone + Iterator<Item = Token<'t>>,
    J: Clone + Iterator<Item = Token<'s>>,
{
    fn eq(&self, other: &TokenTree<'s, J>) -> bool {
        let mut other = other.tree.clone();
        for a in self.tree.clone() {
            match other.next() {
                Some(b) if a == b => {}
                _ => return false,
            };
        }
        true
    }
}

impl<'t, I: Clone + Iterator<Item = Token<'t>>> fmt::Debug for TokenTree<'t, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.tree.clone()).finish()
    }
}

#[test]
fn lookup_ident_test() {
    assert_eq!(
        Token::lookup_ident("opaque", 0..6),
        TokenType::Opaque.into()
    );
}
