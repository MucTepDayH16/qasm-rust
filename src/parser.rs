//! This module implements the various parsing methods.
//! Most methods are not documented, and should only be accessed
//! indirectly from the `parse` method.

use ast::{Argument, AstNode};
use error::Error;
use token::{Token, TokenTree, TokenType};

const SUPPORTED_VERSIONS: [f32; 1] = [2.0];

type Result<'t, T> = std::result::Result<T, Error<'t>>;

pub fn parse<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, Vec<AstNode<'t>>> {
    let mut nodes = vec![];

    // Check that the version is first, and that it is version 2.0
    let version = version(tokens).map_err(|_| Error::MissingVersion)?;
    if !SUPPORTED_VERSIONS.contains(&version) {
        return Err(Error::UnsupportedVersion);
    }

    while tokens.tree.peek().is_some() {
        let node = parse_node(tokens)?;
        nodes.push(node);
    }

    Ok(nodes)
}

fn parse_node<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    match tokens.tree.next().ok_or(Error::SourceError)?.to_tup() {
        (TokenType::QReg, _) => qreg(tokens),
        (TokenType::CReg, _) => creg(tokens),
        (TokenType::Barrier, _) => barrier(tokens),
        (TokenType::Reset, _) => reset(tokens),
        (TokenType::Measure, _) => measure(tokens),
        (TokenType::Id(i), _) => application(tokens, i),
        (TokenType::Opaque, _) => opaque(tokens),
        (TokenType::Gate, _) => gate(tokens),
        (TokenType::If, _) => if_block(tokens),

        _ => Err(Error::SourceError),
    }
}

fn version<'t>(tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>) -> Result<'t, f32> {
    let version;
    #[cfg(not(feature = "no-check-ver"))]
    {
        match_token(tokens, TokenType::OpenQASM)?;
        version = match_real(tokens)?;
        match_semicolon(tokens)?;
    }
    #[cfg(feature = "no-check-ver")]
    {
        if let Some(Token {
            token_type: TokenType::OpenQASM,
            ..
        }) = tokens.tree.peek()
        {
            version = match_real(tokens)?;
            match_semicolon(tokens)?;
        } else {
            version = 2.0;
        }
    }

    Ok(version)
}

fn qreg<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // QReg -> Identifier -> Left Square Bracket -> Int -> Right Square Bracket -> Semicolon
    let identifier = match_identifier(tokens)?;
    match_token(tokens, TokenType::LSParen)?;
    let num = match_int(tokens)?;
    match_token(tokens, TokenType::RSParen)?;
    match_semicolon(tokens)?;

    Ok(AstNode::QReg(identifier, num))
}

fn creg<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // CReg -> Identifier -> Left Square Bracket -> Int -> Right Square Bracket -> Semicolon
    let identifier = match_identifier(tokens)?;
    match_token(tokens, TokenType::LSParen)?;
    let num = match_int(tokens)?;
    match_token(tokens, TokenType::RSParen)?;
    match_semicolon(tokens)?;

    Ok(AstNode::CReg(identifier, num))
}

fn if_block<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    match_token(tokens, TokenType::LParen)?;
    let id = match_identifier(tokens)?;
    match_token(tokens, TokenType::Equals)?;
    match_token(tokens, TokenType::Equals)?;
    let val = match_int(tokens)?;
    match_token(tokens, TokenType::RParen)?;
    let node = parse_node(tokens)?;

    Ok(AstNode::If(id, val, Box::new(node)))
}

fn barrier<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // Barrier -> Argument -> Semicolon
    let argument = match_argument(tokens)?;
    match_semicolon(tokens)?;

    Ok(AstNode::Barrier(argument))
}

fn reset<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // reset -> Argument -> Semicolon
    let argument = match_argument(tokens)?;
    match_semicolon(tokens)?;

    Ok(AstNode::Reset(argument))
}

fn measure<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // Measure -> Argument -> Arrow -> Argument -> Semicolon
    let arg_1 = match_argument(tokens)?;
    match_token(tokens, TokenType::Minus)?;
    match_token(tokens, TokenType::Arrow)?;
    let arg_2 = match_argument(tokens)?;
    match_semicolon(tokens)?;

    Ok(AstNode::Measure(arg_1, arg_2))
}

fn application<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
    id: &'t str,
) -> Result<'t, AstNode<'t>> {
    // id -> argument list -> Semicolon;
    // id -> () -> argument list -> Semicolon;
    // id -> ( Expr list ) ->
    let params = if tokens.tree.next_if_eq(&TokenType::LParen.into()).is_some() {
        if tokens.tree.next_if_eq(&TokenType::RParen.into()).is_some() {
            vec![]
        } else {
            let p = match_mathexpr_list(tokens)?;
            match_token(tokens, TokenType::RParen)?;
            p
        }
    } else {
        vec![]
    };

    let list = match_argument_list(tokens)?;
    match_semicolon(tokens)?;

    Ok(AstNode::ApplyGate(id, list, params))
}

fn opaque<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // opaque -> id -> argument list -> Semicolon;
    // opaque -> id -> () -> argument list -> Semicolon;
    // opaque -> id -> ( Expr list ) ->
    let id = match_identifier(tokens)?;

    let params = if tokens.tree.next_if_eq(&TokenType::LParen.into()).is_some() {
        if tokens.tree.next_if_eq(&TokenType::RParen.into()).is_some() {
            vec![]
        } else {
            let p = match_id_list(tokens)?;
            match_token(tokens, TokenType::RParen)?;
            p
        }
    } else {
        vec![]
    };

    let list = match_argument_list(tokens)?;
    match_semicolon(tokens)?;

    Ok(AstNode::Opaque(id, list, params))
}

fn gate<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, AstNode<'t>> {
    // gate -> id -> argument list -> { -> list of applications -> }
    // gate -> id -> () -> argument list ->{ -> list of applications -> }
    // gate -> id -> ( Expr list ) -> { -> list of applications -> }
    let id = match_identifier(tokens)?;

    let params = if tokens.tree.next_if_eq(&TokenType::LParen.into()).is_some() {
        if tokens.tree.next_if_eq(&TokenType::RParen.into()).is_some() {
            vec![]
        } else {
            let p = match_id_list(tokens)?;
            match_token(tokens, TokenType::RParen)?;
            p
        }
    } else {
        vec![]
    };

    let list = match_id_list(tokens)?;
    match_token(tokens, TokenType::LCParen)?;
    let applications = match_application_list(tokens)?;
    match_token(tokens, TokenType::RCParen)?;

    Ok(AstNode::Gate(id, list, params, applications))
}

//////////////////////////////////////////////////////////////////////
// Terminals
//////////////////////////////////////////////////////////////////////
fn match_application_list<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, Vec<AstNode<'t>>> {
    let mut args = vec![];

    while let Some(tok) = tokens.tree.peek().cloned() {
        let id = match tok.token_type {
            TokenType::Id(id) => {
                tokens.tree.next();
                id
            }
            _ => break,
        };
        let tail = application(tokens, id)?;
        args.push(tail);
    }

    Ok(args)
}

fn match_argument_list<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, Vec<Argument<'t>>> {
    let head = match_argument(tokens)?;
    let mut args = vec![head];

    while tokens.tree.next_if_eq(&TokenType::Comma.into()).is_some() {
        let tail = match_argument(tokens)?;
        args.push(tail);
    }

    Ok(args)
}

fn match_mathexpr_list<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, Vec<&'t str>> {
    let head = match_mathexpr(tokens)?;
    let mut args = vec![head];

    while tokens.tree.next_if_eq(&TokenType::Comma.into()).is_some() {
        let tail = match_mathexpr(tokens)?;
        args.push(tail);
    }

    Ok(args)
}

fn match_id_list<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, Vec<&'t str>> {
    let head = match_identifier(tokens)?;
    let mut args = vec![head];

    while tokens.tree.next_if_eq(&TokenType::Comma.into()).is_some() {
        let tail = match_identifier(tokens)?;
        args.push(tail);
    }

    Ok(args)
}

fn match_mathexpr<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, &'t str> {
    if let None = tokens.tree.peek() {
        return Err(Error::SourceError);
    }

    let mut expr_span = 0..0;
    let mut num_open_paren = 0;

    // Parse until we find a comma, semicolon or a non matching paren
    while let Some(token) = tokens.tree.peek().cloned() {
        match token.token_type {
            TokenType::Id(_)
            | TokenType::Real(_)
            | TokenType::Int(_)
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Times
            | TokenType::Divide
            | TokenType::Power => {}
            TokenType::LParen => {
                num_open_paren += 1;
            }
            TokenType::RParen => {
                if num_open_paren == 0 {
                    return tokens.input.get(expr_span).ok_or(Error::SourceError);
                }
                num_open_paren -= 1;
            }
            _ => return tokens.input.get(expr_span).ok_or(Error::SourceError),
        };
        tokens.tree.next();

        if expr_span.start == 0 {
            expr_span = token.span;
        } else {
            expr_span.end = token.span.end;
        }
    }

    tokens.input.get(expr_span).ok_or(Error::SourceError)
}

fn match_argument<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, Argument<'t>> {
    let id = match_identifier(tokens)?;

    if tokens.tree.next_if_eq(&TokenType::LSParen.into()).is_some() {
        let n = match_int(tokens)?;
        match_token(tokens, TokenType::RSParen)?;
        Ok(Argument::Qubit(id, n))
    } else {
        Ok(Argument::Register(id))
    }
}

fn match_real<'t>(tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>) -> Result<'t, f32> {
    match tokens.tree.next().map(Token::to_tup) {
        Some((TokenType::Real(n), _)) => Ok(n),
        Some(_) => Err(Error::MissingReal),
        None => Err(Error::SourceError),
    }
}

fn match_int<'t>(tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>) -> Result<'t, i32> {
    match tokens.tree.next().map(Token::to_tup) {
        Some((TokenType::Int(n), _)) => Ok(n),
        Some(_) => Err(Error::MissingInt),
        None => Err(Error::SourceError),
    }
}

fn match_identifier<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, &'t str> {
    match tokens.tree.next().map(Token::to_tup) {
        Some((TokenType::Id(s), _)) => Ok(s),
        Some(_) => Err(Error::MissingIdentifier),
        None => Err(Error::SourceError),
    }
}

fn match_token<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
    eq_tok: TokenType<'t>,
) -> Result<'t, ()> {
    match tokens.tree.next().map(Token::to_tup) {
        Some((tok, _)) if tok == eq_tok => Ok(()),
        Some((tok, _)) => Err(Error::UnexpectedToken(eq_tok, tok)),
        None => Err(Error::SourceError),
    }
}

fn match_semicolon<'t>(
    tokens: &mut TokenTree<'t, impl Iterator<Item = Token<'t>>>,
) -> Result<'t, ()> {
    match tokens.tree.next().map(Token::to_tup) {
        Some((TokenType::Semicolon, _)) => Ok(()),
        _ => Err(Error::MissingSemicolon),
    }
}

#[cfg(test)]
#[test]
fn parser() {
    let source = "OPENQASM 2.0;
    qreg q[4];
    creg c[4];
    h q;
    barrier q;
    h q[0];
    measure q[0] -> c[0];
    if(c==1) u1(pi/2) q[1];
    h q[1];
    measure q[1] -> c[1];
    if(c==1) u1(pi/4) q[2];
    if(c==2) u1(pi/2) q[2];
    if(c==3) u1(pi/2+pi/4) q[2];
    h q[2];
    measure q[2] -> c[2];
    if(c==1) u1(pi/8) q[3];
    if(c==2) u1(pi/4) q[3];
    if(c==3) u1(pi/4+pi/8) q[3];
    if(c==4) u1(pi/2) q[3];
    if(c==5) u1(pi/2+pi/8) q[3];
    if(c==6) u1(pi/2+pi/4) q[3];
    if(c==7) u1(pi/2+pi/4+pi/8) q[3];
    h q[3];
    measure q[3] -> c[3];";
    let tokens = crate::lex(source);
    //println!("{tokens:?}");
    let parsed = crate::parse(tokens).unwrap();
    println!("{parsed:?}");
}
