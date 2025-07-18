use crate::diagnostics::{Diagnostics, primary, secondary};
use codemap::Span;
use logos::Logos as _;
use logos_derive::Logos;
use rowan::{Checkpoint, GreenNodeBuilder};

pub fn parse(file: &codemap::File, diagnostics: &mut Diagnostics) -> SyntaxNode {
    let source_code = file.source();
    let tokens = &SyntaxKind::lexer(source_code)
        .spanned()
        .map(|(token, span)| Token {
            kind: token.unwrap_or(ERROR),
            text: &source_code[span.clone()],
            span: file.span.subspan(span.start as u64, span.end as u64),
        })
        .collect::<Vec<_>>();
    Parser {
        builder: GreenNodeBuilder::new(),
        tokens,
        span: file.span,
        diagnostics,
    }
    .parse()
}

pub fn parse_string_literal(token: &SyntaxToken) -> Result<String, ()> {
    let mut res = String::with_capacity(token.text().len() - 1);
    let mut chars = token.text().chars().skip(1);
    while let Some(c) = chars.next() {
        match c {
            '"' => return Ok(res),
            '\\' => match chars.next() {
                Some('"' | '\\') => res.push(c),
                Some('n') => res.push('\n'),
                Some(_) => todo!("invalid escape sequence"),
                None => todo!("unfinished escape sequence"),
            },
            _ => res.push(c),
        }
    }
    // Unterminated string literal. TODO: emit diagnostic
    Err(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[expect(
    non_camel_case_types,
    clippy::upper_case_acronyms,
    reason = "Lets variants be imported unqualified without ambiguity"
)]
#[repr(u16)]
pub enum SyntaxKind {
    EOF = 0,
    #[regex(r"(\p{Whitespace}|#.*)+")]
    TRIVIA,

    DOCUMENT,
    STRUCT,
    FIELD_DEFINITION,
    SPRITE,
    COSTUME_LIST,
    COSTUME,
    FN,
    GENERICS,
    FUNCTION_PARAMETERS,
    PARAMETER,
    EXTERNAL_PARAMETER_NAME,
    BLOCK,
    VARIABLE,
    FUNCTION_CALL,
    ARGUMENTS,
    NAMED_ARGUMENT,
    LET,
    IF,
    ELSE_CLAUSE,
    REPEAT,
    FOREVER,
    WHILE,
    UNTIL,
    FOR,
    PARENTHESIZED_EXPRESSION,
    BINARY_EXPRESSION,
    LITERAL,
    LVALUE,
    GENERIC_TYPE_INSTANTIATION,
    TYPE_PARAMETERS,
    LIST_LITERAL,
    TYPE_ASCRIPTION,
    METHOD_CALL,
    RETURN,

    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,
    #[token("[")]
    LBRACKET,
    #[token("]")]
    RBRACKET,
    #[token("->")]
    ARROW,
    #[token(":")]
    COLON,
    #[token(",")]
    COMMA,
    #[token("=")]
    EQ,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("*")]
    STAR,
    #[token("/")]
    SLASH,
    #[token("%")]
    PERCENT,
    #[token("<")]
    LT,
    #[token("==")]
    EQ_EQ,
    #[token(">")]
    GT,
    #[token("&")]
    AMPERSAND,
    #[token(".")]
    DOT,

    #[token("struct")]
    KW_STRUCT,
    #[token("sprite")]
    KW_SPRITE,
    #[token("inline")]
    KW_INLINE,
    #[token("fn")]
    KW_FN,
    #[token("let")]
    KW_LET,
    #[token("costumes")]
    KW_COSTUMES,
    #[token("false")]
    KW_FALSE,
    #[token("true")]
    KW_TRUE,
    #[token("if")]
    KW_IF,
    #[token("else")]
    KW_ELSE,
    #[token("repeat")]
    KW_REPEAT,
    #[token("forever")]
    KW_FOREVER,
    #[token("while")]
    KW_WHILE,
    #[token("until")]
    KW_UNTIL,
    #[token("for")]
    KW_FOR,
    #[token("as")]
    KW_AS,
    #[token("return")]
    KW_RETURN,

    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}-]*")]
    IDENTIFIER,

    #[regex(r"[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    DECIMAL_NUMBER,
    #[regex(r"[+-]?0[bB][01]+")]
    BINARY_NUMBER,
    #[regex(r"[+-]?0[oO][0-7]+")]
    OCTAL_NUMBER,
    #[regex(r"[+-]?0[xX][0-9a-fA-F]+")]
    HEXADECIMAL_NUMBER,

    #[regex(r#""([^"\n\\]|\\[^\n])*[\\"]?"#)]
    STRING,

    ERROR,
}

use SyntaxKind::*;

impl SyntaxKind {
    pub const fn is_binary_operator(self) -> bool {
        matches!(
            self,
            PLUS | MINUS | STAR | SLASH | PERCENT | LT | EQ_EQ | GT | EQ
        )
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ERROR as u16);
        // SAFETY: `SyntaxKind` is `repr(u16)` and the assertion ensures that
        // `raw` is within range.
        unsafe { std::mem::transmute(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;

pub type SyntaxToken = rowan::SyntaxToken<Lang>;

struct Token<'src> {
    kind: SyntaxKind,
    text: &'src str,
    span: Span,
}

struct Parser<'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'src [Token<'src>],
    span: Span,
    diagnostics: &'src mut Diagnostics,
}

impl Parser<'_> {
    fn skip_trivia(&mut self) {
        while let [token, rest @ ..] = self.tokens {
            if token.kind != TRIVIA {
                break;
            }
            self.tokens = rest;
            self.builder.token(token.kind.into(), token.text);
        }
    }

    fn peek(&self) -> SyntaxKind {
        self.tokens
            .iter()
            .map(|token| token.kind)
            .find(|&it| it != TRIVIA)
            .unwrap_or(EOF)
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .iter()
            .find(|token| token.kind != TRIVIA)
            .map_or_else(
                || {
                    let len = self.span.len();
                    self.span.subspan(len, len)
                },
                |token| token.span,
            )
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.peek() == kind
    }

    fn immediately_at(&self, kind: SyntaxKind) -> bool {
        self.tokens.first().is_some_and(|token| token.kind == kind)
    }

    fn bump(&mut self) {
        while let Some(token) = self.tokens.split_off_first() {
            self.builder.token(token.kind.into(), token.text);
            if token.kind != TRIVIA {
                break;
            }
        }
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        self.at(kind) && {
            self.bump();
            true
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.skip_trivia();
        self.builder.start_node(kind.into());
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.skip_trivia();
        self.builder.checkpoint()
    }

    fn parse_anything(&mut self) {
        match self.peek() {
            KW_STRUCT => self.parse_struct(),
            KW_SPRITE => self.parse_sprite(),
            KW_INLINE | KW_FN => self.parse_function(),
            KW_COSTUMES => self.parse_costume_list(),
            KW_LET => self.parse_let(),
            KW_IF => self.parse_if(),
            KW_REPEAT => self.parse_repeat(),
            KW_FOREVER => self.parse_forever(),
            KW_WHILE => self.parse_while(),
            KW_UNTIL => self.parse_until(),
            KW_FOR => self.parse_for(),
            KW_RETURN => self.parse_return(),
            LPAREN => {
                self.bump();
                while !self.at(EOF) && !self.eat(RPAREN) {
                    self.parse_anything();
                }
            }
            LBRACE => {
                self.bump();
                while !self.at(EOF) && !self.eat(RBRACE) {
                    self.parse_anything();
                }
            }
            LBRACKET => {
                self.bump();
                while !self.at(EOF) && !self.eat(RBRACKET) {
                    self.parse_anything();
                }
            }
            _ => self.bump(),
        }
    }

    fn error(&mut self) {
        self.start_node(ERROR);
        self.parse_anything();
        self.builder.finish_node();
    }

    fn expect(&mut self, kind: SyntaxKind) -> Option<Span> {
        if self.at(kind) {
            let span = self.peek_span();
            self.bump();
            Some(span)
        } else {
            self.error();
            None
        }
    }

    fn parse_struct(&mut self) {
        self.start_node(STRUCT);
        self.bump(); // KW_STRUCT
        if !self.at(LBRACE) {
            let _: Option<Span> = self.expect(IDENTIFIER);
        }
        if self.eat(LBRACE) {
            while !self.at(EOF) && !self.eat(RBRACE) {
                if self.at(IDENTIFIER) {
                    self.parse_field_definition();
                } else {
                    self.error();
                }
            }
        }
        self.builder.finish_node();
    }

    fn parse_field_definition(&mut self) {
        self.start_node(FIELD_DEFINITION);
        self.bump(); // IDENTIFIER
        let _: Option<Span> = self.expect(COLON);
        self.parse_expression();
        let _: bool = self.eat(COMMA);
        self.builder.finish_node();
    }

    fn parse_arguments(&mut self) {
        self.start_node(ARGUMENTS);
        self.bump(); // LPAREN
        while !self.at(EOF) && !self.eat(RPAREN) {
            self.parse_expression();
            let _: bool = self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_list_literal(&mut self) {
        self.start_node(LIST_LITERAL);
        self.bump(); // LBRACKET
        while !self.at(EOF) && !self.eat(RBRACKET) {
            self.parse_expression();
            let _: bool = self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_atom(&mut self) {
        match self.peek() {
            IDENTIFIER => {
                let checkpoint = self.checkpoint();
                self.bump();
                if self.immediately_at(LPAREN) {
                    self.builder.start_node_at(checkpoint, FUNCTION_CALL.into());
                    self.parse_arguments();
                } else if self.immediately_at(COLON) {
                    self.builder
                        .start_node_at(checkpoint, NAMED_ARGUMENT.into());
                    self.bump();
                    self.parse_expression();
                } else {
                    self.builder.start_node_at(checkpoint, VARIABLE.into());
                }
                self.builder.finish_node();
            }
            LPAREN => {
                self.start_node(PARENTHESIZED_EXPRESSION);
                self.bump();
                self.parse_expression();
                let _: Option<Span> = self.expect(RPAREN);
                self.builder.finish_node();
            }
            DECIMAL_NUMBER | BINARY_NUMBER | OCTAL_NUMBER | HEXADECIMAL_NUMBER | STRING
            | KW_FALSE | KW_TRUE => {
                self.start_node(LITERAL);
                self.bump();
                self.builder.finish_node();
            }
            AMPERSAND => {
                self.start_node(LVALUE);
                self.bump();
                self.parse_atom();
                self.builder.finish_node();
            }
            LBRACKET => self.parse_list_literal(),
            _ => self.error(),
        }
    }

    fn parse_expression(&mut self) {
        self.parse_recursive_expression(EOF);
    }

    fn parse_recursive_expression(&mut self, left: SyntaxKind) {
        let checkpoint = self.checkpoint();
        self.parse_atom();
        while self.at(LBRACKET) {
            self.builder
                .start_node_at(checkpoint, GENERIC_TYPE_INSTANTIATION.into());
            self.parse_type_parameters();
            self.builder.finish_node();
        }

        loop {
            let right = self.peek();
            if binding_power(right) <= binding_power(left) {
                break;
            }
            let node_kind = match right {
                KW_AS => TYPE_ASCRIPTION,
                DOT => METHOD_CALL,
                _ => BINARY_EXPRESSION,
            };
            self.builder.start_node_at(checkpoint, node_kind.into());
            self.bump(); // operator
            self.parse_recursive_expression(right);
            self.builder.finish_node();
        }
    }

    fn parse_type_parameters(&mut self) {
        self.start_node(TYPE_PARAMETERS);
        self.bump(); // LBRACKET
        while !self.at(EOF) && !self.eat(RBRACKET) {
            self.parse_expression();
            let _: bool = self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_function_parameters(&mut self) {
        self.start_node(FUNCTION_PARAMETERS);
        self.bump(); // LPAREN
        while !self.at(EOF) && !self.eat(RPAREN) {
            if self.at(COMMA) {
                let span = self.peek_span();
                self.diagnostics
                    .error("unexpected `,`", [primary(span, "expected parameter")]);
                self.bump();
                continue;
            }
            if self.at(ARROW) || self.at(LBRACE) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unterminated parameter list",
                    [primary(span, "expected parameter or `)`")],
                );
                break;
            }
            if !self.at(IDENTIFIER) {
                self.error();
                continue;
            }

            self.start_node(PARAMETER);
            self.start_node(EXTERNAL_PARAMETER_NAME);
            self.bump();
            self.builder.finish_node();
            if !self.at(COLON) {
                let _: Option<Span> = self.expect(IDENTIFIER);
            }
            let _: Option<Span> = self.expect(COLON);
            if self.at(COMMA) {
                let span = self.peek_span();
                self.diagnostics
                    .error("unexpected `,`", [primary(span, "expected expression")]);
            } else if self.at(RPAREN) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unexpected end of parameter list",
                    [primary(span, "expected expression")],
                );
            } else {
                self.parse_expression();
            }
            let _: bool = self.eat(COMMA);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_let(&mut self) {
        self.start_node(LET);
        self.bump(); // KW_LET
        if !self.at(EQ) {
            let _: Option<Span> = self.expect(IDENTIFIER);
        }
        let _: Option<Span> = self.expect(EQ);
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_if(&mut self) {
        self.start_node(IF);
        self.bump(); // KW_IF
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `if`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        if self.eat(KW_ELSE) {
            self.start_node(ELSE_CLAUSE);
            if self.at(KW_IF) {
                self.parse_if();
            } else {
                self.parse_block();
            }
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_repeat(&mut self) {
        self.start_node(REPEAT);
        self.bump(); // KW_REPEAT
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `repeat`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_forever(&mut self) {
        self.start_node(FOREVER);
        self.bump(); // KW_FOREVER
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_while(&mut self) {
        self.start_node(WHILE);
        self.bump(); // KW_WHILE
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `while`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_until(&mut self) {
        self.start_node(UNTIL);
        self.bump(); // KW_UNTIL
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `until`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_for(&mut self) {
        self.start_node(FOR);
        self.bump(); // KW_FOR
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected identifier after `for`", [label]);
        } else {
            let _: Option<Span> = self.expect(IDENTIFIER);
            if self.at(LBRACE) {
                let label = primary(self.peek_span(), "");
                self.diagnostics
                    .error("expected expression after variable in `for` loop", [label]);
            } else {
                self.parse_expression();
            }
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_return(&mut self) {
        self.start_node(RETURN);
        self.bump(); // KW_RETURN
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_statement(&mut self) {
        match self.peek() {
            KW_LET => self.parse_let(),
            KW_IF => self.parse_if(),
            KW_REPEAT => self.parse_repeat(),
            KW_FOREVER => self.parse_forever(),
            KW_WHILE => self.parse_while(),
            KW_UNTIL => self.parse_until(),
            KW_FOR => self.parse_for(),
            KW_RETURN => self.parse_return(),
            _ => self.parse_expression(),
        }
    }

    fn parse_block(&mut self) {
        if !self.at(LBRACE) {
            self.error();
            return;
        }
        self.start_node(BLOCK);
        self.bump();
        while !matches!(self.peek(), EOF | KW_SPRITE) && !self.eat(RBRACE) {
            self.parse_statement();
        }
        self.builder.finish_node();
    }

    fn parse_function(&mut self) {
        let checkpoint = self.checkpoint();
        let _: bool = self.eat(KW_INLINE);
        if !self.eat(KW_FN) {
            let span = self.peek_span();
            self.diagnostics
                .error("expected `fn` after `inline`", [primary(span, "")]);
            return;
        }
        self.builder.start_node_at(checkpoint, FN.into());
        if self.at(IDENTIFIER) || self.peek().is_binary_operator() {
            self.bump();
        } else {
            self.error();
        }
        let _: bool = self.eat(STRING);
        if self.at(LBRACKET) {
            self.parse_generics();
        }
        if self.at(LPAREN) {
            self.parse_function_parameters();
        }
        if self.eat(ARROW) {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_generics(&mut self) {
        self.start_node(GENERICS);
        self.bump(); // LBRACKET
        while !self.at(EOF) && !self.eat(RBRACKET) {
            let _: Option<Span> = self.expect(IDENTIFIER);
            let _: bool = self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_costume_list(&mut self) {
        self.start_node(COSTUME_LIST);
        self.bump(); // KW_COSTUMES
        let _: Option<Span> = self.expect(LBRACE);
        while !self.at(EOF) && !self.eat(RBRACE) {
            if !self.at(STRING) {
                self.error();
                continue;
            }
            self.start_node(COSTUME);
            self.bump();
            let _: Option<Span> = self.expect(COLON);
            let _: Option<Span> = self.expect(STRING);
            let _: bool = self.eat(COMMA);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_sprite(&mut self) {
        self.start_node(SPRITE);
        let kw_sprite_span = self.peek_span();
        self.bump(); // KW_SPRITE
        if !self.at(LBRACE) {
            let _: Option<Span> = self.expect(IDENTIFIER);
        }
        let lbrace_span = self.expect(LBRACE);
        while !self.eat(RBRACE) {
            match self.peek() {
                KW_INLINE | KW_FN => self.parse_function(),
                KW_COSTUMES => self.parse_costume_list(),
                KW_LET => self.parse_let(),
                EOF | KW_SPRITE => {
                    let mut labels = vec![primary(kw_sprite_span, "")];
                    if let Some(lbrace_span) = lbrace_span {
                        labels.push(primary(lbrace_span, "unclosed brace"));
                    }
                    labels.push(secondary(self.peek_span(), "expected `}`"));
                    self.diagnostics
                        .error("unfinished sprite definition", labels);
                    break;
                }
                _ => self.error(),
            }
        }
        self.builder.finish_node();
    }

    fn parse_top_level_item(&mut self) {
        match self.peek() {
            KW_STRUCT => self.parse_struct(),
            KW_SPRITE => self.parse_sprite(),
            KW_INLINE | KW_FN => self.parse_function(),
            KW_LET => self.parse_let(),
            _ => self.error(),
        }
    }

    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(DOCUMENT.into());
        while !self.at(EOF) {
            self.parse_top_level_item();
        }
        self.builder.finish_node();
        SyntaxNode::new_root(self.builder.finish())
    }
}

const PRECEDENCE_TABLE: &[&[SyntaxKind]] = &[
    &[EQ],
    &[LT, EQ_EQ, GT],
    &[PLUS, MINUS],
    &[STAR, SLASH, PERCENT],
    &[KW_AS],
    &[DOT],
];

fn binding_power(kind: SyntaxKind) -> Option<usize> {
    PRECEDENCE_TABLE
        .iter()
        .position(|level| level.contains(&kind))
}
