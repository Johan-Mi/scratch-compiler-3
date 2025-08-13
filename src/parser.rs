use crate::diagnostics::{Diagnostics, primary, secondary};
use codemap::Span;
use logos::Logos as _;
use logos_derive::Logos;
use std::collections::HashMap;

pub fn parse(
    file: &codemap::File,
    string_literals: &mut HashMap<codemap::Pos, String>,
    diagnostics: &mut Diagnostics,
) -> cst::Tree<K> {
    let source_code = file.source();
    let tokens = &K::lexer(source_code)
        .spanned()
        .map(|(token, range)| {
            let span = file.span.subspan(range.start as u64, range.end as u64);
            if token == Ok(K::String) {
                let literal = parse_string_literal(&source_code[range], span, diagnostics);
                string_literals.extend(Some(span.low()).zip(literal));
            }
            (token.unwrap_or(K::Error), span)
        })
        .collect::<Vec<_>>();
    let len = file.span.len();
    Parser {
        builder: cst::Builder::default(),
        tokens,
        eof_span: file.span.subspan(len, len),
        diagnostics,
    }
    .parse()
}

fn parse_string_literal(text: &str, span: Span, diagnostics: &mut Diagnostics) -> Option<String> {
    let mut res = Some(String::with_capacity(text.len() - 1));
    let mut chars = text.chars();
    assert_eq!(chars.next(), Some('"'));
    while let Some(c) = chars.next() {
        match c {
            '"' => return res,
            '\\' => match chars.next().or_else(|| {
                let end = span.len();
                let backslash = span.subspan(end - 1, end);
                diagnostics.error("unfinished escape sequence", [primary(backslash, "")]);
                None
            })? {
                '"' | '\\' => {
                    if let Some(res) = &mut res {
                        res.push(c);
                    }
                }
                'n' => {
                    if let Some(res) = &mut res {
                        res.push('\n');
                    }
                }
                esc => {
                    let end =
                        std::ptr::from_ref(chars.as_str()).addr() - std::ptr::from_ref(text).addr();
                    let start = end - esc.len_utf8() - 1;
                    let span = span.subspan(start.try_into().unwrap(), end.try_into().unwrap());
                    diagnostics.error("invalid escape sequence", [primary(span, "")]);
                    res = None;
                }
            },
            _ => {
                if let Some(res) = &mut res {
                    res.push(c);
                }
            }
        }
    }
    diagnostics.error("unterminated string literal", [primary(span, "")]);
    None
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[repr(u16)]
pub enum K {
    Eof = 0,
    #[regex(r"(\p{Whitespace}|#.*)+")]
    Trivia,

    Document,
    Struct,
    FieldDefinition,
    Sprite,
    CostumeList,
    Costume,
    Function,
    Generics,
    FunctionParameters,
    Parameter,
    ExternalParameterName,
    Block,
    Variable,
    FunctionCall,
    Arguments,
    NamedArgument,
    Let,
    If,
    ElseClause,
    Repeat,
    Forever,
    While,
    Until,
    For,
    ParenthesizedExpression,
    BinaryOperation,
    Literal,
    Lvalue,
    GenericTypeInstantiation,
    TypeParameters,
    ListLiteral,
    TypeAscription,
    MethodCall,
    Return,

    #[token("(")]
    Lparen,
    #[token(")")]
    Rparen,
    #[token("{")]
    Lbrace,
    #[token("}")]
    Rbrace,
    #[token("[")]
    Lbracket,
    #[token("]")]
    Rbracket,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    Lt,
    #[token("==")]
    EqEq,
    #[token(">")]
    Gt,
    #[token("&")]
    Ampersand,
    #[token(".")]
    Dot,

    #[token("struct")]
    KwStruct,
    #[token("sprite")]
    KwSprite,
    #[token("inline")]
    KwInline,
    #[token("fn")]
    KwFn,
    #[token("let")]
    KwLet,
    #[token("costumes")]
    KwCostumes,
    #[token("false")]
    KwFalse,
    #[token("true")]
    KwTrue,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("repeat")]
    KwRepeat,
    #[token("forever")]
    KwForever,
    #[token("while")]
    KwWhile,
    #[token("until")]
    KwUntil,
    #[token("for")]
    KwFor,
    #[token("as")]
    KwAs,
    #[token("return")]
    KwReturn,

    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}-]*")]
    Identifier,

    #[regex(r"[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    DecimalNumber,
    #[regex(r"[+-]?0[bB][01]+")]
    BinaryNumber,
    #[regex(r"[+-]?0[oO][0-7]+")]
    OctalNumber,
    #[regex(r"[+-]?0[xX][0-9a-fA-F]+")]
    HexadecimalNumber,

    #[regex(r#""([^"\n\\]|\\[^\n])*[\\"]?"#)]
    String,

    Error,
}

impl K {
    pub const fn is_binary_operator(self) -> bool {
        matches!(
            self,
            Self::Plus
                | Self::Minus
                | Self::Star
                | Self::Slash
                | Self::Percent
                | Self::Lt
                | Self::EqEq
                | Self::Gt
                | Self::Eq
        )
    }
}

pub type SyntaxNode<'src> = cst::Node<'src, K>;

struct Parser<'src> {
    builder: cst::Builder<K>,
    tokens: &'src [(K, Span)],
    eof_span: Span,
    diagnostics: &'src mut Diagnostics,
}

impl Parser<'_> {
    fn skip_trivia(&mut self) {
        while let [(kind, span), rest @ ..] = self.tokens
            && *kind == K::Trivia
        {
            self.tokens = rest;
            self.builder.token(*kind, *span);
        }
    }

    fn peek(&self) -> K {
        self.tokens
            .iter()
            .map(|&(kind, _)| kind)
            .find(|&it| it != K::Trivia)
            .unwrap_or(K::Eof)
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .iter()
            .find(|&&(kind, _)| kind != K::Trivia)
            .map_or(self.eof_span, |&(_, span)| span)
    }

    fn at(&self, kind: K) -> bool {
        self.peek() == kind
    }

    fn immediately_at(&self, kind: K) -> bool {
        self.tokens.first().is_some_and(|&(k, _)| k == kind)
    }

    fn bump(&mut self) {
        while let Some(&(kind, span)) = self.tokens.split_off_first() {
            self.builder.token(kind, span);
            if kind != K::Trivia {
                break;
            }
        }
    }

    fn eat(&mut self, kind: K) -> bool {
        self.at(kind) && {
            self.bump();
            true
        }
    }

    fn start_node(&mut self, kind: K) {
        self.skip_trivia();
        self.builder.start_node(kind, self.peek_span());
    }

    fn checkpoint(&mut self) -> cst::Checkpoint {
        self.skip_trivia();
        self.builder.checkpoint(self.peek_span())
    }

    fn parse_anything(&mut self) {
        match self.peek() {
            K::KwStruct => self.parse_struct(),
            K::KwSprite => self.parse_sprite(),
            K::KwInline | K::KwFn => self.parse_function(),
            K::KwCostumes => self.parse_costume_list(),
            K::KwLet => self.parse_let(),
            K::KwIf => self.parse_if(),
            K::KwRepeat => self.parse_repeat(),
            K::KwForever => self.parse_forever(),
            K::KwWhile => self.parse_while(),
            K::KwUntil => self.parse_until(),
            K::KwFor => self.parse_for(),
            K::KwReturn => self.parse_return(),
            K::Lparen => {
                self.bump();
                while !self.at(K::Eof) && !self.eat(K::Rparen) {
                    self.parse_anything();
                }
            }
            K::Lbrace => {
                self.bump();
                while !self.at(K::Eof) && !self.eat(K::Rbrace) {
                    self.parse_anything();
                }
            }
            K::Lbracket => {
                self.bump();
                while !self.at(K::Eof) && !self.eat(K::Rbracket) {
                    self.parse_anything();
                }
            }
            _ => self.bump(),
        }
    }

    fn error(&mut self) {
        self.start_node(K::Error);
        self.parse_anything();
        self.builder.finish_node();
    }

    fn parse_struct(&mut self) {
        self.start_node(K::Struct);
        self.bump(); // K::KwStruct
        if !self.at(K::Lbrace) && !self.eat(K::Identifier) {
            self.error();
        }
        if self.eat(K::Lbrace) {
            while !self.at(K::Eof) && !self.eat(K::Rbrace) {
                if self.at(K::Identifier) {
                    self.parse_field_definition();
                } else {
                    self.error();
                }
            }
        }
        self.builder.finish_node();
    }

    fn parse_field_definition(&mut self) {
        self.start_node(K::FieldDefinition);
        self.bump(); // K::Identifier
        if !self.eat(K::Colon) {
            self.error();
        }
        self.parse_expression();
        let _: bool = self.eat(K::Comma);
        self.builder.finish_node();
    }

    fn parse_arguments(&mut self) {
        self.start_node(K::Arguments);
        self.bump(); // K::Lparen
        while !self.at(K::Eof) && !self.eat(K::Rparen) {
            self.parse_expression();
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_list_literal(&mut self) {
        self.start_node(K::ListLiteral);
        self.bump(); // K::Lbracket
        while !self.at(K::Eof) && !self.eat(K::Rbracket) {
            self.parse_expression();
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_atom(&mut self) {
        match self.peek() {
            K::Identifier => {
                let checkpoint = self.checkpoint();
                self.bump();
                if self.immediately_at(K::Lparen) {
                    self.parse_arguments();
                    self.builder.finish_node_at(checkpoint, K::FunctionCall);
                } else if self.immediately_at(K::Colon) {
                    self.bump();
                    self.parse_expression();
                    self.builder.finish_node_at(checkpoint, K::NamedArgument);
                } else {
                    self.builder.finish_node_at(checkpoint, K::Variable);
                }
            }
            K::Lparen => {
                self.start_node(K::ParenthesizedExpression);
                self.bump();
                self.parse_expression();
                if !self.eat(K::Rparen) {
                    self.error();
                }
                self.builder.finish_node();
            }
            K::DecimalNumber
            | K::BinaryNumber
            | K::OctalNumber
            | K::HexadecimalNumber
            | K::String
            | K::KwFalse
            | K::KwTrue => {
                self.start_node(K::Literal);
                self.bump();
                self.builder.finish_node();
            }
            K::Ampersand => {
                self.start_node(K::Lvalue);
                self.bump();
                self.parse_atom();
                self.builder.finish_node();
            }
            K::Lbracket => self.parse_list_literal(),
            _ => self.error(),
        }
    }

    fn parse_expression(&mut self) {
        self.parse_recursive_expression(K::Eof);
    }

    fn parse_recursive_expression(&mut self, left: K) {
        let checkpoint = self.checkpoint();
        self.parse_atom();
        while self.at(K::Lbracket) {
            self.parse_type_parameters();
            self.builder
                .finish_node_at(checkpoint, K::GenericTypeInstantiation);
        }

        while let right = self.peek()
            && binding_power(left) < binding_power(right)
        {
            let node_kind = match right {
                K::KwAs => K::TypeAscription,
                K::Dot => K::MethodCall,
                _ => K::BinaryOperation,
            };
            self.bump(); // operator
            self.parse_recursive_expression(right);
            self.builder.finish_node_at(checkpoint, node_kind);
        }
    }

    fn parse_type_parameters(&mut self) {
        self.start_node(K::TypeParameters);
        self.bump(); // K::Lbracket
        while !self.at(K::Eof) && !self.eat(K::Rbracket) {
            self.parse_expression();
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_function_parameters(&mut self) {
        self.start_node(K::FunctionParameters);
        self.bump(); // K::Lparen
        while !self.at(K::Eof) && !self.eat(K::Rparen) {
            if self.at(K::Comma) {
                let span = self.peek_span();
                self.diagnostics
                    .error("unexpected `,`", [primary(span, "expected parameter")]);
                self.bump();
                continue;
            }
            if self.at(K::Arrow) || self.at(K::Lbrace) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unterminated parameter list",
                    [primary(span, "expected parameter or `)`")],
                );
                break;
            }
            if !self.at(K::Identifier) {
                self.error();
                continue;
            }

            self.start_node(K::Parameter);
            self.start_node(K::ExternalParameterName);
            self.bump();
            self.builder.finish_node();
            if !self.at(K::Colon) && !self.eat(K::Identifier) {
                self.error();
            }
            if !self.eat(K::Colon) {
                self.error();
            }
            if self.at(K::Comma) {
                let span = self.peek_span();
                self.diagnostics
                    .error("unexpected `,`", [primary(span, "expected expression")]);
            } else if self.at(K::Rparen) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unexpected end of parameter list",
                    [primary(span, "expected expression")],
                );
            } else {
                self.parse_expression();
            }
            let _: bool = self.eat(K::Comma);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_let(&mut self) {
        self.start_node(K::Let);
        self.bump(); // K::KwLet
        if !self.at(K::Eq) && !self.eat(K::Identifier) {
            self.error();
        }
        if !self.eat(K::Eq) {
            self.error();
        }
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_if(&mut self) {
        self.start_node(K::If);
        self.bump(); // K::KwIf
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `if`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        if self.eat(K::KwElse) {
            self.start_node(K::ElseClause);
            if self.at(K::KwIf) {
                self.parse_if();
            } else {
                self.parse_block();
            }
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_repeat(&mut self) {
        self.start_node(K::Repeat);
        self.bump(); // K::KwRepeat
        if self.at(K::Lbrace) {
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
        self.start_node(K::Forever);
        self.bump(); // K::KwForever
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_while(&mut self) {
        self.start_node(K::While);
        self.bump(); // K::KwWhile
        if self.at(K::Lbrace) {
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
        self.start_node(K::Until);
        self.bump(); // K::KwUntil
        if self.at(K::Lbrace) {
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
        self.start_node(K::For);
        self.bump(); // K::KwFor
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected identifier after `for`", [label]);
        } else {
            if !self.eat(K::Identifier) {
                self.error();
            }
            if self.at(K::Lbrace) {
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
        self.start_node(K::Return);
        self.bump(); // K::KwReturn
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_statement(&mut self) {
        match self.peek() {
            K::KwLet => self.parse_let(),
            K::KwIf => self.parse_if(),
            K::KwRepeat => self.parse_repeat(),
            K::KwForever => self.parse_forever(),
            K::KwWhile => self.parse_while(),
            K::KwUntil => self.parse_until(),
            K::KwFor => self.parse_for(),
            K::KwReturn => self.parse_return(),
            _ => self.parse_expression(),
        }
    }

    fn parse_block(&mut self) {
        if !self.at(K::Lbrace) {
            self.error();
            return;
        }
        self.start_node(K::Block);
        self.bump();
        while !matches!(self.peek(), K::Eof | K::KwSprite) && !self.eat(K::Rbrace) {
            self.parse_statement();
        }
        self.builder.finish_node();
    }

    fn parse_function(&mut self) {
        let checkpoint = self.checkpoint();
        let _: bool = self.eat(K::KwInline);
        if !self.eat(K::KwFn) {
            let span = self.peek_span();
            self.diagnostics
                .error("expected `fn` after `inline`", [primary(span, "")]);
            return;
        }
        if self.at(K::Identifier) || self.peek().is_binary_operator() {
            self.bump();
        } else {
            self.error();
        }
        let _: bool = self.eat(K::String);
        if self.at(K::Lbracket) {
            self.parse_generics();
        }
        if self.at(K::Lparen) {
            self.parse_function_parameters();
        }
        if self.eat(K::Arrow) {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node_at(checkpoint, K::Function);
    }

    fn parse_generics(&mut self) {
        self.start_node(K::Generics);
        self.bump(); // K::Lbracket
        while !self.at(K::Eof) && !self.eat(K::Rbracket) {
            if !self.eat(K::Identifier) {
                self.error();
            }
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_costume_list(&mut self) {
        self.start_node(K::CostumeList);
        self.bump(); // K::KwCostumes
        if !self.eat(K::Lbrace) {
            self.error();
        }
        while !self.at(K::Eof) && !self.eat(K::Rbrace) {
            if !self.at(K::String) {
                self.error();
                continue;
            }
            self.start_node(K::Costume);
            self.bump();
            if !self.eat(K::Colon) {
                self.error();
            }
            if !self.eat(K::String) {
                self.error();
            }
            let _: bool = self.eat(K::Comma);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_sprite(&mut self) {
        self.start_node(K::Sprite);
        let kw_sprite_span = self.peek_span();
        self.bump(); // K::KwSprite
        if !self.at(K::Lbrace) && !self.eat(K::Identifier) {
            self.error();
        }
        let lbrace_span = if self.at(K::Lbrace) {
            let span = self.peek_span();
            self.bump();
            Some(span)
        } else {
            self.error();
            None
        };
        while !self.eat(K::Rbrace) {
            match self.peek() {
                K::KwInline | K::KwFn => self.parse_function(),
                K::KwCostumes => self.parse_costume_list(),
                K::KwLet => self.parse_let(),
                K::Eof | K::KwSprite => {
                    let labels = std::iter::once(primary(kw_sprite_span, ""))
                        .chain(lbrace_span.map(|it| primary(it, "unclosed brace")))
                        .chain(std::iter::once(secondary(self.peek_span(), "expected `}`")))
                        .collect::<Vec<_>>();
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
            K::KwStruct => self.parse_struct(),
            K::KwSprite => self.parse_sprite(),
            K::KwInline | K::KwFn => self.parse_function(),
            K::KwLet => self.parse_let(),
            _ => self.error(),
        }
    }

    fn parse(mut self) -> cst::Tree<K> {
        self.builder.start_node(K::Document, self.eof_span);
        while !self.at(K::Eof) {
            self.parse_top_level_item();
        }
        self.builder.finish_node();
        self.builder.build()
    }
}

const PRECEDENCE: &[&[K]] = &[
    &[K::Eq],
    &[K::Lt, K::EqEq, K::Gt],
    &[K::Plus, K::Minus],
    &[K::Star, K::Slash, K::Percent],
    &[K::KwAs],
    &[K::Dot],
];

fn binding_power(kind: K) -> Option<usize> {
    PRECEDENCE.iter().position(|level| level.contains(&kind))
}
