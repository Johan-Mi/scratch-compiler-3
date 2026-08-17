use crate::parser::K;

pub fn lex(mut source: &str) -> impl Iterator<Item = (K, &str)> {
    std::iter::from_fn(move || {
        if let Some((kind, rest)) = lex_one(source) {
            let s = before(source, rest);
            source = rest;
            return Some((kind, s));
        }

        let mut chars = source.chars();
        while chars.next().is_some() {
            if lex_one(chars.as_str()).is_some() {
                let s = before(source, chars.as_str());
                source = chars.as_str();
                return Some((K::Error, s));
            }
        }

        None
    })
}

fn lex_one(source: &str) -> Option<(K, &str)> {
    use lexagon::{Lexer, Maybe, Or, Pred, Repeat0, Repeat1};

    if let Some(rest) =
        Repeat1(Or(Pred(char::is_whitespace), ("#", Pred(|c| c != '\n')))).lex(source)
    {
        return Some((K::Trivia, rest));
    }

    for (prefix_1, prefix_2, base, kind) in [
        ("0b", "0B", 2, K::BinaryNumber),
        ("0o", "0O", 8, K::OctalNumber),
        ("0x", "0X", 16, K::HexadecimalNumber),
    ] {
        if let Some(rest) = (
            Maybe(Or("+", "-")),
            Or(prefix_1, prefix_2),
            Repeat1(Pred(|c: char| c.is_digit(base))),
        )
            .lex(source)
        {
            return Some((kind, rest));
        }
    }

    for (s, kind) in [
        ("==", K::EqEq),
        ("(", K::Lparen),
        (")", K::Rparen),
        ("{", K::Lbrace),
        ("}", K::Rbrace),
        ("[", K::Lbracket),
        ("]", K::Rbracket),
        (":", K::Colon),
        (",", K::Comma),
        ("=", K::Eq),
        ("+", K::Plus),
        ("-", K::Minus),
        ("*", K::Star),
        ("/", K::Slash),
        ("%", K::Percent),
        ("<", K::Lt),
        (">", K::Gt),
        ("&", K::Ampersand),
        (".", K::Dot),
    ] {
        if let Some(rest) = s.lex(source) {
            return Some((kind, rest));
        }
    }

    if let Some(rest) = (
        "\"",
        Repeat0(Or(
            Pred(|c| !matches!(c, '"' | '\n')),
            ("\\", Pred(|c| c != '\n')),
        )),
        Maybe(Or("\\", "\"")),
    )
        .lex(source)
    {
        return Some((K::String, rest));
    }

    if let Some(rest) = (
        Or(Pred(unicode_ident::is_xid_start), "_"),
        Repeat0(Or(Pred(unicode_ident::is_xid_continue), "-")),
    )
        .lex(source)
    {
        let kind = match before(source, rest) {
            "struct" => K::KwStruct,
            "sprite" => K::KwSprite,
            "inline" => K::KwInline,
            "fn" => K::KwFn,
            "let" => K::KwLet,
            "costumes" => K::KwCostumes,
            "false" => K::KwFalse,
            "true" => K::KwTrue,
            "if" => K::KwIf,
            "else" => K::KwElse,
            "forever" => K::KwForever,
            "while" => K::KwWhile,
            "until" => K::KwUntil,
            "for" => K::KwFor,
            "as" => K::KwAs,
            "return" => K::KwReturn,
            _ => K::Identifier,
        };
        return Some((kind, rest));
    }

    None
}

fn before<'src>(full: &'src str, part: &str) -> &'src str {
    &full[..part.as_ptr().addr().strict_sub(full.as_ptr().addr())]
}
