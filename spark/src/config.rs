/*
 * Copyright (c) 2023 xvanc and contributors
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

use core::{fmt, mem, str::Chars};

/// A location within a source
#[derive(Clone, Copy, Debug)]
struct SourceLocation(u32, u32);

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.0 + 1, self.1 + 1)
    }
}

#[derive(Clone, Copy, Debug)]
struct Token<'a> {
    location: (u32, u32),
    kind: TokenKind<'a>,
}

impl Token<'_> {
    /// Returns the location of the token in the source input
    ///
    /// The returned location is 0-indexed.
    const fn location(&self) -> SourceLocation {
        SourceLocation(self.location.0, self.location.1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenKind<'a> {
    Ident(&'a str),
    Number(&'a str),
    String(&'a str),
    Bool(bool),
    Semi,
    Colon,
    OpenBrace,
    CloseBrace,
    Eq,
    Comment,
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TokenKind::Ident(_) => "ident",
            TokenKind::Number(_) => "number",
            TokenKind::String(_) => "string",
            TokenKind::Bool(_) => "bool",
            TokenKind::Semi => ";",
            TokenKind::Colon => ":",
            TokenKind::OpenBrace => "{",
            TokenKind::CloseBrace => "}",
            TokenKind::Eq => "=",
            TokenKind::Comment => "comment",
        };
        write!(f, "{s}")
    }
}

struct Lexer<'src> {
    position: (u32, u32),
    input: Chars<'src>,
}

impl<'src> Lexer<'src> {
    fn new(input: &'src str) -> Lexer<'src> {
        Self {
            position: (0, 0),
            input: input.chars(),
        }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.input.next();
        if c == Some('\n') {
            self.position.0 += 1;
            self.position.1 = 0;
        } else {
            self.position.1 += 1;
        }
        c
    }

    fn first(&self) -> char {
        self.input.clone().next().unwrap_or('\0')
    }

    fn second(&self) -> char {
        self.input.clone().nth(1).unwrap_or('\0')
    }

    fn next_token(&mut self) -> Option<Token<'src>> {
        while self.first().is_whitespace() {
            self.bump()?;
        }

        let location = self.position;

        macro_rules! matchtok {
            ($kind:ident) => {{
                self.bump()?;
                TokenKind::$kind
            }};
        }

        let kind = match self.first() {
            ';' => matchtok!(Semi),
            ':' => matchtok!(Colon),
            '{' => matchtok!(OpenBrace),
            '}' => matchtok!(CloseBrace),
            '=' => matchtok!(Eq),
            '"' => {
                self.bump()?;
                let start = self.input.as_str();
                let mut len = 0;
                while self.first() != '"' {
                    len += 1;
                    self.bump()?;
                }
                let inner = &start[..len];
                self.bump()?;
                TokenKind::String(inner)
            }
            '_' | '-' | 'a'..='z' | 'A'..='Z' => {
                let start = self.input.as_str();
                let mut len = 0;
                while matches!(self.first(), '_' | '-' | 'a'..='z' | 'A'..='Z' | '0'..='9') {
                    len += 1;
                    self.bump()?;
                }
                match &start[..len] {
                    "true" => TokenKind::Bool(true),
                    "false" => TokenKind::Bool(false),
                    ident => TokenKind::Ident(ident),
                }
            }
            '/' if self.second() == '/' => {
                while self.bump()? != '\n' {}
                TokenKind::Comment
            }
            '\0' => return None,
            unk => todo!("{unk:?}"),
        };

        Some(Token { location, kind })
    }
}

#[derive(Debug)]
struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    index: usize,
}

impl<'src> Parser<'src> {
    fn parse_token(&mut self, expected: TokenKind) -> Option<Token<'src>> {
        let token = self.tokens.get(self.index)?;
        let found = token.kind;

        if mem::discriminant(&found) == mem::discriminant(&expected) {
            self.index += 1;
            Some(*token)
        } else {
            log::error!(
                "{}: expected `{expected}`, found `{found}`",
                token.location()
            );
            None
        }
    }

    fn parse_entry(&mut self, key: &'src str, name: Option<&'src str>) -> Option<Entry<'src>> {
        let mut params = vec![];
        let mut entries = vec![];

        loop {
            if matches!(
                self.tokens.get(self.index),
                None | Some(Token {
                    kind: TokenKind::CloseBrace,
                    ..
                })
            ) {
                break;
            }

            let token = self.tokens.get(self.index)?;
            let key = match token.kind {
                TokenKind::Ident(ident) => {
                    self.index += 1;
                    ident
                }
                kind => {
                    log::error!("{}: expected `ident`, found `{kind}`", token.location());
                    return None;
                }
            };

            let name = if let TokenKind::String(name) = self.tokens.get(self.index)?.kind {
                self.index += 1;
                Some(name)
            } else {
                None
            };

            let token = self.tokens.get(self.index)?;
            match token.kind {
                TokenKind::Eq => {
                    assert!(name.is_none());
                    self.index += 1;

                    let token = self.tokens.get(self.index)?;
                    let value = match token.kind {
                        TokenKind::Number(num) => Value::Number(num),
                        TokenKind::Bool(bool) => Value::Bool(bool),
                        TokenKind::String(str) => Value::String(str),
                        kind => {
                            log::error!("{}: expected `value`, found `{kind}`", token.location());
                            return None;
                        }
                    };
                    self.index += 1;

                    self.parse_token(TokenKind::Semi)?;
                    params.push(Param { key, value });
                }
                TokenKind::OpenBrace => {
                    self.index += 1;
                    let entry = self.parse_entry(key, name)?;
                    self.parse_token(TokenKind::CloseBrace)?;
                    entries.push(entry);
                }
                _ => {
                    log::debug!("BREAK");
                    return None;
                }
            }
        }

        Some(Entry {
            key,
            name,
            params,
            entries,
        })
    }
}

pub fn parse_config_file(input: &[u8]) -> Entry {
    let mut lexer = Lexer::new(core::str::from_utf8(input).unwrap());
    let mut tokens = vec![];
    while let Some(token) = lexer.next_token() {
        if token.kind != TokenKind::Comment {
            tokens.push(token);
        }
    }
    let mut parser = Parser { tokens, index: 0 };
    let entry = parser.parse_entry("<root>", None).unwrap();
    entry
}

#[derive(Debug)]
pub struct Entry<'src> {
    pub key: &'src str,
    pub name: Option<&'src str>,
    pub params: Vec<Param<'src>>,
    pub entries: Vec<Entry<'src>>,
}

impl<'src> Entry<'src> {
    pub fn param(&self, key: &str) -> Option<&Value<'src>> {
        self.params.iter().find_map(|param| {
            if param.key == key {
                Some(&param.value)
            } else {
                None
            }
        })
    }
}

#[derive(Debug)]
pub struct Param<'src> {
    pub key: &'src str,
    pub value: Value<'src>,
}

impl<'src> Param<'src> {}

/// A parameter value
#[derive(Debug)]
pub enum Value<'src> {
    /// A boolean
    Bool(bool),

    /// An integer in some base
    ///
    /// The contained string is guaranteed to a valid representation of an integer in
    /// the specified base. It is stored as a string here so as not to impose any restrictions
    /// on the value, the consumer can use `.parse()` to get the desired integer type.
    Number(&'src str),

    /// A string
    String(&'src str),
}
