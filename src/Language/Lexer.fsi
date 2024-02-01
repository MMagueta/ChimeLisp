module Lexer

/// Rule tokenStream
val tokenStream: lexbuf: LexBuffer<char> -> token
/// Rule read_string
val read_string: str: obj -> ignorequote: obj -> lexbuf: LexBuffer<char> -> token
