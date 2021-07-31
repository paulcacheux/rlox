use string_interner::DefaultSymbol;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    // literals
    NumberLiteral(f64),
    StringLiteral(DefaultSymbol),
    BoolLiteral(bool),

    // identifier and keywords
    Identifier(DefaultSymbol),
    ClassKeyword,
    FunKeyword,
    VarKeyword,
    ForKeyword,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,
    PrintKeyword,
    ReturnKeyword,
    AndKeyword,
    OrKeyword,
    NilKeyword,
    ThisKeyword,
    SuperKeyword,

    // punctuations
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    SemiColon,
    Dot,
    Comma,

    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Equal,
    EqualEqual,
    BangEqual,
    Bang,
    Minus,
    Plus,
    Slash,
    Star,

    // Special
    EndOfFile,
}
