#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // literals
    NumberLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),

    // identifier and keywords
    Identifier(String),
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
