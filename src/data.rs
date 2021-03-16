pub enum SExpr {
    LitNumber(f64),
    LitString(String),
    Symbol(String),
    List(Vec<SExpr>),
}
