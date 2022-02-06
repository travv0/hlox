module Ast where

import           Token

data Literal
    = LiteralBool Bool
    | LiteralString String
    | LiteralNumber Double
    | LiteralFunction String Int ([Literal] -> Literal)
    | LiteralNil

data BinaryOp
    = BinaryPlus
    | BinaryBangEqual
    | BinaryEqualEqual
    | BinaryMinus
    | BinarySlash
    | BinaryStar
    | BinaryGreater
    | BinaryGreaterEqual
    | BinaryLess
    | BinaryLessEqual

binOfToken :: Token -> Maybe BinaryOp
binOfToken Token { tokenType = type_ } = case type_ of
    BangEqual    -> Just BinaryBangEqual
    EqualEqual   -> Just BinaryEqualEqual
    Greater      -> Just BinaryGreater
    GreaterEqual -> Just BinaryGreaterEqual
    Less         -> Just BinaryLess
    LessEqual    -> Just BinaryLessEqual
    Minus        -> Just BinaryMinus
    Plus         -> Just BinaryPlus
    Slash        -> Just BinarySlash
    Star         -> Just BinaryStar
    _            -> Nothing

data LogicalOp = LogicalAnd | LogicalOr

logOfToken :: Token -> Maybe LogicalOp
logOfToken Token { tokenType = type_ } = case type_ of
    And -> Just LogicalAnd
    Or  -> Just LogicalOr
    _   -> Nothing

data UnaryOp = UnaryMinus | UnaryBang

data Expr
    = ExprAssign Token Expr
    | ExprBinary Expr (Token, BinaryOp) Expr
    | ExprCall Expr Token [Expr]
    | ExprLogical Expr (Token, LogicalOp) Expr
    | ExprUnary (Token, UnaryOp) Expr
    | ExprLiteral Literal
    | ExprVariable Token
    | ExprGrouping Expr

data Stmt
    = StmtExpression Expr
    | StmtFunction Token [Token] Stmt
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtPrint Expr
    | StmtReturn Token (Maybe Expr)
    | StmtVar Token (Maybe Expr)
    | StmtWhile Expr Stmt
    | StmtBlock [Stmt]