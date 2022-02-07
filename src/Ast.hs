module Ast where

import           Token

data Literal
    = LiteralBool Bool
    | LiteralString String
    | LiteralNumber Double
    | LiteralFunction String Int ([Literal] -> Literal)
    | LiteralNil

instance Eq Literal where
    LiteralNumber left == LiteralNumber right | isNaN left && isNaN right = True
    LiteralNumber left == LiteralNumber right = left == right
    LiteralBool left == LiteralBool right = left == right
    LiteralString left == LiteralString right = left == right
    LiteralNil == LiteralNil = True
    _ == _ = False

instance Show Literal where
    show (LiteralBool   True      ) = "true"
    show (LiteralBool   False     ) = "false"
    show (LiteralString s         ) = s
    show (LiteralNumber n         ) = show n
    show (LiteralFunction name _ _) = name
    show LiteralNil                 = "nil"

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
    deriving (Show, Eq)

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

data LogicalOp = LogicalAnd | LogicalOr deriving (Show, Eq)

logOfToken :: Token -> Maybe LogicalOp
logOfToken Token { tokenType = type_ } = case type_ of
    And -> Just LogicalAnd
    Or  -> Just LogicalOr
    _   -> Nothing

data UnaryOp = UnaryMinus | UnaryBang deriving (Show, Eq)

data Expr
    = ExprAssign Token Expr
    | ExprBinary Expr (Token, BinaryOp) Expr
    | ExprCall Expr Token [Expr]
    | ExprLogical Expr (Token, LogicalOp) Expr
    | ExprUnary (Token, UnaryOp) Expr
    | ExprLiteral Literal
    | ExprVariable Token
    | ExprGrouping Expr
    deriving (Show)

data Stmt
    = StmtExpression Expr
    | StmtFunction Token [Token] [Stmt]
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtPrint Expr
    | Stmtpure Token (Maybe Expr)
    | StmtVar Token (Maybe Expr)
    | StmtWhile Expr Stmt
    deriving (Show)
