module Ast where

import           Data.IORef                     ( IORef )
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Token

type Environment = [Map Text (IORef Literal)]

data LoxClass = LoxClass Text
                         (Maybe LoxClass)
                         (IORef (Map Text LoxFunction))
                         Environment

data FunctionType
    = Function
    | Method
    | Initializer

data LoxFunction = LoxFunction Text
                               Int
                               FunctionType
                               Environment
                               ([Literal] -> Environment -> Int -> IO Literal)

data Literal
    = LiteralBool Bool
    | LiteralString Text
    | LiteralNumber Double
    | LiteralFunction LoxFunction
    | LiteralClass LoxClass
    | LiteralInstance LoxClass (IORef (Map Text Literal))
    | LiteralNil

instance Eq Literal where
    LiteralNumber left == LiteralNumber right | isNaN left && isNaN right = True
    LiteralNumber left == LiteralNumber right = left == right
    LiteralBool left == LiteralBool right = left == right
    LiteralString left == LiteralString right = left == right
    LiteralNil == LiteralNil = True
    _ == _ = False

prettyLiteral :: Literal -> Text
prettyLiteral (LiteralString s) = s
prettyLiteral (LiteralNumber n) =
    let num = T.pack $ show n in fromMaybe num . T.stripSuffix ".0" $ num
prettyLiteral l = T.pack $ show l

instance Show Literal where
    show (LiteralBool   True ) = "true"
    show (LiteralBool   False) = "false"
    show (LiteralString s    ) = show s
    show (LiteralNumber n    ) = show n
    show (LiteralFunction (LoxFunction name _ _ _ _)) =
        "<fn " <> T.unpack name <> ">"
    show (LiteralClass (LoxClass name _ _ _)) =
        "<class " <> T.unpack name <> ">"
    show (LiteralInstance (LoxClass name _ _ _) _) =
        "<instance " <> T.unpack name <> ">"
    show LiteralNil = "nil"

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
    | ExprGet Expr Token
    | ExprLogical Expr (Token, LogicalOp) Expr
    | ExprSet Expr Token Expr
    | ExprSuper Token Token
    | ExprThis Token
    | ExprUnary (Token, UnaryOp) Expr
    | ExprLiteral Literal
    | ExprVariable Token
    | ExprGrouping Expr
    deriving Show

data StmtFun = StmtFun Token [Token] Stmt
    deriving Show

data Stmt
    = StmtExpression Expr
    | StmtFunction StmtFun
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtPrint Expr
    | StmtReturn Token (Maybe Expr)
    | StmtVar Token (Maybe Expr)
    | StmtWhile Expr Stmt
    | StmtBlock [Stmt]
    | StmtClass Token (Maybe (Token, Expr)) [StmtFun]
    deriving Show
