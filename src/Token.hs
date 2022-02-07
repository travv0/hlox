module Token where
import           Data.Text                      ( Text )

data TokenType
    -- Single-character tokens.
    = LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star

    -- One or two character tokens.
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    -- Literals.
    | Identifier
    | String Text
    | Number Double

    -- Keywords.
    | And
    | Class
    | Else
    | False_
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True_
    | Var
    | While

    | Eof
    deriving (Show, Eq)

data Token = Token
    { tokenType   :: TokenType
    , tokenLexeme :: Text
    , tokenLine   :: Int
    }
    deriving Show

eof :: Int -> Token
eof line = Token { tokenType = Eof, tokenLexeme = "", tokenLine = line }
