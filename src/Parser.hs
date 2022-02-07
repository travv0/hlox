{-# LANGUAGE NamedFieldPuns #-}
module Parser
    ( parse
    , reportParseError
    ) where

import           Ast
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( Except
                                                , lift
                                                , runExcept
                                                )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , gets
                                                , modify'
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Token

type Parser = StateT ParserState (Except [ParseError])

make :: [Token] -> ParserState
make tokens = ParserState { parserTokens = tokens, parserErrors = [] }

runParser :: Parser a -> [Token] -> Either [ParseError] a
runParser parser = runExcept . evalStateT parser . make

parse :: [Token] -> Either [ParseError] Expr
parse = runParser expression

-- parse :: [Token] -> Either [ParseError] [Stmt]
-- parse = runParser go
--   where
--     go = do
--         stmt <- declaration
--         (stmt :) <$> go

data ParserState = ParserState
    { parserTokens :: [Token]
    , parserErrors :: [ParseError]
    }
    deriving Show

data ParseError = ParseError
    { parseErrorToken   :: Maybe Token
    , parseErrorMessage :: String
    }
    deriving Show

reportParseError :: ParseError -> IO ()
reportParseError ParseError { parseErrorToken = Just Token { tokenType = Eof, tokenLexeme, tokenLine }, parseErrorMessage = message }
    = hPutStrLn stderr
        $  "[line "
        <> show tokenLine
        <> "] Error at end: "
        <> message
reportParseError ParseError { parseErrorToken = Just Token { tokenLexeme, tokenLine }, parseErrorMessage = message }
    = hPutStrLn stderr
        $  "[line "
        <> show tokenLine
        <> "] Error at '"
        <> tokenLexeme
        <> "': "
        <> message
reportParseError ParseError { parseErrorToken = Nothing, parseErrorMessage = message }
    = hPutStrLn stderr $ "[Unknown location] Error: " <> message

declaration :: Parser Stmt
declaration = do
    token <- peek
    case tokenType token of
        Var -> parseOne >> varDeclaration
        Fun -> parseOne >> funDeclaration "function"
        _   -> statement

varDeclaration :: Parser Stmt
varDeclaration = do
    identifier <- match [Identifier] "Expected variable name."
    next       <- peek
    case tokenType next of
        Equal -> StmtVar identifier . Just <$> expression
        _     -> StmtVar identifier Nothing
            <$ consume Semicolon "Expected ';' after variable declaration."

funDeclaration :: String -> Parser Stmt
funDeclaration kind = do
    identifier <- match [Identifier] $ "Expected " <> kind <> " name."
    consume LeftParen $ "Expected '(' after " <> kind <> " name."
    params <- parameters
    consume LeftBrace $ "Expected '{' before " <> kind <> " body."
    StmtFunction identifier params <$> block

parameters :: Parser [Token]
parameters = undefined

statement :: Parser Stmt
statement = undefined

block :: Parser [Stmt]
block = undefined

returnStatement :: Parser Stmt
returnStatement = undefined

ifStatement :: Parser Stmt
ifStatement = undefined

whileStatement :: Parser Stmt
whileStatement = undefined

forStatement :: Parser Stmt
forStatement = undefined

printStatement :: Parser Stmt
printStatement = undefined

expressionStatement :: Parser Stmt
expressionStatement = undefined

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = undefined

call :: Parser Expr
call = undefined

arguments :: Parser [Expr]
arguments = undefined

unary :: Parser Expr
unary = undefined

bin
    :: (Expr -> (Token, op) -> Expr -> Expr)
    -> (Token -> Maybe op)
    -> Parser Expr
    -> [TokenType]
    -> Parser Expr
bin cons ofToken nextPrec opTokens = nextPrec >>= go
  where
    go left = do
        moperator <- tryMatch opTokens
        case moperator of
            Just operator -> case ofToken operator of
                Just op -> do
                    right <- nextPrec
                    go (cons left (operator, op) right)
                Nothing -> pure left
            Nothing -> pure left

logic :: Parser Expr -> [TokenType] -> Parser Expr
logic = bin ExprLogical logOfToken

logicOr :: Parser Expr
logicOr = logic logicAnd [Or]

logicAnd :: Parser Expr
logicAnd = logic equality [And]

binary :: Parser Expr -> [TokenType] -> Parser Expr
binary = bin ExprBinary binOfToken

equality :: Parser Expr
equality = binary comparison [BangEqual, EqualEqual]

comparison :: Parser Expr
comparison = binary term [Greater, GreaterEqual, Less, LessEqual]

term :: Parser Expr
term = binary factor [Minus, Plus]

factor :: Parser Expr
factor = binary unary [Slash, Star]

primary :: Parser Expr
primary = do
    token <- parseOne
    case token of
        Token { tokenType = False_ } -> pure . ExprLiteral $ LiteralBool False
        Token { tokenType = True_ } -> pure . ExprLiteral $ LiteralBool True
        Token { tokenType = Nil } -> pure $ ExprLiteral LiteralNil
        Token { tokenType = String s } -> pure . ExprLiteral $ LiteralString s
        Token { tokenType = Number n } -> pure . ExprLiteral $ LiteralNumber n
        Token { tokenType = Identifier } -> pure $ ExprVariable token
        Token { tokenType = LeftParen } ->
            ExprGrouping <$> expression <* consume
                RightParen
                "Expected '' after expression."
        _ -> throwError (Just token) "Expected expression."

peek :: Parser Token
peek = do
    tokens <- gets parserTokens
    case tokens of
        token : _ -> pure token
        _         -> throwError Nothing "Unexpected end of file."

parseOne :: Parser Token
parseOne = do
    tokens <- gets parserTokens
    case splitAt 1 tokens of
        ([token], rest) -> do
            modify' (\s -> s { parserTokens = rest })
            pure token
        _ -> throwError Nothing "Unexpected end of file."

match :: [TokenType] -> String -> Parser Token
match types message = do
    token <- peek
    if tokenType token `elem` types
        then parseOne
        else throwError (Just token) message

tryMatch :: [TokenType] -> Parser (Maybe Token)
tryMatch types = do
    token <- peek
    if tokenType token `elem` types then Just <$> parseOne else pure Nothing

consume :: TokenType -> String -> Parser ()
consume type_ message = do
    token <- parseOne
    when (tokenType token /= type_) $ throwError (Just token) message

logError :: Maybe Token -> String -> Parser ()
logError token message = modify'
    (\s -> s
        { parserErrors = ParseError { parseErrorToken   = token
                                    , parseErrorMessage = message
                                    }
                             : parserErrors s
        }
    )

throwError :: Maybe Token -> String -> Parser a
throwError token message = do
    logError token message
    errors <- gets parserErrors
    lift . throwE $ reverse errors
