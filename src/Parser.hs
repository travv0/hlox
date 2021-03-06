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
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn )
import           System.IO                      ( stderr )
import           Token

type Parser = StateT ParserState (Except [ParseError])

make :: [Token] -> ParserState
make tokens = ParserState { parserTokens = tokens, parserErrors = [] }

runParser :: Parser a -> [Token] -> Either [ParseError] a
runParser parser = runExcept . evalStateT parser . make

parse :: [Token] -> Either [ParseError] [Stmt]
parse = runParser go
  where
    go = do
        next <- peek
        case tokenType next of
            Eof -> do
                errors <- gets parserErrors
                case errors of
                    [] -> pure []
                    _  -> lift $ throwE errors
            _ -> do
                stmt <- declaration
                (stmt :) <$> go

data ParserState = ParserState
    { parserTokens :: [Token]
    , parserErrors :: [ParseError]
    }
    deriving Show

data ParseError = ParseError
    { parseErrorToken   :: Maybe Token
    , parseErrorMessage :: Text
    }
    deriving Show

reportParseError :: ParseError -> IO ()
reportParseError ParseError { parseErrorToken = Just Token { tokenType = Eof, tokenLine }, parseErrorMessage = message }
    = hPutStrLn stderr
        $  "[line "
        <> T.pack (show tokenLine)
        <> "] Error at end: "
        <> message
reportParseError ParseError { parseErrorToken = Just Token { tokenLexeme, tokenLine }, parseErrorMessage = message }
    = hPutStrLn stderr
        $  "[line "
        <> T.pack (show tokenLine)
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
        Var   -> parseOne *> varDeclaration
        Class -> parseOne *> classDeclaration
        Fun   -> parseOne *> (StmtFunction <$> funDeclaration "function")
        _     -> statement

varDeclaration :: Parser Stmt
varDeclaration = do
    identifier <- match [Identifier] "Expect variable name."
    next       <- peek
    value      <- case tokenType next of
        Equal -> parseOne *> (Just <$> expression)
        _     -> pure Nothing
    consume Semicolon "Expect ';' after variable declaration."
    pure $ StmtVar identifier value

classDeclaration :: Parser Stmt
classDeclaration = do
    name       <- match [Identifier] "Expect class name."
    next       <- peek
    superclass <- case tokenType next of
        Less ->
            parseOne *> (Just <$> match [Identifier] "Expect superclass name.")
        _ -> pure Nothing
    let scNode = fmap (\sc -> (sc, ExprVariable sc)) superclass
    consume LeftBrace "Expect '{' before class body."
    mthds <- methods
    consume RightBrace "Expect '}' after class body."
    pure $ StmtClass name scNode mthds

methods :: Parser [StmtFun]
methods = do
    next <- peek
    case tokenType next of
        RightBrace -> pure []
        Eof        -> pure []
        _          -> (:) <$> funDeclaration "method" <*> methods

funDeclaration :: Text -> Parser StmtFun
funDeclaration kind = do
    identifier <- match [Identifier] $ "Expect " <> kind <> " name."
    consume LeftParen $ "Expect '(' after " <> kind <> " name."
    params <- parameters
    consume LeftBrace $ "Expect '{' before " <> kind <> " body."
    StmtFun identifier params <$> block

parameters :: Parser [Token]
parameters = do
    next <- parseOne
    case tokenType next of
        RightParen -> pure []
        Identifier -> do
            afterIdent <- parseOne
            case tokenType afterIdent of
                Comma -> do
                    params <- parameters
                    when (length params >= 255) $ do
                        n <- peek
                        logError (Just n) "Can't have more than 255 parameters."
                    pure $ next : params
                RightParen -> pure [next]
                _          -> throwError (Just afterIdent)
                                         "Expect ')' after parameters."
        _ -> throwError (Just next) "Expect ')' or parameter."

statement :: Parser Stmt
statement = do
    token <- peek
    case tokenType token of
        For       -> parseOne *> forStatement
        If        -> parseOne *> ifStatement
        While     -> parseOne *> whileStatement
        Return    -> returnStatement
        Print     -> parseOne *> printStatement
        LeftBrace -> parseOne *> block
        _         -> expressionStatement

block :: Parser Stmt
block = StmtBlock <$> go
  where
    go = do
        next <- peek
        case tokenType next of
            Eof        -> throwError (Just next) "Expect '}' after block."
            RightBrace -> parseOne $> []
            _          -> do
                decl  <- declaration
                decls <- go
                pure $ decl : decls

returnStatement :: Parser Stmt
returnStatement = do
    token <- parseOne
    next  <- peek
    StmtReturn token <$> case tokenType next of
        Semicolon -> parseOne $> Nothing
        _         -> do
            expr <- expression
                <* consume Semicolon "Expect ';' after return statement."
            pure $ Just expr

ifStatement :: Parser Stmt
ifStatement = do
    consume LeftParen "Expect '(' after 'if'."
    condition <- expression
    consume RightParen "Expect ')' after if condition."
    thenBranch <- statement
    next       <- peek
    elseBranch <- case tokenType next of
        Else -> parseOne *> (Just <$> statement)
        _    -> pure Nothing
    pure $ StmtIf condition thenBranch elseBranch

whileStatement :: Parser Stmt
whileStatement = do
    consume LeftParen "Expect '(' after 'while'."
    condition <- expression
    consume RightParen "Expect ')' after while condition."
    StmtWhile condition <$> statement

forStatement :: Parser Stmt
forStatement = do
    consume LeftParen "Expect '(' after 'for'."

    initPeek    <- peek
    initializer <- case tokenType initPeek of
        Semicolon -> pure Nothing
        Var       -> parseOne *> (Just <$> varDeclaration)
        _         -> Just <$> expressionStatement
    condPeek  <- peek
    condition <- case tokenType condPeek of
        Semicolon -> pure Nothing
        _         -> Just <$> expression <* consume
            Semicolon
            "Expect ';' after loop condition."
    incPeek   <- peek
    increment <- case tokenType incPeek of
        RightParen -> pure Nothing
        _ -> Just <$> expression <* consume RightParen
                                            "Expect ')' after for clauses."

    body <- statement

    let incBody = case increment of
            Nothing  -> body
            Just inc -> StmtBlock [body, StmtExpression inc]
        condBody = case condition of
            Nothing   -> StmtWhile (ExprLiteral (LiteralBool True)) incBody
            Just cond -> StmtWhile cond incBody

    pure $ case initializer of
        Nothing      -> condBody
        Just initial -> StmtBlock [initial, condBody]


printStatement :: Parser Stmt
printStatement = do
    value <- expression <* consume Semicolon "Expect ';' after value."
    pure $ StmtPrint value

expressionStatement :: Parser Stmt
expressionStatement = do
    expr <- expression <* consume Semicolon "Expect ';' after expression."
    pure $ StmtExpression expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = do
    expr <- logicOr
    next <- peek
    case tokenType next of
        Equal -> do
            value <- parseOne *> assignment
            case expr of
                ExprVariable v      -> pure $ ExprAssign v value
                ExprGet object name -> pure $ ExprSet object name value
                _                   -> do
                    logError (Just next) "Invalid assignment target"
                    pure expr
        _ -> pure expr

unary :: Parser Expr
unary = do
    op <- peek
    case tokenType op of
        Bang  -> ExprUnary (op, UnaryBang) <$> primary
        Minus -> ExprUnary (op, UnaryMinus) <$> primary
        _     -> call

call :: Parser Expr
call = primary >>= go
  where
    go expr = do
        next <- peek
        case tokenType next of
            LeftParen -> do
                args <- parseOne *> arguments
                go $ ExprCall expr next args
            Dot -> do
                name <- parseOne
                    *> match [Identifier] "Expect property name after '.'."
                go $ ExprGet expr name
            _ -> pure expr

arguments :: Parser [Expr]
arguments = do
    next <- peek
    case tokenType next of
        RightParen -> parseOne $> []
        _          -> do
            arg        <- expression
            afterIdent <- parseOne
            case tokenType afterIdent of
                Comma -> do
                    args <- arguments
                    when (length args >= 255) $ do
                        n <- peek
                        logError (Just n) "Can't have more than 255 arguments."
                    pure $ arg : args
                RightParen -> pure [arg]
                _ -> throwError (Just afterIdent) "Expect ')' after arguments."

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
    case tokenType token of
        False_   -> pure . ExprLiteral $ LiteralBool False
        True_    -> pure . ExprLiteral $ LiteralBool True
        Nil      -> pure $ ExprLiteral LiteralNil
        String s -> pure . ExprLiteral $ LiteralString s
        Number n -> pure . ExprLiteral $ LiteralNumber n
        Super    -> do
            consume Dot "Expect '.' after 'super'."
            method <- match [Identifier] "Expect superclass method name."
            pure $ ExprSuper token method
        This       -> pure $ ExprThis token
        Identifier -> pure $ ExprVariable token
        LeftParen  -> ExprGrouping <$> expression <* consume
            RightParen
            "Expect ')' after expression."
        _ -> throwError (Just token) "Expect expression."

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

match :: [TokenType] -> Text -> Parser Token
match types message = do
    token <- peek
    if tokenType token `elem` types
        then parseOne
        else throwError (Just token) message

tryMatch :: [TokenType] -> Parser (Maybe Token)
tryMatch types = do
    token <- peek
    if tokenType token `elem` types then Just <$> parseOne else pure Nothing

consume :: TokenType -> Text -> Parser ()
consume type_ message = do
    token <- parseOne
    when (tokenType token /= type_) $ throwError (Just token) message

logError :: Maybe Token -> Text -> Parser ()
logError token message = modify'
    (\s -> s
        { parserErrors = ParseError { parseErrorToken   = token
                                    , parseErrorMessage = message
                                    }
                             : parserErrors s
        }
    )

throwError :: Maybe Token -> Text -> Parser a
throwError token message = do
    logError token message
    errors <- gets parserErrors
    lift . throwE $ reverse errors
