{-# LANGUAGE FlexibleContexts #-}

module Interpreter
    ( interpret
    ) where

import           Ast
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( ExceptT
                                                , lift
                                                , runExceptT
                                                )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , gets
                                                , liftIO
                                                , modify'
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import           Data.Functor                   ( ($>) )
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn
                                                , putStrLn
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Prelude                 hiding ( putStrLn )
import           System.IO                      ( stderr )
import           Token

type Interpreter = StateT InterpreterState (ExceptT [InterpretError] IO)

type Environment = [Map Text (IORef Literal)]

data InterpreterState = InterpreterState
    { interpreterErrors :: [InterpretError]
    , interpreterEnv    :: Environment
    }

data TypeError = TypeError
    { typeErrorExpected :: Text
    , typeErrorValue    :: Literal
    , typeErrorLine     :: Int
    }
    deriving Show

data InterpretError
    = InterpretTypeError TypeError
    | InterpretOtherError Text Int
    deriving Show

reportInterpretError :: InterpretError -> IO ()
reportInterpretError (InterpretTypeError TypeError { typeErrorExpected, typeErrorValue, typeErrorLine })
    = hPutStrLn stderr
        $  "[line "
        <> T.pack (show typeErrorLine)
        <> "] Error at '"
        <> T.pack (show typeErrorValue)
        <> "': Expected "
        <> typeErrorExpected
        <> "."
reportInterpretError (InterpretOtherError message line) =
    hPutStrLn stderr $ "[line " <> T.pack (show line) <> "] " <> message

interpret :: [Stmt] -> IO ()
interpret statements = do
    result <- runInterpreter $ do
        defineGlobal "clock" clock
        traverse_ execute statements
    case result of
        Right _    -> pure ()
        Left  errs -> for_ errs reportInterpretError

runInterpreter :: Interpreter a -> IO (Either [InterpretError] a)
runInterpreter interpreter = runExceptT $ evalStateT
    interpreter
    (InterpreterState { interpreterErrors = [], interpreterEnv = [Map.empty] })

execute :: Stmt -> Interpreter ()
execute (StmtExpression expr      ) = evaluate expr >> pure ()
execute (StmtBlock      statements) = do
    pushEnv
    for_ statements execute
    popEnv
execute (StmtPrint expr) = do
    v <- evaluate expr
    liftIO . putStrLn $ prettyLiteral v
execute StmtFunction{}                   = undefined
execute (StmtIf cond thenBranch Nothing) = do
    c <- evaluate cond
    when (isTruthy c) $ execute thenBranch
execute (StmtIf cond thenBranch (Just elseBranch)) = do
    c <- evaluate cond
    if isTruthy c then execute thenBranch else execute elseBranch
execute StmtReturn{}                = undefined
execute (StmtVar token (Just expr)) = do
    value <- evaluate expr
    defineVar token value
execute (StmtVar token Nothing) = do
    defineVar token LiteralNil
execute (StmtWhile cond body) = go
  where
    go = do
        c <- evaluate cond
        when (isTruthy c) $ do
            execute body
            go

clock :: Text -> Literal
clock name =
    LiteralFunction name 0 (\[] -> LiteralNumber . realToFrac <$> getPOSIXTime)

evaluate :: Expr -> Interpreter Literal
evaluate (ExprAssign token expr) = do
    value <- evaluate expr
    assignVar token value
evaluate (ExprBinary left (Token { tokenLine }, op) right) = do
    l <- evaluate left
    r <- evaluate right
    case (l, op, r) of
        (LiteralNumber leftNum, BinaryMinus, LiteralNumber rightNum) ->
            pure $ LiteralNumber (leftNum - rightNum)
        (LiteralNumber leftNum, BinarySlash, LiteralNumber rightNum) ->
            pure $ LiteralNumber (leftNum / rightNum)
        (LiteralNumber leftNum, BinaryStar, LiteralNumber rightNum) ->
            pure $ LiteralNumber (leftNum * rightNum)

        (LiteralNumber _, BinaryMinus, badRight) ->
            typeError "number" badRight tokenLine
        (LiteralNumber _, BinarySlash, badRight) ->
            typeError "number" badRight tokenLine
        (LiteralNumber _, BinaryStar, badRight) ->
            typeError "number" badRight tokenLine

        (badLeft, BinaryMinus, _) -> typeError "number" badLeft tokenLine
        (badLeft, BinarySlash, _) -> typeError "number" badLeft tokenLine
        (badLeft, BinaryStar , _) -> typeError "number" badLeft tokenLine

        (LiteralNumber leftNum, BinaryPlus, LiteralNumber rightNum) ->
            pure $ LiteralNumber (leftNum + rightNum)
        (LiteralString leftStr, BinaryPlus, LiteralString rightStr) ->
            pure $ LiteralString (leftStr <> rightStr)

        (LiteralNumber _, BinaryPlus, badRight) ->
            typeError "number" badRight tokenLine
        (LiteralString _, BinaryPlus, badRight) ->
            typeError "string" badRight tokenLine
        (badLeft, BinaryPlus, _) ->
            typeError "number or string" badLeft tokenLine

        (LiteralNumber leftNum, BinaryGreater, LiteralNumber rightNum) ->
            pure $ LiteralBool (leftNum > rightNum)
        (LiteralNumber leftNum, BinaryGreaterEqual, LiteralNumber rightNum) ->
            pure $ LiteralBool (leftNum >= rightNum)
        (LiteralNumber leftNum, BinaryLess, LiteralNumber rightNum) ->
            pure $ LiteralBool (leftNum < rightNum)
        (LiteralNumber leftNum, BinaryLessEqual, LiteralNumber rightNum) ->
            pure $ LiteralBool (leftNum <= rightNum)

        (LiteralNumber _, BinaryGreater, badRight) ->
            typeError "number" badRight tokenLine
        (LiteralNumber _, BinaryGreaterEqual, badRight) ->
            typeError "number" badRight tokenLine
        (LiteralNumber _, BinaryLess, badRight) ->
            typeError "number" badRight tokenLine
        (LiteralNumber _, BinaryLessEqual, badRight) ->
            typeError "number" badRight tokenLine

        (badLeft, BinaryGreater, _) -> typeError "number" badLeft tokenLine
        (badLeft, BinaryGreaterEqual, _) ->
            typeError "number" badLeft tokenLine
        (badLeft, BinaryLess     , _) -> typeError "number" badLeft tokenLine
        (badLeft, BinaryLessEqual, _) -> typeError "number" badLeft tokenLine

        (leftAny, BinaryBangEqual, rightAny) ->
            pure $ LiteralBool (leftAny /= rightAny)
        (leftAny, BinaryEqualEqual, rightAny) ->
            pure $ LiteralBool (leftAny == rightAny)

evaluate (ExprCall callee token argExprs) = do
    fn   <- evaluate callee
    args <- traverse evaluate argExprs
    call token fn args

evaluate (ExprLogical leftExpr (_, LogicalAnd) rightExpr) = do
    left <- evaluate leftExpr
    if isTruthy left then evaluate rightExpr else pure left
evaluate (ExprLogical leftExpr (_, LogicalOr) rightExpr) = do
    left <- evaluate leftExpr
    if isTruthy left then pure left else evaluate rightExpr
evaluate (ExprUnary (Token { tokenLine }, UnaryMinus) right) = do
    evaluate right >>= \case
        LiteralNumber n -> pure $ LiteralNumber (-n)
        v               -> typeError "number" v tokenLine
evaluate (ExprUnary (_, UnaryBang) right) = do
    LiteralBool . not . isTruthy <$> evaluate right
evaluate (ExprLiteral  lit  ) = pure lit
evaluate (ExprVariable token) = getVar token
evaluate (ExprGrouping e    ) = evaluate e

typeError :: Text -> Literal -> Int -> Interpreter a
typeError expected value line = do
    modify'
        (\s -> s
            { interpreterErrors = InterpretTypeError TypeError
                                          { typeErrorExpected = expected
                                          , typeErrorValue    = value
                                          , typeErrorLine     = line
                                          }
                                      : interpreterErrors s
            }
        )
    errors <- gets interpreterErrors
    lift . throwE $ reverse errors

throwError :: Text -> Int -> Interpreter a
throwError message line = do
    modify'
        (\s -> s
            { interpreterErrors = InterpretOtherError message line
                                      : interpreterErrors s
            }
        )
    errors <- gets interpreterErrors
    lift . throwE $ reverse errors

isTruthy :: Literal -> Bool
isTruthy LiteralNil      = False
isTruthy (LiteralBool b) = b
isTruthy _               = True

pushEnv :: Interpreter ()
pushEnv = modify' (\s -> s { interpreterEnv = Map.empty : interpreterEnv s })

popEnv :: Interpreter ()
popEnv = modify' (\s -> s { interpreterEnv = tail $ interpreterEnv s })

defineVar :: Token -> Literal -> Interpreter ()
defineVar Token { tokenLexeme } value = do
    valueRef <- liftIO $ newIORef value
    modify'
        (\s ->
            let (env : envs) = interpreterEnv s
            in  s
                    { interpreterEnv = Map.insert tokenLexeme valueRef env
                                           : envs
                    }
        )

getVar :: Token -> Interpreter Literal
getVar Token { tokenLexeme, tokenLine } = do
    env <- gets interpreterEnv
    go env
  where
    go [] =
        throwError ("Undefined variable '" <> tokenLexeme <> "'.") tokenLine
    go (env : envs) = case Map.lookup tokenLexeme env of
        Just valueRef -> liftIO $ readIORef valueRef
        Nothing       -> go envs

assignVar :: Token -> Literal -> Interpreter Literal
assignVar Token { tokenLexeme, tokenLine } value = do
    env <- gets interpreterEnv
    go env
  where
    go [] =
        throwError ("Undefined variable '" <> tokenLexeme <> "'.") tokenLine
    go (env : envs) = case Map.lookup tokenLexeme env of
        Just valueRef -> liftIO $ writeIORef valueRef value $> value
        Nothing       -> go envs

call :: Token -> Literal -> [Literal] -> Interpreter Literal
call _ (LiteralFunction _ arity fn) args | length args == arity =
    liftIO $ fn args
call Token { tokenLine } (LiteralFunction _ arity _) args = throwError
    (  "Expected "
    <> T.pack (show arity)
    <> " arguments but got "
    <> T.pack (show (length args))
    <> "."
    )
    tokenLine
call Token { tokenLine } literal _ = typeError "function" literal tokenLine

defineGlobal :: Text -> (Text -> Literal) -> Interpreter ()
defineGlobal name fn = do
    env <- gets interpreterEnv
    go [] env
  where
    go _    []    = error "No environment"
    go seen [env] = do
        valueRef <- liftIO . newIORef $ fn name
        modify'
            (\s -> do
                s
                    { interpreterEnv = Map.insert name valueRef env
                                           : reverse seen
                    }
            )
    go seen (env : envs) = go (env : seen) envs
