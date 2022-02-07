{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

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
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Token

type Interpreter = StateT InterpreterState (ExceptT [InterpretError] IO)

data InterpreterState = InterpreterState
    { interpreterErrors :: [InterpretError]
    }
    deriving Show

data TypeError = TypeError
    { typeErrorExpected :: String
    , typeErrorValue    :: Literal
    , typeErrorLine     :: Int
    }
    deriving Show

data InterpretError = InterpretTypeError TypeError
    deriving Show

reportInterpretError :: InterpretError -> IO ()
reportInterpretError (InterpretTypeError TypeError { typeErrorExpected, typeErrorValue, typeErrorLine })
    = hPutStrLn stderr
        $  "[line "
        <> show typeErrorLine
        <> "] Error at '"
        <> show typeErrorValue
        <> "': Expected "
        <> typeErrorExpected
        <> "."

interpret :: [Stmt] -> IO ()
interpret statements = do
    result <- runInterpreter $ traverse_ execute statements
    case result of
        Right _    -> pure ()
        Left  errs -> for_ errs reportInterpretError

runInterpreter :: Interpreter a -> IO (Either [InterpretError] a)
runInterpreter interpreter = runExceptT
    $ evalStateT interpreter (InterpreterState { interpreterErrors = [] })

execute :: Stmt -> Interpreter ()
execute (StmtExpression expr      ) = evaluate expr >> pure ()
execute (StmtBlock      statements) = for_ statements execute
execute (StmtPrint      expr      ) = do
    v <- evaluate expr
    liftIO . putStrLn $ prettyLiteral v
execute StmtFunction{}                   = undefined
execute (StmtIf cond thenBranch Nothing) = do
    c <- evaluate cond
    when (isTruthy c) $ execute thenBranch
execute (StmtIf cond thenBranch (Just elseBranch)) = do
    c <- evaluate cond
    if isTruthy c then execute thenBranch else execute elseBranch
execute StmtReturn{}          = undefined
execute StmtVar{}             = undefined
execute (StmtWhile cond body) = go
  where
    go = do
        c <- evaluate cond
        when (isTruthy c) $ do
            execute body
            go

evaluate :: Expr -> Interpreter Literal
evaluate ExprAssign{} = undefined
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

evaluate ExprCall{} = undefined
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
evaluate (ExprLiteral  lit) = pure lit
evaluate (ExprVariable _  ) = undefined
evaluate (ExprGrouping e  ) = evaluate e

typeError :: String -> Literal -> Int -> Interpreter a
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

isTruthy :: Literal -> Bool
isTruthy LiteralNil      = False
isTruthy (LiteralBool b) = b
isTruthy _               = True
