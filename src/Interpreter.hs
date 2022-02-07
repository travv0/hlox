{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter
    ( interpret
    ) where

import           Ast
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                )
import           Data.Foldable                  ( for_ )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Token

type Interpreter = StateT InterpreterState (Except [InterpretError])

data InterpreterState = InterpreterState
    { interpreterErrors :: [InterpretError]
    }
    deriving Show

data InterpretError = InterpretError
    { interpretErrorMessage :: String
    }
    deriving Show

interpret :: Expr -> IO ()
interpret expr = case runInterpreter (evaluate expr) of
    Right lit  -> print lit
    Left  errs -> for_ errs (hPutStrLn stderr . interpretErrorMessage)

runInterpreter :: Interpreter a -> Either [InterpretError] a
runInterpreter interpreter = runExcept
    $ evalStateT interpreter (InterpreterState { interpreterErrors = [] })

evaluate :: Expr -> Interpreter Literal
evaluate expr = do
    case expr of
        ExprAssign{} -> undefined
        ExprBinary left (Token { tokenLine }, op) right -> do
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

                (badLeft, BinaryMinus, _) ->
                    typeError "number" badLeft tokenLine
                (badLeft, BinarySlash, _) ->
                    typeError "number" badLeft tokenLine
                (badLeft, BinaryStar, _) ->
                    typeError "number" badLeft tokenLine

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

                (LiteralNumber leftNum, BinaryGreater, LiteralNumber rightNum)
                    -> pure $ LiteralBool (leftNum > rightNum)
                (LiteralNumber leftNum, BinaryGreaterEqual, LiteralNumber rightNum)
                    -> pure $ LiteralBool (leftNum >= rightNum)
                (LiteralNumber leftNum, BinaryLess, LiteralNumber rightNum) ->
                    pure $ LiteralBool (leftNum < rightNum)
                (LiteralNumber leftNum, BinaryLessEqual, LiteralNumber rightNum)
                    -> pure $ LiteralBool (leftNum <= rightNum)

                (LiteralNumber _, BinaryGreater, badRight) ->
                    typeError "number" badRight tokenLine
                (LiteralNumber _, BinaryGreaterEqual, badRight) ->
                    typeError "number" badRight tokenLine
                (LiteralNumber _, BinaryLess, badRight) ->
                    typeError "number" badRight tokenLine
                (LiteralNumber _, BinaryLessEqual, badRight) ->
                    typeError "number" badRight tokenLine

                (badLeft, BinaryGreater, _) ->
                    typeError "number" badLeft tokenLine
                (badLeft, BinaryGreaterEqual, _) ->
                    typeError "number" badLeft tokenLine
                (badLeft, BinaryLess, _) ->
                    typeError "number" badLeft tokenLine
                (badLeft, BinaryLessEqual, _) ->
                    typeError "number" badLeft tokenLine

                (leftAny, BinaryBangEqual, rightAny) ->
                    pure $ LiteralBool (leftAny /= rightAny)
                (leftAny, BinaryEqualEqual, rightAny) ->
                    pure $ LiteralBool (leftAny == rightAny)

        ExprCall{}    -> undefined
        ExprLogical{} -> undefined
        ExprUnary (Token { tokenLine }, UnaryMinus) right -> do
            evaluate right >>= \case
                LiteralNumber n -> pure $ LiteralNumber (-n)
                v               -> typeError "number" v tokenLine
        ExprUnary (_, UnaryBang) right -> do
            LiteralBool . not . isTruthy <$> evaluate right
        ExprLiteral  lit -> pure lit
        ExprVariable _   -> undefined
        ExprGrouping e   -> evaluate e

typeError :: String -> Literal -> Int -> Interpreter a
typeError expected value line = undefined

isTruthy :: Literal -> Bool
isTruthy LiteralNil      = False
isTruthy (LiteralBool b) = b
isTruthy _               = True
