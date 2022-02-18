{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter
    ( interpret
    , Interpreter
    , runInterpreter
    , reportInterpretError
    ) where

import           Ast
import           Control.Exception              ( throw )
import           Control.Monad                  ( unless
                                                , when
                                                , zipWithM_
                                                )
import           Control.Monad.Catch            ( Exception
                                                , catch
                                                )
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
import           Data.IORef                     ( modifyIORef'
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
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

instance Exception InterpretError
instance Exception [InterpretError]

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

interpret :: [Stmt] -> Interpreter ()
interpret statements = do
    defineGlobal "clock" clock
    traverse_ execute statements
    errors <- gets interpreterErrors
    unless (null errors) $ lift $ throwE errors

runInterpreter
    :: Interpreter a -> Environment -> IO (Either [InterpretError] a)
runInterpreter interpreter env = runExceptT $ evalStateT
    interpreter
    (InterpreterState { interpreterErrors = [], interpreterEnv = env })

data ReturnExn = ReturnExn Token Literal
    deriving Show

instance Exception ReturnExn

execute :: Stmt -> Interpreter ()
execute (StmtExpression expr      ) = evaluate expr >> pure ()
execute (StmtBlock      statements) = do
    pushEnv
    for_ statements execute
    popEnv
execute (StmtPrint expr) = do
    v <- evaluate expr
    liftIO . putStrLn $ prettyLiteral v
execute (StmtFunction (StmtFun token params body)) = do
    defineVar token undefined
    env <- gets interpreterEnv
    _   <- assignVar
        token
        (LiteralFunction
            ( LoxFunction (tokenLexeme token) (length params) Function env
            $ fnWrap params body
            )
        )
    pure ()
execute (StmtIf cond thenBranch Nothing) = do
    c <- evaluate cond
    when (isTruthy c) $ execute thenBranch
execute (StmtIf cond thenBranch (Just elseBranch)) = do
    c <- evaluate cond
    if isTruthy c then execute thenBranch else execute elseBranch
execute (StmtReturn ret (Just expr)) = do
    value <- evaluate expr
    throw (ReturnExn ret value)
execute (StmtReturn ret   Nothing    ) = throw (ReturnExn ret LiteralNil)
execute (StmtVar    token (Just expr)) = do
    value <- evaluate expr
    defineVar token value
execute (     StmtVar   token Nothing) = defineVar token LiteralNil
execute stmt@(StmtWhile cond  body   ) = do
    c <- evaluate cond
    when (isTruthy c) $ do
        execute body
        execute stmt
execute (StmtClass token@Token { tokenLine } sc classMethods) = do
    case sc of
        Just (Token { tokenLine = line }, expr) -> evaluate expr >>= \case
            LiteralClass klass -> go $ Just klass
            _                  -> throwError "Superclass must be a class." line
        Nothing -> go Nothing
  where
    go superclass = do
        defineVar token undefined
        traverse_ defineSuper $ LiteralClass <$> superclass
        env     <- gets interpreterEnv

        methods <- liftIO $ newIORef Map.empty

        liftIO
            $ for_ classMethods
            $ \(StmtFun Token { tokenLexeme } params body) ->
                  let method =
                          LoxFunction
                                  tokenLexeme
                                  (length params)
                                  (if tokenLexeme == "init"
                                      then Initializer
                                      else Method
                                  )
                                  env
                              $ fnWrap params body
                  in  modifyIORef' methods $ Map.insert tokenLexeme method

        _ <- assignVar
            token
            (LiteralClass (LoxClass (tokenLexeme token) superclass methods env))
        pure ()
    defineSuper = do
        defineVar Token { tokenLexeme = "super"
                        , tokenType   = Super
                        , tokenLine   = tokenLine
                        }

fnWrap :: [Token] -> Stmt -> [Literal] -> Environment -> a -> IO Literal
fnWrap params body args env _line = do
    let newEnv = Map.empty : env
    result <- runInterpreter
        (do
            zipWithM_ defineVar params args
            (execute body $> LiteralNil) `catch` \(ReturnExn _ v) -> pure v
        )
        newEnv
    case result of
        Left  errs -> throw errs
        Right r    -> pure r

clock :: Text -> Interpreter Literal
clock name = do
    env <- gets interpreterEnv
    pure $ LiteralFunction
        (LoxFunction
            name
            0
            Function
            env
            (\[] _ _ -> LiteralNumber . realToFrac <$> getPOSIXTime)
        )

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
    call token fn args `catch` \errs -> throwErrors errs

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
evaluate (ExprSuper token Token { tokenLexeme = methodLexeme, tokenLine = methodLine })
    = do
        getVar token >>= \case
            LiteralClass superclass ->
                findMethod superclass methodLexeme >>= \case
                    Nothing -> throwError
                        ("Undefined property '" <> methodLexeme <> "'.")
                        methodLine
                    Just fn -> do
                        obj <-
                            getVar Token { tokenLexeme = "this"
                                         , tokenType   = This
                                         , tokenLine   = methodLine
                                         }
                                >>= \o -> bindThis o fn
                        pure $ LiteralFunction obj
            _ -> error "Non-class is superclass"
evaluate (ExprThis token) = getVar token
evaluate (ExprGet expr Token { tokenLexeme, tokenLine }) = do
    evaluate expr >>= \case
        obj@(LiteralInstance _ propsRef) -> do
            props <- liftIO $ readIORef propsRef
            case Map.lookup tokenLexeme props of
                Just v  -> pure v
                Nothing -> getMethod obj tokenLexeme tokenLine >>= \case
                    Just fn -> pure $ LiteralFunction fn
                    Nothing -> throwError
                        ("No method '" <> tokenLexeme <> "' on object.")
                        tokenLine

        _ -> throwError "Only instances have fields." tokenLine
evaluate (ExprSet expr Token { tokenLexeme, tokenLine } valueExpr) =
    evaluate expr >>= \case
        LiteralInstance _ propsRef -> do
            value <- evaluate valueExpr
            liftIO $ modifyIORef' propsRef (Map.insert tokenLexeme value)
            pure value
        _ -> throwError "Only instances have fields." tokenLine

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

throwErrors :: [InterpretError] -> Interpreter a
throwErrors errs = do
    errors <- gets interpreterErrors
    lift . throwE $ reverse errors <> errs

isTruthy :: Literal -> Bool
isTruthy LiteralNil      = False
isTruthy (LiteralBool b) = b
isTruthy _               = True

call :: Token -> Literal -> [Literal] -> Interpreter Literal
call Token { tokenLine } (LiteralFunction (LoxFunction _ arity type_ env fn)) args
    | length args == arity
    = case type_ of
        Initializer -> do
            _ <- liftIO $ fn args env tokenLine
            getVar'
                Token { tokenLexeme = "this"
                      , tokenType   = This
                      , tokenLine   = tokenLine
                      }
                env
        _ -> liftIO $ fn args env tokenLine
call Token { tokenLine } (LiteralFunction (LoxFunction _ arity _ _ _)) args =
    throwError
        (  "Expected "
        <> T.pack (show arity)
        <> " arguments but got "
        <> T.pack (show (length args))
        <> "."
        )
        tokenLine
call Token { tokenLine } (LiteralClass klass) args = do
    methodsRef <- liftIO $ newIORef Map.empty
    let inst = LiteralInstance klass methodsRef
    _ <- getMethod inst "init" tokenLine >>= \case
        Just (LoxFunction _ _ _ env initializer) ->
            liftIO $ initializer args env tokenLine
        Nothing -> pure LiteralNil
    pure inst
call Token { tokenLine } literal _ = typeError "function" literal tokenLine

findMethod :: LoxClass -> Text -> Interpreter (Maybe LoxFunction)
findMethod (LoxClass _ superclass methodsRef _) name = do
    methods <- liftIO $ readIORef methodsRef
    case Map.lookup name methods of
        Just fn -> pure $ Just fn
        Nothing -> case superclass of
            Just sc -> findMethod sc name
            Nothing -> pure Nothing

getMethod :: Literal -> Text -> Int -> Interpreter (Maybe LoxFunction)
getMethod obj name line = case obj of
    LiteralInstance klass _ -> findMethod klass name >>= \case
        Just m  -> Just <$> bindThis obj m
        Nothing -> pure Nothing
    _ -> throwError "Only instances have methods." line

bindThis :: Literal -> LoxFunction -> Interpreter LoxFunction
bindThis obj (LoxFunction fnName arity type_ env fn) = do
    objRef <- liftIO $ newIORef obj
    let env' = Map.singleton "this" objRef : env
    pure $ LoxFunction fnName arity type_ env' fn

defineGlobal :: Text -> (Text -> Interpreter Literal) -> Interpreter ()
defineGlobal name fn = do
    env <- gets interpreterEnv
    go [] env
  where
    go _    []    = error "No environment"
    go seen [env] = do
        valueRef <- liftIO . newIORef =<< fn name
        modify'
            (\s -> do
                s
                    { interpreterEnv = Map.insert name valueRef env
                                           : reverse seen
                    }
            )
    go seen (env : envs) = go (env : seen) envs

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
getVar token = do
    env <- gets interpreterEnv
    getVar' token env

getVar' :: Token -> Environment -> Interpreter Literal
getVar' Token { tokenLexeme, tokenLine } [] =
    throwError ("Undefined variable '" <> tokenLexeme <> "'.") tokenLine
getVar' token@Token { tokenLexeme } (env : envs) =
    case Map.lookup tokenLexeme env of
        Just valueRef -> liftIO $ readIORef valueRef
        Nothing       -> getVar' token envs

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
