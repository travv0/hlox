module Analyzer where
import           Ast                            ( Expr(..)
                                                , Stmt(..)
                                                , StmtFun(StmtFun)
                                                )
import           Control.Monad.State            ( State
                                                , execState
                                                , gets
                                                , modify'
                                                , when
                                                )
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn )
import           System.IO                      ( stderr )
import           Token                          ( Token(..)
                                                , TokenType(Eof)
                                                )

data FunctionType
    = FTNone
    | FTFunction
    | FTInitializer
    | FTMethod
    deriving (Show, Eq)

data ClassType
    = CTNone
    | CTClass
    | CTSubclass
    deriving (Show, Eq)

type Analyzer = State AnalyzerState

type AnalysisError = (Token, Text)

data AnalyzerState = AnalyzerState
    { analyzerErrors          :: [AnalysisError]
    , analyzerScopes          :: [Map Text Bool]
    , analyzerCurrentFunction :: FunctionType
    , analyzerCurrentClass    :: ClassType
    }

analyze :: Foldable t => t Stmt -> [(Token, Text)]
analyze ast = reverse . analyzerErrors $ execState
    (for_ ast $ \stmt -> analyzeStmt stmt)
    AnalyzerState { analyzerErrors          = []
                  , analyzerScopes          = []
                  , analyzerCurrentFunction = FTNone
                  , analyzerCurrentClass    = CTNone
                  }

reportAnalysisError :: AnalysisError -> IO ()
reportAnalysisError (Token { tokenType = Eof, tokenLine }, message) =
    hPutStrLn stderr
        $  "[line "
        <> T.pack (show tokenLine)
        <> "] Error at end: "
        <> message
reportAnalysisError (Token { tokenLexeme, tokenLine }, message) =
    hPutStrLn stderr
        $  "[line "
        <> T.pack (show tokenLine)
        <> "] Error at '"
        <> tokenLexeme
        <> "': "
        <> message

beginScope :: Analyzer ()
beginScope =
    modify' (\s -> s { analyzerScopes = Map.empty : analyzerScopes s })

endScope :: Analyzer ()
endScope = modify' (\s -> s { analyzerScopes = tail $ analyzerScopes s })

analysisError :: Token -> Text -> Analyzer ()
analysisError token message =
    modify' (\s -> s { analyzerErrors = (token, message) : analyzerErrors s })

declare :: Token -> Analyzer ()
declare name@Token { tokenLexeme } = gets analyzerScopes >>= \case
    scope : scopes -> case Map.lookup tokenLexeme scope of
        Just _ -> analysisError
            name
            "Already a variable with this name in this scope."
        Nothing -> modify'
            (\s -> s
                { analyzerScopes = Map.insert tokenLexeme False scope : scopes
                }
            )
    _ -> pure ()

define :: Token -> Analyzer ()
define Token { tokenLexeme } = gets analyzerScopes >>= \case
    scope : scopes -> modify'
        (\s -> s { analyzerScopes = Map.insert tokenLexeme True scope : scopes }
        )
    _ -> pure ()

peek :: Token -> Analyzer (Maybe Bool)
peek Token { tokenLexeme } = gets analyzerScopes >>= \case
    scope : _ -> pure $ Map.lookup tokenLexeme scope
    _         -> pure Nothing

analyzeExpr :: Expr -> Analyzer ()
analyzeExpr expr = case expr of
    ExprLiteral  _     -> pure ()

    ExprVariable token -> do
        peek token >>= \case
            Just False -> analysisError
                token
                "Can't read local variable in its own initializer."
            _ -> pure ()

    ExprThis token -> gets analyzerCurrentClass >>= \case
        CTNone -> analysisError token "Can't use 'this' outside of a class."
        _      -> pure ()

    ExprAssign _ binding    -> analyzeExpr binding

    ExprBinary left _ right -> do
        analyzeExpr left
        analyzeExpr right

    ExprCall callee _ args -> do
        analyzeExpr callee
        for_ args $ \arg -> analyzeExpr arg

    ExprGrouping e           -> analyzeExpr e

    ExprLogical left _ right -> do
        analyzeExpr left
        analyzeExpr right

    ExprUnary _   right -> analyzeExpr right

    ExprGet   obj _     -> analyzeExpr obj

    ExprSet obj _ value -> do
        analyzeExpr obj
        analyzeExpr value

    ExprSuper token _ -> gets analyzerCurrentClass >>= \case
        currentClass | currentClass == CTNone ->
            analysisError token "Can't use 'super' outside of a class."
        currentClass | currentClass /= CTSubclass -> analysisError
            token
            "Can't use 'super' in a class with no superclass."
        _ -> pure ()

analyzeStmt :: Stmt -> Analyzer ()
analyzeStmt stmt = case stmt of
    StmtIf cond thenBranch elseBranch -> do
        analyzeExpr cond
        analyzeStmt thenBranch
        traverse_ analyzeStmt elseBranch

    StmtVar token binding -> do
        declare token
        traverse_ analyzeExpr binding
        define token

    StmtFunction fn@(StmtFun token _ _) -> do
        declare token
        define token

        analyzeFunction fn FTFunction

    StmtBlock stmts -> do
        beginScope
        for_ stmts $ \statement -> analyzeStmt statement
        endScope

    StmtExpression expr     -> analyzeExpr expr

    StmtPrint      expr     -> analyzeExpr expr

    StmtReturn keyword expr -> do
        currentFunction <- gets analyzerCurrentFunction
        when (currentFunction == FTNone)
            $ analysisError keyword "Can't return from top-level code."

        traverse_
            (\v -> do
                when (currentFunction == FTInitializer) $ analysisError
                    keyword
                    "Can't return a value from an initializer."
                analyzeExpr v
            )
            expr

    StmtWhile cond body -> do
        analyzeExpr cond
        analyzeStmt body

    StmtClass name@Token { tokenLexeme = nameLexeme } superclass methods -> do
        enclosingClass <- gets analyzerCurrentClass
        modify' (\s -> s { analyzerCurrentClass = CTClass })

        declare name
        define name

        traverse_
            (\(scName@Token { tokenLexeme = scLexeme }, sc) -> do
                when (nameLexeme == scLexeme) $ analysisError
                    scName
                    "A class can't inherit from itself."

                modify' (\s -> s { analyzerCurrentClass = CTSubclass })
                analyzeExpr sc

                beginScope
                modify'
                    (\s ->
                        let scope : scopes = analyzerScopes s
                        in  s
                                { analyzerScopes = Map.insert "super" True scope
                                                       : scopes
                                }
                    )
            )
            superclass

        beginScope
        modify'
            (\s ->
                let scope : scopes = analyzerScopes s
                in  s { analyzerScopes = Map.insert "this" True scope : scopes }
            )

        for_ methods $ \method@(StmtFun Token { tokenLexeme } _ _) ->
            let declaration = if tokenLexeme == "init"
                    then FTInitializer
                    else FTMethod
            in  analyzeFunction method declaration

        endScope

        traverse_ (const endScope) superclass

        modify' (\s -> s { analyzerCurrentClass = enclosingClass })

analyzeFunction :: StmtFun -> FunctionType -> Analyzer ()
analyzeFunction (StmtFun _ params body) fnType = do
    enclosingFunction <- gets analyzerCurrentFunction
    modify' (\s -> s { analyzerCurrentFunction = fnType })

    beginScope

    for_ params $ \param -> do
        declare param
        define param

    analyzeStmt body

    endScope

    modify' (\s -> s { analyzerCurrentFunction = enclosingFunction })
