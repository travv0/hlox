module Main where

import           Analyzer                       ( analyze
                                                , reportAnalysisError
                                                )
import           Control.Monad.Catch            ( catchIf )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Foldable                  ( for_ )
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as Map
import           Interpreter                    ( Interpreter
                                                , interpret
                                                , reportInterpretError
                                                , runInterpreter
                                                )
import           Parser                         ( parse
                                                , reportParseError
                                                )
import           Scanner                        ( reportScanError
                                                , scan
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.IO                      ( hFlush
                                                , hPutStrLn
                                                , stderr
                                                , stdout
                                                )
import           System.IO.Error                ( isEOFError )

data RunMode
    = Interpret
    | Ast
    | Tokens
    deriving Eq

main :: IO ()
main = do
    args <- getArgs
    case args of
        []                 -> runPrompt
        [file]             -> runFile Interpret file
        ["--ast"   , file] -> runFile Ast file
        ["--tokens", file] -> runFile Tokens file
        _                  -> do
            liftIO $ hPutStrLn stderr "Usage: hlox [[--ast|--tokens] script]"
            liftIO $ exitWith $ ExitFailure 64

run :: RunMode -> String -> Interpreter ExitCode
run runMode source = do
    let (tokens, scanErrors) = scan source
    liftIO $ for_ scanErrors reportScanError

    if runMode == Tokens
        then if null scanErrors
            then liftIO $ print tokens $> ExitSuccess
            else exitError
        else case parse tokens of
            Left errors -> do
                liftIO $ for_ errors reportParseError
                exitError
            Right ast -> do
                if runMode == Ast
                    then liftIO $ print ast $> ExitSuccess
                    else case analyze ast of
                        [] -> do
                            interpret ast
                            if not (null scanErrors)
                                then exitError
                                else pure ExitSuccess
                        errs -> do
                            liftIO $ for_ errs reportAnalysisError
                            exitError
    where exitError = pure $ ExitFailure 65

runFile :: RunMode -> FilePath -> IO ()
runFile runMode path = do
    runInterpreter
            (liftIO (readFile path) >>= run runMode >>= liftIO . exitWith)
            [Map.empty]
        >>= \case
                Right _    -> pure ()
                Left  errs -> for_ errs reportInterpretError

getLineMaybe :: IO (Maybe String)
getLineMaybe = catchIf isEOFError (Just <$> getLine) (const (pure Nothing))

runPrompt :: IO ()
runPrompt = runInterpreter go [Map.empty] >>= \case
    Right _    -> runPrompt
    Left  errs -> do
        for_ errs reportInterpretError
        runPrompt
  where
    go = do
        liftIO $ putStr "> " *> hFlush stdout
        mline <- liftIO getLineMaybe
        case mline of
            Just line -> run Interpret line *> go
            Nothing   -> pure ()
