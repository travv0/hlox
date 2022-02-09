module Main where

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
    runInterpreter
            (case args of
                []                 -> runPrompt
                [file]             -> runFile Interpret file
                ["--ast"   , file] -> runFile Ast file
                ["--tokens", file] -> runFile Tokens file
                _                  -> do
                    liftIO $ hPutStrLn
                        stderr
                        "Usage: hlox [[--ast|--tokens] script]"
                    liftIO $ exitWith $ ExitFailure 64
            )
            [Map.empty]
        >>= \case
                Right _    -> pure ()
                Left  errs -> for_ errs reportInterpretError

run :: RunMode -> String -> Interpreter ExitCode
run runMode source = do
    let (tokens, scanErrors) = scan source
    liftIO $ for_ scanErrors reportScanError

    if runMode == Tokens
        then if null scanErrors
            then liftIO $ print tokens $> ExitSuccess
            else pure $ ExitFailure 65
        else case parse tokens of
            Left errors -> do
                liftIO $ for_ errors reportParseError
                pure $ ExitFailure 65
            Right ast -> do
                if runMode == Ast
                    then liftIO $ print ast $> ExitSuccess
                    else do
                        interpret ast
                        if not (null scanErrors)
                            then pure $ ExitFailure 65
                            else pure ExitSuccess

runFile :: RunMode -> FilePath -> Interpreter a
runFile runMode path = do
    liftIO (readFile path) >>= run runMode >>= liftIO . exitWith

getLineMaybe :: IO (Maybe String)
getLineMaybe = catchIf isEOFError (Just <$> getLine) (const (pure Nothing))

runPrompt :: Interpreter ()
runPrompt = do
    liftIO $ putStr "> " *> hFlush stdout
    mline <- liftIO getLineMaybe
    case mline of
        Just line -> run Interpret line *> runPrompt
        Nothing   -> pure ()
