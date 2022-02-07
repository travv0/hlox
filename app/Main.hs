module Main where

import           Control.Monad.Catch            ( catchIf )
import           Data.Foldable                  ( for_ )
import           Data.Functor                   ( ($>) )
import           Interpreter                    ( interpret )
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
            hPutStrLn stderr "Usage: hlox [[--ast|--tokens] script]"
            exitWith $ ExitFailure 64

run :: RunMode -> String -> IO ExitCode
run runMode source = do
    let (tokens, scanErrors) = scan source
    for_ scanErrors reportScanError

    if runMode == Tokens
        then if null scanErrors
            then print tokens $> ExitSuccess
            else pure $ ExitFailure 65
        else case parse tokens of
            Left errors -> do
                for_ errors reportParseError
                pure $ ExitFailure 65
            Right ast -> do
                if runMode == Ast
                    then print ast $> ExitSuccess
                    else do
                        interpret ast
                        if not (null scanErrors)
                            then pure $ ExitFailure 65
                            else pure ExitSuccess

runFile :: RunMode -> FilePath -> IO a
runFile runMode path = do
    readFile path >>= run runMode >>= exitWith

getLineMaybe :: IO (Maybe String)
getLineMaybe = catchIf isEOFError (Just <$> getLine) (const (pure Nothing))

runPrompt :: IO ()
runPrompt = do
    putStr "> " *> hFlush stdout
    mline <- getLineMaybe
    case mline of
        Just line -> run Interpret line *> runPrompt
        Nothing   -> pure ()
