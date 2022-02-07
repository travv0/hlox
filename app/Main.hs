module Main where

import           Control.Monad.Catch            ( catchIf )
import           Data.Foldable                  ( for_ )
import           Scanner
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

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> runPrompt
        [file] -> runFile file
        _      -> do
            hPutStrLn stderr "Usage: flox [script]"
            exitWith $ ExitFailure 64

run :: String -> IO ExitCode
run source = do
    let (tokens, scanErrors) = scan source
    for_ scanErrors reportScanError

    print tokens

    if not (null scanErrors)
        then return $ ExitFailure 65
        else return ExitSuccess

runFile :: FilePath -> IO a
runFile path = do
    readFile path >>= run >>= exitWith

getLineMaybe :: IO (Maybe String)
getLineMaybe = catchIf isEOFError (Just <$> getLine) (const (pure Nothing))

runPrompt :: IO ()
runPrompt = do
    putStr "> " >> hFlush stdout
    mline <- getLineMaybe
    case mline of
        Just line -> run line >> runPrompt
        Nothing   -> return ()
