{-# LANGUAGE NamedFieldPuns #-}

module Scanner where

import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char                      ( isAlphaNum
                                                , isDigit
                                                )
import qualified Data.List                     as List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Token

data Scanner = Scanner
    { scannerSource :: String
    , scannerLine   :: Int
    , scannerTokens :: [Token]
    }

keywords = Map.fromList
    [ ("and"   , And)
    , ("class" , Class)
    , ("else"  , Else)
    , ("false" , False_)
    , ("fun"   , Fun)
    , ("for"   , For)
    , ("if"    , If)
    , ("nil"   , Nil)
    , ("or"    , Or)
    , ("print" , Print)
    , ("return", Return)
    , ("super" , Super)
    , ("this"  , This)
    , ("true"  , True_)
    , ("var"   , Var)
    , ("while" , While)
    ]

make source =
    Scanner { scannerSource = source, scannerLine = 1, scannerTokens = [] }

addToken token newSource tokenLength = do
    scanner@Scanner { scannerLine, scannerSource } <- get
    let text = List.take tokenLength source
    scanner
        { scannerSource = newSource
        , scannerTokens = Token { tokenType   = token
                                , tokenLexeme = text
                                , tokenLine   = line
                                }
                          : scanner
                          . Tokens
        }

scanIdentifier = do
    let (ident, rest) = List.splitAt 1 source
    let (ident, rest) =
            first (++ i) $ splitWhen (\c -> c == '_' || isAlphaNum c) rest
    let tokenType = fromMaybe Identifier (Map.lookup ident keywords)
    addToken scanner tokenType rest (List.length ident)

scanNumber = do
    let (num, rest) = splitWhen isDigit source
    let (num, rest) = case rest of
            '.' : c : rest | isDigit c -> do
                let (cs, rest) = splitWhen isDigit rest
                (num ++ '.' : c : cs, rest)
            _ -> (num, rest)
    addToken scanner (Number (num |> float)) rest (List.length num)

scanString = do
    let (str, rest) = source |> List.tail |> splitWhen (/= '"')

    let line        = line + (str |> filter (== '\n') |> length)

    case rest of
        [] -> do
            Error.Report (line, "Unterminated string.")
            scanner { scannerLine = line, scannerSource = [] }
        _ -> do
            let scanner =
                    addToken scanner (String str) rest (List.length str + 2)

            scanner { scannerLine   = line
                    , scannerSource = List.tail scanner . Source
                    }

scanToken scanner = case scanner of
    Scanner { scannerSource = source, scannerLine = line } -> case source of
        '('       : source -> addToken scanner LeftParen source 1
        ')'       : source -> addToken scanner RightParen source 1
        '{'       : source -> addToken scanner LeftBrace source 1
        '}'       : source -> addToken scanner RightBrace source 1
        ','       : source -> addToken scanner Comma source 1
        '.'       : source -> addToken scanner Dot source 1
        '-'       : source -> addToken scanner Minus source 1
        '+'       : source -> addToken scanner Plus source 1
        ';'       : source -> addToken scanner Semicolon source 1
        '*'       : source -> addToken scanner Star source 1
        '!' : '=' : source -> addToken scanner BangEqual source 2
        '!'       : source -> addToken scanner Bang source 1
        '=' : '=' : source -> addToken scanner EqualEqual source 2
        '='       : source -> addToken scanner Equal source 1
        '<' : '=' : source -> addToken scanner LessEqual source 2
        '<'       : source -> addToken scanner Less source 1
        '>' : '=' : source -> addToken scanner GreaterEqual source 2
        '>'       : source -> addToken scanner Greater source 1
        '/' : '/' : source ->
            scanner { scannerSource = source |> skipWhen (/= '\n') }
        '/'  : source -> addToken scanner Slash source 1

        ' '  : source -> scanner { scannerSource = source }
        '\r' : source -> scanner { scannerSource = source }
        '\t' : source -> scanner { scannerSource = source }

        '\n' : source -> scanner { scannerLine   = scannerLine scanner + 1
                                 , scannerSource = source
                                 }

        '"' : _                       -> scanString scanner

        c : _ | isDigit c             -> scanNumber scanner
        c : _ | isAlpha c || c == '_' -> scanIdentifier scanner

        c : source                    -> Error.Report
            (line, $ "Unexpected character: '%c{c}'")
            scanner { scannerSource = source }

scanTokens scanner = case scanner of
    Scanner { scannerSource = [], scannerTokens = tokens, scannerLine = line }
        -> eof line : tokens |> List.rev
    scanner -> scanToken scanner |> scanTokens
