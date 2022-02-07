module Scanner
    ( scan
    , reportScanError
    ) where

import           Control.Monad                  ( when )
import           Control.Monad.State            ( State
                                                , get
                                                , gets
                                                , modify'
                                                , runState
                                                )
import           Data.Bifunctor                 ( bimap
                                                , second
                                                )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn )
import           System.IO                      ( stderr )
import           Token

type Scanner = State ScannerState

data ScanError = ScanError
    { scanErrorLine    :: Int
    , scanErrorMessage :: Text
    }
    deriving Show

data ScannerState = ScannerState
    { scannerSource :: [Char]
    , scannerLine   :: Int
    , scannerTokens :: [Token]
    , scannerErrors :: [ScanError]
    }
    deriving Show

scan :: [Char] -> ([Token], [ScanError])
scan = second (reverse . scannerErrors) . runState scanTokens . make

reportScanError :: ScanError -> IO ()
reportScanError ScanError { scanErrorLine = line, scanErrorMessage = message }
    = hPutStrLn stderr
        $  "[line "
        <> T.pack (show line)
        <> "] Error: "
        <> message

scanTokens :: Scanner [Token]
scanTokens = do
    ScannerState { scannerSource = source, scannerTokens = tokens, scannerLine = line } <-
        get
    case source of
        [] -> pure . reverse $ eof line : tokens
        _  -> scanToken *> scanTokens

keywords :: Map [Char] TokenType
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

make :: [Char] -> ScannerState
make source = ScannerState { scannerSource = source
                           , scannerLine   = 1
                           , scannerTokens = []
                           , scannerErrors = []
                           }

scanError :: Int -> Text -> Scanner ()
scanError line message = do
    modify'
        (\s -> s
            { scannerErrors =
                ScanError { scanErrorLine = line, scanErrorMessage = message }
                    : scannerErrors s
            }
        )

addToken :: TokenType -> Text -> Int -> Scanner ()
addToken token lexeme line = do
    modify'
        (\s -> s
            { scannerTokens = Token { tokenType   = token
                                    , tokenLexeme = lexeme
                                    , tokenLine   = line
                                    }
                                  : scannerTokens s
            }
        )

splitWhile :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
splitWhile f = bimap reverse reverse . snd . foldl'
    (\(done, (xs, ys)) x -> if done || not (f x)
        then (True, (xs, x : ys))
        else (done, (x : xs, ys))
    )
    (False, ([], []))

scanOne :: Scanner (Maybe Char)
scanOne = do
    source <- gets scannerSource
    case splitAt 1 source of
        ([c], rest) -> do
            modify'
                (\s -> s
                    { scannerSource = rest
                    , scannerLine   = if c == '\n'
                                          then scannerLine s + 1
                                          else scannerLine s
                    }
                )
            pure $ Just c
        _ -> pure Nothing

scanWhile :: (Char -> Bool) -> Scanner [Char]
scanWhile f = do
    source <- gets scannerSource
    let (cs, rest) = splitWhile f source
    modify'
        (\s -> s { scannerSource = rest
                 , scannerLine   = scannerLine s + length (filter (== '\n') cs)
                 }
        )
    pure cs

scanIdentifier :: Char -> Scanner ()
scanIdentifier first = do
    line  <- gets scannerLine
    ident <- (first :) <$> scanWhile (\c -> c == '_' || isAlphaNum c)
    let type_ = fromMaybe Identifier (Map.lookup ident keywords)
    addToken type_ (T.pack ident) line

consume :: Char -> Text -> Scanner ()
consume char message = do
    line <- gets scannerLine
    c    <- scanOne
    when (c /= Just char) $ scanError line message

scanNumber :: Char -> Scanner ()
scanNumber first = do
    line   <- gets scannerLine
    num    <- (first :) <$> scanWhile isDigit
    source <- gets scannerSource
    number <- case source of
        '.' : c : _ | isDigit c -> do
            consume '.' "Failed to consume '.' in number."
            cs <- scanWhile isDigit
            pure $ num ++ '.' : cs
        _ -> pure num
    addToken (Number (read number)) (T.pack number) line

scanString :: Scanner ()
scanString = do
    line <- gets scannerLine
    str  <- T.pack <$> scanWhile (/= '"') <* consume '"' "Unterminated string."
    addToken (String str) str line

comment :: Scanner ()
comment =
    modify' (\s -> s { scannerSource = dropWhile (/= '\n') $ scannerSource s })

scanToken :: Scanner ()
scanToken = do
    line   <- gets scannerLine
    c      <- scanOne
    source <- gets scannerSource
    case (c, source) of
        (Just '(' , _      )                -> addToken LeftParen "(" line
        (Just ')' , _      )                -> addToken RightParen ")" line
        (Just '{' , _      )                -> addToken LeftBrace "{" line
        (Just '}' , _      )                -> addToken RightBrace "}" line
        (Just ',' , _      )                -> addToken Comma "," line
        (Just '.' , _      )                -> addToken Dot "." line
        (Just '-' , _      )                -> addToken Minus "-" line
        (Just '+' , _      )                -> addToken Plus "+" line
        (Just ';' , _      )                -> addToken Semicolon ";" line
        (Just '*' , _      )                -> addToken Star "*" line
        (Just '!', '=' : _) -> scanOne *> addToken BangEqual "!=" line
        (Just '!' , _      )                -> addToken Bang "!" line
        (Just '=', '=' : _) -> scanOne *> addToken EqualEqual "==" line
        (Just '=' , _      )                -> addToken Equal "=" line
        (Just '<', '=' : _) -> scanOne *> addToken LessEqual "<=" line
        (Just '<' , _      )                -> addToken Less "<" line
        (Just '>', '=' : _) -> scanOne *> addToken GreaterEqual ">=" line
        (Just '>' , _      )                -> addToken Greater ">" line
        (Just '/' , '/' : _)                -> comment
        (Just '/' , _      )                -> addToken Slash "/" line

        (Just ' ' , _      )                -> pure ()
        (Just '\r', _      )                -> pure ()
        (Just '\t', _      )                -> pure ()
        (Just '\n', _      )                -> pure ()

        (Just '"' , _      )                -> scanString

        (Just d, _) | isDigit d             -> scanNumber d
        (Just a, _) | isAlpha a || a == '_' -> scanIdentifier a

        (Just u, _) ->
            scanError line $ "Unexpected character: '" <> T.pack [u] <> "'"
        (Nothing, _) -> error "Unexpected end of file"
