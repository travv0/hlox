module Scanner
    ( scan
    ) where

import           Control.Monad                  ( when )
import           Control.Monad.State            ( StateT
                                                , get
                                                , gets
                                                , liftIO
                                                , modify'
                                                , runStateT
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
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Token

type Scanner = StateT ScannerState IO

data ScannerState = ScannerState
    { scannerSource  :: String
    , scannerLine    :: Int
    , scannerTokens  :: [Token]
    , scannerErrored :: Bool
    }
    deriving Show

scan :: String -> IO ([Token], Bool)
scan input = second scannerErrored <$> runStateT scanTokens (make input)

scanTokens :: Scanner [Token]
scanTokens = do
    ScannerState { scannerSource = source, scannerTokens = tokens, scannerLine = line } <-
        get
    case source of
        [] -> return . reverse $ eof line : tokens
        _  -> scanToken >> scanTokens

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

make :: String -> ScannerState
make source = ScannerState { scannerSource  = source
                           , scannerLine    = 1
                           , scannerTokens  = []
                           , scannerErrored = False
                           }

reportError :: Int -> String -> Scanner ()
reportError line message = do
    liftIO $ hPutStrLn stderr $ "[line " <> show line <> "] Error: " <> message
    modify' (\s -> s { scannerErrored = True })

addToken :: TokenType -> String -> Int -> Scanner ()
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
            return $ Just c
        _ -> return Nothing

scanWhile :: (Char -> Bool) -> Scanner String
scanWhile f = do
    source <- gets scannerSource
    let (cs, rest) = splitWhile f source
    modify'
        (\s -> s { scannerSource = rest
                 , scannerLine   = scannerLine s + length (filter (== '\n') cs)
                 }
        )
    return cs

scanIdentifier :: Char -> Scanner ()
scanIdentifier first = do
    line  <- gets scannerLine
    ident <- (first :) <$> scanWhile (\c -> c == '_' || isAlphaNum c)
    let typ = fromMaybe Identifier (Map.lookup ident keywords)
    addToken typ ident line

consume :: Char -> String -> Scanner ()
consume char message = do
    line <- gets scannerLine
    c    <- scanOne
    when (c /= Just char) $ reportError line message

scanNumber :: Char -> Scanner ()
scanNumber first = do
    line   <- gets scannerLine
    num    <- (first :) <$> scanWhile isDigit
    source <- gets scannerSource
    number <- case source of
        '.' : c : _ | isDigit c -> do
            consume '.' "Expected '.' in number."
            cs <- scanWhile isDigit
            return $ num ++ '.' : cs
        _ -> return num
    addToken (Number (read number)) number line

scanString :: Scanner ()
scanString = do
    line <- gets scannerLine
    str  <- scanWhile (/= '"') <* consume '"' "Expected \" at end of string"
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
        (Just c1@'(', _         )           -> addToken LeftParen [c1] line
        (Just c1@')', _         )           -> addToken RightParen [c1] line
        (Just c1@'{', _         )           -> addToken LeftBrace [c1] line
        (Just c1@'}', _         )           -> addToken RightBrace [c1] line
        (Just c1@',', _         )           -> addToken Comma [c1] line
        (Just c1@'.', _         )           -> addToken Dot [c1] line
        (Just c1@'-', _         )           -> addToken Minus [c1] line
        (Just c1@'+', _         )           -> addToken Plus [c1] line
        (Just c1@';', _         )           -> addToken Semicolon [c1] line
        (Just c1@'*', _         )           -> addToken Star [c1] line
        (Just c1@'!', c2@'=' : _)           -> addToken BangEqual [c1, c2] line
        (Just c1@'!', _         )           -> addToken Bang [c1] line
        (Just c1@'=', c2@'=' : _)           -> addToken EqualEqual [c1, c2] line
        (Just c1@'=', _         )           -> addToken Equal [c1] line
        (Just c1@'<', c2@'=' : _)           -> addToken LessEqual [c1, c2] line
        (Just c1@'<', _         )           -> addToken Less [c1] line
        (Just c1@'>', c2@'=' : _) -> addToken GreaterEqual [c1, c2] line
        (Just c1@'>', _         )           -> addToken Greater [c1] line
        (Just '/'   , '/' : _   )           -> comment
        (Just c1@'/', _         )           -> addToken Slash [c1] line

        (Just ' '   , _         )           -> return ()
        (Just '\r'  , _         )           -> return ()
        (Just '\t'  , _         )           -> return ()
        (Just '\n'  , _         )           -> return ()

        (Just '"'   , _         )           -> scanString

        (Just d, _) | isDigit d             -> scanNumber d
        (Just a, _) | isAlpha a || a == '_' -> scanIdentifier a

        (Just u, _) ->
            reportError line $ "Unexpected character: '" <> [u] <> "'"
        (Nothing, _) -> error "Unexpected end of file"
