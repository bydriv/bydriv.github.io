module Data.Anne(
  Anne(..), Atom(..), Datum(..)
, Term(..), Token(..)
, Cursor, Pos, Error(..)
, lex, parse
) where

import           Prelude   hiding (lex)
import qualified Data.List              as List

data Anne = Anne [[Datum]]
  deriving (Eq, Ord, Read, Show)

data Atom =
    String Pos String
  deriving (Eq, Ord, Read, Show)

data Datum =
    Atom Atom
  | List [Datum]
  deriving (Eq, Ord, Read, Show)

data Term =
    LBRACE
  | RBRACE
  | BLANK
  | TEXT
  deriving (Eq, Ord, Read, Show)

data Token = Token Term Pos String
  deriving (Eq, Ord, Read, Show)

type Cursor = Int
type Pos = (Cursor, Cursor)

data Error =
    UnexpectedEOF
  | UnexpectedChar (Cursor, Char)
  | UnexpectedToken Token
  deriving (Eq, Ord, Read, Show)

lex :: String -> Either Error [Token]
lex = lex' 0
  where
    lex' :: Cursor -> String -> Either Error [Token]
    lex' _ [] =
      Right []
    lex' _ ['\n'] =
      Right []
    lex' p ('[':s) =
      let q = p + 1 in
        either Left (Right . (Token LBRACE (p, q) "[" :)) (lex' q s)
    lex' p (']':s) =
      let q = p + 1 in
        either Left (Right . (Token RBRACE (p, q) "]" :)) (lex' q s)
    lex' p ('\n':'\n':s) =
      let (b, s') = span (== '\n') s in
      let q = p + length b + 2 in
        either Left (Right . (Token BLANK (p, q) ('\n':'\n':b) :)) (lex' q s')
    lex' p ('\n':s) =
      lex' (p + 1) s
    lex' p ('<':'<':s) =
      let (k, s') = span (/= '\n') s in
        case lexHereDoc k "" s' of
          Left e ->
            Left e
          Right t ->
            let q = p + length t + length k in
            let s'' = drop (length t + length k) s' in
              either Left (Right . (Token TEXT (p, q) (tail t) :)) (lex' q s'')
    lex' p ('<':k:s) =
      let (t, s') = span (/= k) s in
        case s' of
          [] ->
            Left UnexpectedEOF
          (c:s'')
            | c /= k ->
                Left (UnexpectedChar (p + 2, c))
            | otherwise ->
                let q = p + length t + 3 in
                  either Left (Right . (Token TEXT (p, q) t :)) (lex' q s'')
    lex' p (c:s) =
      let (q, t) = lexText p "" (c:s) in
      let s' = drop (q - p) (c:s) in
        either Left (Right . (Token TEXT (p, q) t :)) (lex' q s')

    lexHereDoc :: String -> String -> String -> Either Error String
    lexHereDoc k t [] =
      Left UnexpectedEOF
    lexHereDoc k t ('\n':s)
      | List.isPrefixOf k s, let ('\n':s') = drop (length k) s =
          Right (reverse ('\n':t))
      | otherwise =
          lexHereDoc k ('\n':t) s
    lexHereDoc k t (c:s) =
      lexHereDoc k (c:t) s

    lexText :: Cursor -> String -> String -> (Cursor, String)
    lexText p t []            = (p, reverse t)
    lexText p t ('[':_)       = (p, reverse t)
    lexText p t (']':_)       = (p, reverse t)
    lexText p t ('\n':'\n':_) = (p, reverse t)
    lexText p t ('\n':_)      = (p, reverse t)
    lexText p t ('<':'<':_)   = (p, reverse t)
    lexText p t ('<':_)       = (p, reverse t)
    lexText p t ('\\':c:s)    = lexText (p + 2) (c:t) s
    lexText p t (c:s)         = lexText (p + 1) (c:t) s

parse :: [Token] -> Either Error Anne
parse = either Left (Right . Anne) . consumed . parseAnne
  where
    consumed :: Either Error (a, [Token]) -> Either Error a
    consumed (Left e)           = Left e
    consumed (Right (_, (t:_))) = Left (UnexpectedToken t)
    consumed (Right (x, []))    = Right x

    parseAnne :: [Token] -> Either Error ([[Datum]], [Token])
    parseAnne [] =
      Right ([], [])
    parseAnne ts =
      case parseData ts of
        Left e ->
          Left e
        Right (dat, []) ->
          Right ([dat], [])
        Right (dat, Token BLANK _ _:ts') ->
          case parseAnne ts' of
            Left e ->
              Left e
            Right (dat', ts'') ->
              Right (dat:dat', ts'')
        Right (_, t:_) ->
          Left (UnexpectedToken t)

    parseAtom :: [Token] -> Either Error (Atom, [Token])
    parseAtom []                  = Left UnexpectedEOF
    parseAtom (Token TEXT p s:ts) = Right (String p s, ts)
    parseAtom (t:_)               = Left (UnexpectedToken t)

    parseList :: [Token] -> Either Error ([Datum], [Token])
    parseList [] =
      Left UnexpectedEOF
    parseList (Token LBRACE _ _:ts) =
      case parseData ts of
        Left e -> Left e
        Right (_, []) -> Left UnexpectedEOF
        Right (dat, Token RBRACE _ _:ts') -> Right (dat, ts')
        Right (_, t:_) -> Left (UnexpectedToken t)
    parseList (t:_) =
      Left (UnexpectedToken t)

    parseDatum :: [Token] -> Either Error (Datum, [Token])
    parseDatum ts =
      case parseAtom ts of
        Right (atom, ts') ->
          Right (Atom atom, ts')
        Left _ ->
          case parseList ts of
            Right (dat, ts') ->
              Right (List dat, ts')
            Left e ->
              Left e

    parseData :: [Token] -> Either Error ([Datum], [Token])
    parseData ts =
      case parseDatum ts of
        Left _ -> Right ([], ts)
        Right (datum, ts') ->
          case parseData ts' of
            Left e -> Left e
            Right (dat, ts'') ->
              Right (datum:dat, ts'')
