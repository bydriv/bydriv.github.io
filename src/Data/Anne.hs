module Data.Anne(
  Anne(..), Atom(..), Datum(..)
, Term(..), Token(..)
, Cursor, Pos, ParseError(..)
, lex, parse
) where

import Prelude hiding (lex)

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

data ParseError = Unexpected Token | UnexpectedEOF
  deriving (Eq, Ord, Read, Show)

lex :: String -> [Token]
lex = lex' 0
  where
    lex' :: Cursor -> String -> [Token]
    lex' _ [] =
      []
    lex' p ('[':s) =
      let q = p + 1 in
        Token LBRACE (p, q) "[" : lex' q s
    lex' p (']':s) =
      let q = p + 1 in
        Token RBRACE (p, q) "]" : lex' q s
    lex' p ('\n':'\n':s) =
      let (b, s') = span (== '\n') s in
      let q = p + length b + 2 in
        Token BLANK (p, q) ('\n':'\n':b) : lex' q s'
    lex' p (c:s) =
      let t = lexText "" (c:s) in
      let q = p + length t in
      let s' = drop (length t) (c:s) in
        Token TEXT (p, q) t : lex' q s'

    lexText :: String -> String -> String
    lexText t []            = reverse t
    lexText t ('[':_)       = reverse t
    lexText t (']':_)       = reverse t
    lexText t ('\n':'\n':_) = reverse t
    lexText t ('\\':c:s)    = lexText (c:t) s
    lexText t (c:s)         = lexText (c:t) s

parse :: [Token] -> Either ParseError Anne
parse = either Left (Right . Anne) . consumed . parseAnne
  where
    consumed :: Either ParseError (a, [Token]) -> Either ParseError a
    consumed (Left e)           = Left e
    consumed (Right (_, (t:_))) = Left (Unexpected t)
    consumed (Right (x, []))    = Right x

    parseAnne :: [Token] -> Either ParseError ([[Datum]], [Token])
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
          Left (Unexpected t)

    parseAtom :: [Token] -> Either ParseError (Atom, [Token])
    parseAtom []                  = Left UnexpectedEOF
    parseAtom (Token TEXT p s:ts) = Right (String p s, ts)
    parseAtom (t:_)               = Left (Unexpected t)

    parseList :: [Token] -> Either ParseError ([Datum], [Token])
    parseList [] =
      Left UnexpectedEOF
    parseList (Token LBRACE _ _:ts) =
      case parseData ts of
        Left e -> Left e
        Right (_, []) -> Left UnexpectedEOF
        Right (dat, Token RBRACE _ _:ts') -> Right (dat, ts')
        Right (_, t:_) -> Left (Unexpected t)
    parseList (t:_) =
      Left (Unexpected t)

    parseDatum :: [Token] -> Either ParseError (Datum, [Token])
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

    parseData :: [Token] -> Either ParseError ([Datum], [Token])
    parseData ts =
      case parseDatum ts of
        Left _ -> Right ([], ts)
        Right (datum, ts') ->
          case parseData ts' of
            Left e -> Left e
            Right (dat, ts'') ->
              Right (datum:dat, ts'')
