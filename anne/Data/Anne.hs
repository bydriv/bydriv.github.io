module Data.Anne(
  module Data.Anne.Defn,
  SyntaxError(..),
  parse
) where

import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State    as State
import qualified Data.Maybe             as Maybe
import           Data.Anne.Defn
import qualified Data.Anne.Parsing      as Parsing
import qualified Data.Anne.Lexing       as Lexing

data SyntaxError =
    NonterminatingRawText Pos
  | UnrecognizedCharacter Pos Char
  | UnexpectedEOF Pos
  | UnexpectedToken Pos Parsing.Token
  deriving (Eq, Ord, Read, Show)

posOf :: Parsing.Token -> Pos
posOf (Parsing.LBRACKET (p, _)) = p
posOf (Parsing.RBRACKET (p, _)) = p
posOf (Parsing.BLANK (p, _)) = p
posOf (Parsing.TEXT (p, _)) = p
posOf (Parsing.RAW1 (p, _, _)) = p
posOf (Parsing.RAWN (p, _, _)) = p

parse :: String -> Either SyntaxError Anne
parse s =
  let ((tokens0, s'), (_, bufP, bufN, k, _)) = flip State.runState (0, 0, 0, "", []) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
  let tokens = Maybe.catMaybes tokens0 in

  case k of
    (_ : _) ->
      Left (NonterminatingRawText (bufP, bufP + bufN))
    "" ->
      case s' of
        (c : _) ->
          let p = length s - length s' in
          let q = p + 1 in
            Left (UnrecognizedCharacter (p, q) c)
        [] ->
          case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
            Left Nothing ->
              let p = length s in
              let q = p in
                Left (UnexpectedEOF (p, q))
            Left (Just t) ->
              Left (UnexpectedToken (posOf t) t)
            Right (result, _) ->
              Right result
