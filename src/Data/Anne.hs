module Data.Anne(
  module Data.Anne.Defn,
  SyntaxError,
  parse
) where

import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State    as State
import qualified Data.Maybe             as Maybe
import           Data.Anne.Defn
import qualified Data.Anne.Parsing      as Parsing
import qualified Data.Anne.Lexing       as Lexing

data SyntaxError = SyntaxError
  deriving (Eq, Ord, Read, Show)

parse :: String -> Either SyntaxError Anne
parse s =
  let ((tokens0, s'), _) = flip State.runState (0, 0, 0, "", []) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
  let tokens = Maybe.catMaybes tokens0 in

  case s' of
    (c : _) ->
      Left SyntaxError
    [] ->
      case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
        Left Nothing ->
          Left SyntaxError
        Left (Just token) ->
          Left SyntaxError
        Right (result, _) ->
          Right result
