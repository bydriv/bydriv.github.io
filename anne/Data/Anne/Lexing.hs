module  Data.Anne.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Control.Monad.State as State
import qualified Data.Anne.Parsing   as Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  | Middle
  | Raw1
  | RawN
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { saBlank :: String -> Lexing m a
  , saRaw1 :: String -> Lexing m a
  , saRawN :: String -> Lexing m a
  , saLeftBracket :: String -> Lexing m a
  , saRightBracket :: String -> Lexing m a
  , saLeftBrace :: String -> Lexing m a
  , saRightBrace :: String -> Lexing m a
  , saText :: String -> Lexing m a
  , saRaw1Char :: String -> Lexing m a
  , saRawNLine :: String -> Lexing m a }

instance Monad m => Functor (Lexing m) where
  fmap = Monad.liftM

instance Monad m => Applicative.Applicative (Lexing m) where
  pure = return
  (<*>) = Monad.ap

instance Monad m => Monad (Lexing m) where
  return x = Lexing $ \s -> return (x, s)
  Lexing f >>= k = Lexing $ \s -> do
    (x, s') <- f s
    unLexing (k x) s'

instance MonadTrans.MonadTrans Lexing where
  lift m = Lexing $ \s -> do { x <- m; return (x, s) }

runLexing :: Monad m => Lexing m a -> m a
runLexing = Monad.liftM fst . flip unLexing Initial

yybegin :: Monad m => LexingState -> Lexing m ()
yybegin s = Lexing $ const $ return ((), s)

dfa0InitialState :: Int
dfa0InitialState = 1

dfa0FinalStates :: [Int]
dfa0FinalStates = [2]

dfa0Transition :: Int -> Char -> Int
dfa0Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 1
          13 -> 2
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      _ -> 0

dfa1InitialState :: Int
dfa1InitialState = 1

dfa1FinalStates :: [Int]
dfa1FinalStates = [3]

dfa1Transition :: Int -> Char -> Int
dfa1Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          60 -> 2
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,59),(61,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 2) -> 2
      (2, 0) -> 3
      (2, 1) -> 3
      _ -> 0

dfa2InitialState :: Int
dfa2InitialState = 1

dfa2FinalStates :: [Int]
dfa2FinalStates = [4,5,6]

dfa2Transition :: Int -> Char -> Int
dfa2Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 2
          13 -> 3
          60 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,9),(11,12),(14,59),(61,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 4) -> 2
      (2, 0) -> 3
      (2, 4) -> 3
      (3, 0) -> 4
      (3, 1) -> 4
      (3, 4) -> 4
      (4, 0) -> 4
      (4, 1) -> 4
      (4, 2) -> 5
      (4, 3) -> 6
      (4, 4) -> 4
      (6, 0) -> 5
      (6, 2) -> 5
      _ -> 0

dfa3InitialState :: Int
dfa3InitialState = 1

dfa3FinalStates :: [Int]
dfa3FinalStates = [2]

dfa3Transition :: Int -> Char -> Int
dfa3Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          91 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa4InitialState :: Int
dfa4InitialState = 1

dfa4FinalStates :: [Int]
dfa4FinalStates = [2]

dfa4Transition :: Int -> Char -> Int
dfa4Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa5InitialState :: Int
dfa5InitialState = 1

dfa5FinalStates :: [Int]
dfa5FinalStates = [2]

dfa5Transition :: Int -> Char -> Int
dfa5Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          123 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa6InitialState :: Int
dfa6InitialState = 1

dfa6FinalStates :: [Int]
dfa6FinalStates = [2]

dfa6Transition :: Int -> Char -> Int
dfa6Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          125 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa7InitialState :: Int
dfa7InitialState = 1

dfa7FinalStates :: [Int]
dfa7FinalStates = [1,2,3,4,6]

dfa7Transition :: Int -> Char -> Int
dfa7Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 2
          13 -> 3
          60 -> 4
          91 -> 5
          92 -> 6
          93 -> 7
          123 -> 8
          125 -> 9
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,9),(11,12),(14,59),(61,90),(94,122),(124,124),(126,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 1) -> 1
      (1, 2) -> 3
      (1, 3) -> 4
      (1, 6) -> 5
      (2, 0) -> 2
      (2, 1) -> 1
      (2, 2) -> 1
      (2, 3) -> 6
      (2, 4) -> 1
      (2, 5) -> 1
      (2, 6) -> 2
      (2, 7) -> 1
      (2, 8) -> 1
      (2, 9) -> 1
      (4, 0) -> 3
      (4, 2) -> 3
      (5, 0) -> 6
      (5, 2) -> 1
      (5, 3) -> 6
      (5, 4) -> 1
      (5, 5) -> 1
      (5, 6) -> 1
      (5, 7) -> 1
      (5, 8) -> 1
      (5, 9) -> 1
      (6, 0) -> 2
      (6, 1) -> 1
      (6, 2) -> 1
      (6, 3) -> 4
      (6, 6) -> 5
      _ -> 0

dfa8InitialState :: Int
dfa8InitialState = 1

dfa8FinalStates :: [Int]
dfa8FinalStates = [3]

dfa8Transition :: Int -> Char -> Int
dfa8Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          60 -> 2
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,59),(61,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 2) -> 2
      (2, 0) -> 3
      (2, 1) -> 3
      _ -> 0

dfa9InitialState :: Int
dfa9InitialState = 1

dfa9FinalStates :: [Int]
dfa9FinalStates = [4,5,6]

dfa9Transition :: Int -> Char -> Int
dfa9Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 2
          13 -> 3
          60 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,9),(11,12),(14,59),(61,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 4) -> 2
      (2, 0) -> 3
      (2, 4) -> 3
      (3, 0) -> 4
      (3, 1) -> 4
      (3, 4) -> 4
      (4, 0) -> 4
      (4, 1) -> 4
      (4, 2) -> 5
      (4, 3) -> 6
      (4, 4) -> 4
      (6, 0) -> 5
      (6, 2) -> 5
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [2]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          91 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa11InitialState :: Int
dfa11InitialState = 1

dfa11FinalStates :: [Int]
dfa11FinalStates = [2]

dfa11Transition :: Int -> Char -> Int
dfa11Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa12InitialState :: Int
dfa12InitialState = 1

dfa12FinalStates :: [Int]
dfa12FinalStates = [2]

dfa12Transition :: Int -> Char -> Int
dfa12Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          123 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa13InitialState :: Int
dfa13InitialState = 1

dfa13FinalStates :: [Int]
dfa13FinalStates = [2]

dfa13Transition :: Int -> Char -> Int
dfa13Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          125 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa14InitialState :: Int
dfa14InitialState = 1

dfa14FinalStates :: [Int]
dfa14FinalStates = [1,2,3,4,6]

dfa14Transition :: Int -> Char -> Int
dfa14Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 2
          13 -> 3
          60 -> 4
          91 -> 5
          92 -> 6
          93 -> 7
          123 -> 8
          125 -> 9
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,9),(11,12),(14,59),(61,90),(94,122),(124,124),(126,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 1) -> 1
      (1, 2) -> 3
      (1, 3) -> 4
      (1, 6) -> 5
      (2, 0) -> 2
      (2, 1) -> 1
      (2, 2) -> 1
      (2, 3) -> 6
      (2, 4) -> 1
      (2, 5) -> 1
      (2, 6) -> 2
      (2, 7) -> 1
      (2, 8) -> 1
      (2, 9) -> 1
      (4, 0) -> 3
      (4, 2) -> 3
      (5, 0) -> 6
      (5, 2) -> 1
      (5, 3) -> 6
      (5, 4) -> 1
      (5, 5) -> 1
      (5, 6) -> 1
      (5, 7) -> 1
      (5, 8) -> 1
      (5, 9) -> 1
      (6, 0) -> 2
      (6, 1) -> 1
      (6, 2) -> 1
      (6, 3) -> 4
      (6, 6) -> 5
      _ -> 0

dfa15InitialState :: Int
dfa15InitialState = 1

dfa15FinalStates :: [Int]
dfa15FinalStates = [2]

dfa15Transition :: Int -> Char -> Int
dfa15Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          _ ->
            0 in
    case (q, c') of
      (1, 0) -> 2
      _ -> 0

dfa16InitialState :: Int
dfa16InitialState = 1

dfa16FinalStates :: [Int]
dfa16FinalStates = [1,2,3]

dfa16Transition :: Int -> Char -> Int
dfa16Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 2
          13 -> 3
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,9),(11,12),(14,1114111)] then 1
            else 0 in
    case (q, c') of
      (1, 0) -> 1
      (1, 1) -> 1
      (1, 2) -> 2
      (1, 3) -> 3
      (3, 0) -> 2
      (3, 2) -> 2
      _ -> 0

match :: Int -> [Int] -> (Int -> Char -> Int) -> String -> Maybe Int
match initialState finalStates transition = match' 0 Nothing initialState
  where
    match' :: Int -> Maybe Int -> Int -> String -> Maybe Int
    match' _ r 0 _ =
      r
    match' i r q s =
      let r' =
            if q `elem` finalStates then
              Just i
            else
              r in
        case s of
          [] ->
            r'
          (c : s') ->
            let q' = transition q c in
              match' (i + 1) r' q' s'

lex :: Monad m => SemanticActions m a -> String -> Lexing m ([a], String)
lex actions = lex' where
  lex' s = do
    p <- Lexing $ \p -> return (p, p)
    if p == Initial then
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -0 -> do
                x <- saBlank actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -1 -> do
                x <- saRaw1 actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -2 -> do
                x <- saRawN actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -3 -> do
                x <- saLeftBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -4 -> do
                x <- saRightBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -5 -> do
                x <- saLeftBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -6 -> do
                x <- saRightBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- saText actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Middle then
      case max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ max (match dfa12InitialState dfa12FinalStates dfa12Transition s, -12) $ max (match dfa13InitialState dfa13FinalStates dfa13Transition s, -13) $ max (match dfa14InitialState dfa14FinalStates dfa14Transition s, -14) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -8 -> do
                x <- saRaw1 actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- saRawN actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- saLeftBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -11 -> do
                x <- saRightBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -12 -> do
                x <- saLeftBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -13 -> do
                x <- saRightBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -14 -> do
                x <- saText actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Raw1 then
      case max (match dfa15InitialState dfa15FinalStates dfa15Transition s, -15) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -15 -> do
                x <- saRaw1Char actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == RawN then
      case max (match dfa16InitialState dfa16FinalStates dfa16Transition s, -16) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -16 -> do
                x <- saRawNLine actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else
      return ([], s)



type S = (Int, Int, Int, String, [String])

withPosition :: (Int -> Int -> String -> Lexing (State.State S) (Maybe Parsing.Token)) -> String -> Lexing (State.State S) (Maybe Parsing.Token)
withPosition f yytext = do
  let n = length yytext
  (pos, bufP, bufN, k, buf) <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put (pos + n, bufP, bufN, k, buf)
  f pos n yytext

clearBuffer :: Lexing (State.State S) ()
clearBuffer = do
  (pos, _, _, _, _) <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put (pos, 0, 0, "", [])

setBuffer :: Int -> Int -> String -> Lexing (State.State S) ()
setBuffer p n k = do
  (pos, _, _, _, _) <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put (pos, p, n, k, [])

appendToBuffer :: Int -> String -> Lexing (State.State S) ()
appendToBuffer n s = do
  (pos, bufP, bufN, k, buf) <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put (pos, bufP, bufN + n, k, s:buf)

flushBuffer :: Lexing (State.State S) String
flushBuffer = do
  (_, _, _, _, buf) <- MonadTrans.lift State.get
  clearBuffer
  return $ concat $ reverse buf

unescape :: String -> String
unescape [] = []
unescape ('\\':c:s) = c : unescape s
unescape (c:s) = c : unescape s

semanticActions :: SemanticActions (State.State S) (Maybe Parsing.Token)
semanticActions = SemanticActions
  { saBlank = withPosition $ \p n yytext ->
      return $ Just $ Parsing.BLANK ((p, p + n), yytext)
  , saRaw1 = withPosition $ \p n yytext -> do
      yybegin Raw1
      setBuffer p n (tail yytext)
      return Nothing
  , saRawN = withPosition $ \p n yytext -> do
      yybegin RawN
      setBuffer p n (drop 2 yytext)
      return Nothing
  , saLeftBracket = withPosition $ \p n yytext -> do
      yybegin Middle
      return $ Just $ Parsing.LBRACKET ((p, p + n), yytext)
  , saRightBracket = withPosition $ \p n yytext -> do
      yybegin Middle
      return $ Just $ Parsing.RBRACKET ((p, p + n), yytext)
  , saLeftBrace = withPosition $ \p n yytext -> do
      yybegin Initial
      return $ Just $ Parsing.LBRACE ((p, p + n), yytext)
  , saRightBrace = withPosition $ \p n yytext -> do
      yybegin Middle
      return $ Just $ Parsing.RBRACE ((p, p + n), yytext)
  , saText = withPosition $ \p n yytext -> do
      yybegin Initial
      return $ Just $ Parsing.TEXT ((p, p + n), unescape yytext)
  , saRaw1Char = withPosition $ \_ n yytext -> do
      (_, bufP, bufN, k, _) <- MonadTrans.lift State.get

      if yytext == k then do
        s <- flushBuffer
        yybegin Initial
        return $ Just $ Parsing.RAW1 ((bufP, bufP + bufN + n), head k, s)
      else do
        appendToBuffer n yytext
        return Nothing
  , saRawNLine = withPosition $ \_ n yytext -> do
      (_, bufP, bufN, k, _) <- MonadTrans.lift State.get

      if yytext == k then do
        s <- flushBuffer
        yybegin Initial
        return $ Just $ Parsing.RAWN ((bufP, bufP + bufN + n), k, s)
      else do
        appendToBuffer n yytext
        return Nothing }

