module  Data.Anne.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as MonadTrans
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
dfa2FinalStates = [4,5]

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
      (4, 3) -> 5
      (4, 4) -> 4
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
dfa5FinalStates = [1,2]

dfa5Transition :: Int -> Char -> Int
dfa5Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 1
          13 -> 2
          92 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(60,60),(91,91),(93,93)] then 3
            else 0 in
    case (q, c') of
      (1, 0) -> 1
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 4) -> 3
      (3, 0) -> 1
      (3, 1) -> 1
      (3, 2) -> 1
      (3, 3) -> 1
      (3, 4) -> 1
      _ -> 0

dfa6InitialState :: Int
dfa6InitialState = 1

dfa6FinalStates :: [Int]
dfa6FinalStates = [3]

dfa6Transition :: Int -> Char -> Int
dfa6Transition q c =
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

dfa7InitialState :: Int
dfa7InitialState = 1

dfa7FinalStates :: [Int]
dfa7FinalStates = [4,5]

dfa7Transition :: Int -> Char -> Int
dfa7Transition q c =
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
      (4, 3) -> 5
      (4, 4) -> 4
      _ -> 0

dfa8InitialState :: Int
dfa8InitialState = 1

dfa8FinalStates :: [Int]
dfa8FinalStates = [2]

dfa8Transition :: Int -> Char -> Int
dfa8Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          91 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa9InitialState :: Int
dfa9InitialState = 1

dfa9FinalStates :: [Int]
dfa9FinalStates = [2]

dfa9Transition :: Int -> Char -> Int
dfa9Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [1,2]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 1
          13 -> 2
          92 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(60,60),(91,91),(93,93)] then 3
            else 0 in
    case (q, c') of
      (1, 0) -> 1
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 4) -> 3
      (3, 0) -> 1
      (3, 1) -> 1
      (3, 2) -> 1
      (3, 3) -> 1
      (3, 4) -> 1
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
          _ ->
            0 in
    case (q, c') of
      (1, 0) -> 2
      _ -> 0

dfa12InitialState :: Int
dfa12InitialState = 1

dfa12FinalStates :: [Int]
dfa12FinalStates = [1,2]

dfa12Transition :: Int -> Char -> Int
dfa12Transition q c =
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
      (1, 3) -> 2
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
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ (Nothing, 1 :: Int) of
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
                x <- saText actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Middle then
      case max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -6 -> do
                x <- saRaw1 actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- saRawN actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -8 -> do
                x <- saLeftBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- saRightBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- saText actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Raw1 then
      case max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -11 -> do
                x <- saRaw1Char actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == RawN then
      case max (match dfa12InitialState dfa12FinalStates dfa12Transition s, -12) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -12 -> do
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
  , saText = withPosition $ \p n yytext -> do
      yybegin Initial
      return $ Just $ Parsing.TEXT ((p, p + n), unescape yytext)
  , saRaw1Char = withPosition $ \_ n yytext -> do
      (_, bufP, bufN, k, _) <- MonadTrans.lift State.get

      if yytext == k then do
        s <- flushBuffer
        yybegin Initial
        return $ Just $ Parsing.RAW1 ((bufP, bufN + n), head k, s)
      else do
        appendToBuffer n yytext
        return Nothing
  , saRawNLine = withPosition $ \_ n yytext -> do
      (_, bufP, bufN, k, _) <- MonadTrans.lift State.get

      if yytext == k then do
        s <- flushBuffer
        yybegin Initial
        return $ Just $ Parsing.RAWN ((bufP, bufN + n), k, s)
      else do
        appendToBuffer n yytext
        return Nothing }

