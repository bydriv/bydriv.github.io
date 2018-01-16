-- Data.Regexp.hs © 2018 Kaoru Kawamukai <bydriv@gmail.com>
--
-- Data.Regexp.hs is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Data.Regexp.hs is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Data.Regexp.hs.  If not, see <http://www.gnu.org/licenses/>.

module Data.Regexp
  ( Regexp
  , empty
  , epsilon
  , to
  , generalCategory
  , (>>>)
  , (|||)
  , many
  , some
  , optional
  , any
  , char
  , string
  , oneOf
  , noneOf
  , not
  , (&&&)
  , diff
  , xor
  , convertRegexpToDFA
  , fillTable
  , minimize
  , convertDFAToRegexp
  , runDFAToRegexp ) where

import           Prelude
  hiding (any, lex, not)
import qualified Prelude
import qualified Control.Monad                as Monad
import qualified Control.Monad.State          as State
import qualified Data.Char                    as Char
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.RBMap                   as RBMap
import qualified Data.RBSet                   as RBSet
import qualified Data.Word                    as Word

infix 5 `to`
infixr 4 :::, >>>
infixr 3 &&&
infixr 2 :|:, |||

type Range = (Char, Char)

data Regexp =
    Empty
  | Epsilon
  | Ranges [Range]
  | Regexp ::: Regexp
  | Regexp :|: Regexp
  | Many Regexp
  deriving (Eq, Ord, Read, Show)

type NFAState = Word.Word64

data NFALabel =
    NFAEpsilon
  | NFARanges [Range]
  deriving (Eq, Ord, Read, Show)

type NFATransition = RBMap.RBMap (NFAState, NFALabel) (RBSet.RBSet NFAState)

data NFA = NFA
  { nfaTransition :: NFATransition
  , nfaInitialState :: NFAState
  , nfaFinalStates :: RBSet.RBSet NFAState }
  deriving (Eq, Ord, Read, Show)

type DFAState = Word.Word64

data DFALabel =
    DFARanges [Range]
  deriving (Eq, Ord, Read, Show)

type DFATransition = RBMap.RBMap (DFAState, DFALabel) DFAState

data DFA = DFA
  { dfaInitialState :: DFAState
  , dfaFinalStates :: RBSet.RBSet DFAState
  , dfaTransition :: DFATransition }
  deriving (Eq, Ord, Read, Show)

data RegexpToNFAState = RegexpToNFAState
  { nfaFreshState :: NFAState }
  deriving (Eq, Ord, Read, Show)

type RegexpToNFA = State.State RegexpToNFAState

data NFAToDFAState = NFAToDFAState
  { dfaStates :: RBMap.RBMap DFAState (RBSet.RBSet NFAState)
  , dfaFreshState :: DFAState
  , dfaCurrentState :: DFAState
  , dfaTrans :: DFATransition }
  deriving (Eq, Ord, Read, Show)

type NFAToDFA = State.State NFAToDFAState

type DFAToRegexpState =
  (Word.Word64, Word.Word64, RBMap.RBMap (Word.Word64, Word.Word64) Regexp, RBSet.RBSet Word.Word64)

type DFAToRegexp = State.State DFAToRegexpState

nullRange :: Range -> Bool
nullRange (x, y) = x > y

subsetRange :: Range -> Range -> Bool
subsetRange (x, y) (x', y') = x' <= x && y <= y'

empty :: Regexp
empty = Empty

epsilon :: Regexp
epsilon = Epsilon

to :: Char -> Char -> Regexp
c1 `to` c2 = Ranges [(c1, c2)]

generalCategory :: Char.GeneralCategory -> Regexp
generalCategory = maybe Empty Ranges . flip RBMap.lookup generalCategoryInfo

(>>>) :: Regexp -> Regexp -> Regexp
(>>>) = (:::)

(|||) :: Regexp -> Regexp -> Regexp
(|||) = (:|:)

many :: Regexp -> Regexp
many = Many

some :: Regexp -> Regexp
some regexp = regexp ::: Many regexp

optional :: Regexp -> Regexp
optional = (:|: Epsilon)

any :: Regexp
any = Ranges [(minBound, maxBound)]

char :: Char -> Regexp
char c = Ranges [(c, c)]

string :: String -> Regexp
string = foldr (:::) Epsilon . map char

oneOf :: [Char] -> Regexp
oneOf = Ranges . minimizeRanges . map (\c -> (c, c)) . List.nub . List.sort
  where
    minimizeRanges [] = []
    minimizeRanges [r] = [r]
    minimizeRanges (r1 : r2 : rs) =
      let (c1, c2) = r1 in
      let (c3, c4) = r2 in
        if c2 == maxBound then
          [(c1, c2)]
        else if succ c2 == c3 then
          minimizeRanges ((c1, c4) : rs)
        else
          r1 : minimizeRanges (r2 : rs)

noneOf :: [Char] -> Regexp
noneOf = diff any . oneOf

not :: Regexp -> Regexp
not = runDFAToRegexp . convertDFAToRegexp . complement . minimize .
  runNFAToDFA . convertNFAToDFA . runRegexpToNFA . convertRegexpToNFA

(&&&) :: Regexp -> Regexp -> Regexp
r1 &&& r2 = not (not r1 :|: not r2)

diff :: Regexp -> Regexp -> Regexp
diff r1 r2 = not (not r1 :|: r2)

xor :: Regexp -> Regexp -> Regexp
xor r1 r2 = (r1 :|: r2) `diff` (r1 &&& r2)

groupByGeneralCategory :: Range -> [(Char.GeneralCategory, Range)]
groupByGeneralCategory =
  map (\(gcat, cs) -> (gcat, (head cs, last cs))) .
    map (\tmp -> (fst (head tmp), map snd tmp)) .
      List.groupBy (\(gcat, _) (gcat', _) -> gcat == gcat') .
        map (\c -> (Char.generalCategory c, c)) .
          uncurry enumFromTo

generalCategoryInfo :: RBMap.RBMap Char.GeneralCategory [Range]
generalCategoryInfo =
  RBMap.fromList $
    map (\tmp -> (fst (head tmp), map snd tmp)) $
      List.groupBy (\(gcat, _) (gcat', _) -> gcat == gcat') $
        List.sortBy (\(gcat, _) (gcat', _) -> compare gcat gcat') $
          groupByGeneralCategory (minBound, maxBound)

runRegexpToNFA :: RegexpToNFA a -> a
runRegexpToNFA = flip State.evalState $ RegexpToNFAState { nfaFreshState = 1 }

generateFreshNFAState :: RegexpToNFA NFAState
generateFreshNFAState = do
  nfaFreshState' <- State.gets nfaFreshState
  State.modify $ \s -> s { nfaFreshState = nfaFreshState s + 1 }
  return nfaFreshState'

convertRegexpToNFA :: Regexp -> RegexpToNFA NFA
convertRegexpToNFA Empty = do
  nfaInitialState' <- generateFreshNFAState
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = RBSet.empty
    , nfaTransition = RBMap.empty }
convertRegexpToNFA Epsilon = do
  nfaInitialState' <- generateFreshNFAState
  let nfaFinalStates' = RBSet.singleton nfaInitialState'
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = nfaFinalStates'
    , nfaTransition = RBMap.empty }
convertRegexpToNFA (Ranges ranges) = do
  nfaInitialState' <- generateFreshNFAState
  nfaFinalStates' <- fmap RBSet.singleton generateFreshNFAState
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = nfaFinalStates'
    , nfaTransition =
        RBMap.singleton (nfaInitialState', NFARanges ranges) nfaFinalStates' }
convertRegexpToNFA (regexp1 ::: regexp2) = do
  nfa1 <- convertRegexpToNFA regexp1
  nfa2 <- convertRegexpToNFA regexp2
  return $ NFA
    { nfaInitialState = nfaInitialState nfa1
    , nfaFinalStates = nfaFinalStates nfa2
    , nfaTransition =
        RBMap.unionsBy RBSet.union
          [ nfaTransition nfa1
          , nfaTransition nfa2
          , RBMap.fromList $ map
              (\nfaMiddleState ->
                ( (nfaMiddleState, NFAEpsilon)
                , RBSet.singleton $ nfaInitialState nfa2 ))
              (RBSet.toList (nfaFinalStates nfa1)) ] }
convertRegexpToNFA (regexp1 :|: regexp2) = do
  nfaInitialState' <- generateFreshNFAState
  nfa1 <- convertRegexpToNFA regexp1
  nfa2 <- convertRegexpToNFA regexp2
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = RBSet.union (nfaFinalStates nfa1) (nfaFinalStates nfa2)
    , nfaTransition =
        RBMap.unionsBy RBSet.union
          [ nfaTransition nfa1
          , nfaTransition nfa2
          , RBMap.singleton
              (nfaInitialState', NFAEpsilon)
              (RBSet.fromList [nfaInitialState nfa1, nfaInitialState nfa2]) ] }
convertRegexpToNFA (Many regexp) = do
  nfa <- convertRegexpToNFA regexp
  return $ NFA
    { nfaInitialState = nfaInitialState nfa
    , nfaFinalStates = RBSet.singleton $ nfaInitialState nfa -- $ nfaFinalStates nfa
    , nfaTransition =
        RBMap.unionBy RBSet.union (nfaTransition nfa) $ RBMap.fromList $ map
          (\nfaLoopState ->
            ( (nfaLoopState, NFAEpsilon)
            , RBSet.singleton $ nfaInitialState nfa ))
          (RBSet.toList (nfaFinalStates nfa)) }

runNFAToDFA :: NFAToDFA a -> a
runNFAToDFA =
  flip State.evalState $ NFAToDFAState
    { dfaStates = RBMap.empty
    , dfaFreshState = 1
    , dfaCurrentState = 0
    , dfaTrans = RBMap.empty }

convertNFAToDFA :: NFA -> NFAToDFA DFA
convertNFAToDFA nfa = do
  isEmpty <- State.gets (RBMap.null . dfaStates)
  Monad.when isEmpty $
    State.modify $ \s ->
      s { dfaStates =
            RBMap.fromList
              [ (0, RBSet.empty)
              , (1, closure (RBSet.singleton (nfaInitialState nfa))) ] }
  makeDFA
  states <- State.gets dfaStates
  dfaTransition' <- State.gets dfaTrans
  let dfaFinalStates'' =
        RBSet.fromList $ RBMap.keys $ RBMap.filter
          (RBSet.any (flip RBSet.member (nfaFinalStates nfa)))
          states
  return $ DFA
    { dfaInitialState = 1
    , dfaFinalStates = dfaFinalStates''
    , dfaTransition = dfaTransition' }
  where
    closure states =
      let nfaTransition' =
            flip RBMap.filteri (nfaTransition nfa) $ \(s, c) _ ->
              RBSet.member s states && c == NFAEpsilon in
      let states' = RBSet.unions $ states : RBMap.elts nfaTransition' in
        if states == states' then
          states
        else
          closure states'

    dfaLabel NFAEpsilon =
      Nothing
    dfaLabel (NFARanges ranges) =
      Just $ DFARanges ranges

    splitRange range1 range2 =
      let (c1, c2) = range1 in
      let (c3, c4) = range2 in
        if range1 == range2 then
          []
        else if c1 > c4 || c2 < c3 then
          [range2]
        else if subsetRange range1 range2 then
          if c1 == minBound && c2 == maxBound then
            []
          else if c1 == minBound then
            [(succ c2, c4)]
          else if c2 == maxBound then
            [(c3, pred c1)]
          else
            [(c3, pred c1), (succ c2, c4)]
        else if subsetRange range2 range1 then
          []
        else
          if c1 > c3 then
            [(c3, pred c1)]
          else if c2 < c4 then
            [(succ c2, c4)]
          else
            []

    splitRanges = concatMap . splitRange

    splitRanges' ranges =
      map (\r -> foldr splitRanges r $ concat $ filter (\r'' -> Prelude.not (all (\r' -> Prelude.any (subsetRange r') r'') r)) ranges) ranges

    dfaRanges =
      RBSet.toList $ RBSet.fromList $ map (filter (Prelude.not . nullRange)) $ splitRanges' $ [(minBound, maxBound)] : (Maybe.mapMaybe (\(_, c) ->
        case c of
          NFAEpsilon -> Nothing
          NFARanges ranges -> Just ranges
          ) $
        RBMap.keys $ nfaTransition nfa)

    dfaLabels =
      map DFARanges dfaRanges

    subset (DFARanges ranges1) (DFARanges ranges2) =
      all (\r -> Prelude.any (subsetRange r) ranges2) ranges1

    makeDFA = do
      continue <- State.gets (\(NFAToDFAState _ p j _) -> j <= p)
      if Prelude.not continue then
        return ()
      else do
        Monad.forM_ dfaLabels $ \c -> do
          e <-
            State.gets $ \(NFAToDFAState states _ j _) ->
              case RBMap.lookup j states of
                Nothing ->
                  undefined
                Just states' ->
                  closure $ RBSet.unions $ RBMap.elts $
                    flip RBMap.filteri (nfaTransition nfa) $ \(s, c') _ ->
                      case dfaLabel c' of
                        Nothing ->
                          False
                        Just c'' ->
                          RBSet.member s states' && subset c c''
          maybeI <-
            State.gets $ \(NFAToDFAState states _ _ _) ->
              fmap fst $ List.find (\(_, states') -> states' == e) $
                RBMap.toList states
          case maybeI of
            Nothing ->
              State.modify $ \(NFAToDFAState states p j trans) ->
                let p' = p + 1 in
                let states' = RBMap.insert p' e states in
                let trans' = RBMap.insert (j, c) p' trans in
                  (NFAToDFAState states' p' j trans')
            Just i ->
              State.modify $ \(NFAToDFAState states p j trans) ->
                let trans' = RBMap.insert (j, c) i trans in
                  (NFAToDFAState states p j trans')
        State.modify $ \(NFAToDFAState states p j trans) ->
          let j' = j + 1 in
            (NFAToDFAState states p j' trans)
        makeDFA

convertRegexpToDFA :: Regexp -> DFA
convertRegexpToDFA = runNFAToDFA . convertNFAToDFA . runRegexpToNFA . convertRegexpToNFA

fillTable :: DFA -> RBSet.RBSet (DFAState, DFAState)
fillTable dfa = fillTable' RBSet.empty initialTable where
  states =
    RBSet.insert (dfaInitialState dfa) $
      RBSet.union (dfaFinalStates dfa) $
      RBSet.fromList $ concatMap (\((s, _), s') -> [s, s']) $
        RBMap.toList $ dfaTransition dfa

  initialTable =
    RBSet.fromList
      (concatMap
        (\p ->
          concatMap (\q -> [(min p q, max p q)]) (RBSet.toList (dfaFinalStates dfa)))
        (RBSet.toList (RBSet.diff states (dfaFinalStates dfa))))

  m = RBMap.fromList $
    map (\l -> (fst (head l), RBSet.toList $ RBSet.fromList $ map snd l)) $
    List.groupBy (\lhs rhs -> fst lhs == fst rhs) $
    List.sortBy (\lhs rhs -> compare (fst lhs) (fst rhs)) $
    map (\((p, c), q) -> (q, (p, c))) $ RBMap.toList $ dfaTransition dfa

  fillTable' table table' =
    let table'' =
          RBSet.fromList $
            concatMap
              (\(p, q) ->
                case RBMap.lookup p m of
                  Nothing ->
                    []
                  Just l ->
                    case RBMap.lookup q m of
                      Nothing ->
                        []
                      Just l' ->
                        concatMap (\(p', c) -> Maybe.mapMaybe (\(q', c') -> if p' /= q' && c == c' then Just (min p' q', max p' q') else Nothing) l) l')
              (RBSet.toList table') in
      if RBSet.union table table' == RBSet.unions [table, table', table''] then
        RBSet.union table table'
      else
        fillTable' (RBSet.union table table') table''

minimize :: DFA -> DFA
minimize dfa = DFA
  { dfaTransition = RBMap.fromList $ map (\((p, c), q) -> ((mapState p, c), mapState q)) $ RBMap.toList $ dfaTransition dfa
  , dfaInitialState = mapState $ dfaInitialState dfa
  , dfaFinalStates = RBSet.fromList $ map mapState $ RBSet.toList $ dfaFinalStates dfa } where
    states =
      RBSet.insert (dfaInitialState dfa) $
        RBSet.union (dfaFinalStates dfa) $
        RBSet.fromList $ concatMap (\((s, _), s') -> [s, s']) $
          RBMap.toList $ dfaTransition dfa

    table = fillTable dfa

    p /== q =
      RBSet.member (min p q, max p q) table

    p === q =
      Prelude.not $ p /== q

    eqStates =
      RBSet.fromList $ map RBSet.fromList $ foldr
        (\q s ->
          let s' =
                map
                  (\qs ->
                    if Maybe.isJust (List.find (\q' -> q === q') qs) then
                      q : qs
                    else
                      qs)
                  s in
            if s == s' then
              [q] : s
            else
              s')
        []
        (RBSet.toList states)

    mapState q =
      case List.find (\(_, qs) -> RBSet.member q qs) $ zip [0..] $ RBSet.toList eqStates of
        Nothing ->
          undefined
        Just (i, _) ->
          i

runDFAToRegexp :: DFAToRegexp a -> a
runDFAToRegexp = flip State.evalState (0, 0, RBMap.empty, RBSet.empty)

convertDFAToRegexp :: DFA -> DFAToRegexp Regexp
convertDFAToRegexp dfa = do
  State.put (initialState, finalState, mapping, RBSet.insert initialState $ RBSet.insert finalState $ states)
  Monad.forM_ (RBSet.toList states) $ \s -> do
    State.modify $ \(q, q', m, states') -> (q, q', m, RBSet.delete s states')
    states' <- State.gets $ \(_, _, _, states') -> states'
    Monad.forM_ (RBSet.toList states') $ \q ->
      Monad.forM_ (RBSet.toList (RBSet.delete q states')) $ \q' -> do
        regexp1 <-
          State.gets $ \(_, _, m, _) ->
            case RBMap.lookup (q, s) m of
              Nothing -> Empty
              Just r -> r
        regexp2 <-
          State.gets $ \(_, _, m, _) ->
            case RBMap.lookup (s, s) m of
              Nothing -> Empty
              Just r -> r
        regexp3 <-
          State.gets $ \(_, _, m, _) ->
            case RBMap.lookup (s, q') m of
              Nothing -> Empty
              Just r -> r
        regexp4 <-
          State.gets $ \(_, _, m, _) ->
            case RBMap.lookup (q, q') m of
              Nothing -> Empty
              Just r -> r
        State.modify $ \(s1, s2, m, states'') ->
          (s1, s2, RBMap.insert (q, q') (minimizeRegexp (regexp1 ::: Many regexp2 ::: regexp3 :|: regexp4)) m, states'')
  State.gets $ \(_, _, m, _) ->
    case RBMap.lookup (initialState, finalState) m of
      Nothing -> Empty
      Just r -> r
  where
    states =
      RBSet.insert (dfaInitialState dfa) $
        RBSet.union (dfaFinalStates dfa) $
        RBSet.fromList $ concatMap (\((s, _), s') -> [s, s']) $
          RBMap.toList $ dfaTransition dfa

    initialState = maybe 0 id (RBSet.maximum states) + 1
    finalState = maybe 0 id (RBSet.maximum states) + 2

    mapping =
      RBMap.insert (initialState, finalState) Empty $
        RBMap.unionsBy (:|:)
          [ RBMap.fromList
              (map
                (\s ->
                  if s == dfaInitialState dfa then
                    ((initialState, s), Epsilon)
                  else
                    ((initialState, s), Empty))
                (RBSet.toList states))
          , RBMap.fromList
              (map
                (\s ->
                  if RBSet.member s (dfaFinalStates dfa) then
                    ((s, finalState), Epsilon)
                  else
                    ((s, finalState), Empty))
                (RBSet.toList states))
          , RBMap.fromList
              (concatMap
                (\s ->
                  map
                    (\s' ->
                      case List.filter (\((s1, _), s2) -> s1 == s && s2 == s') (RBMap.toList (dfaTransition dfa)) of
                        [] ->
                          ((s, s'), Empty)
                        trans ->
                          let c' =
                                foldr
                                  (\((_, c), _) r ->
                                    case c of
                                      DFARanges ranges ->
                                        Ranges ranges :|: r)
                                  Empty
                                  trans in
                            ((s, s'), c'))
                    (RBSet.toList states))
                (RBSet.toList states)) ]

    minimizeRegexp :: Regexp -> Regexp
    minimizeRegexp Empty = Empty
    minimizeRegexp Epsilon = Epsilon
    minimizeRegexp (Ranges ranges) = Ranges ranges
    minimizeRegexp (r1 ::: r2) =
      case minimizeRegexp r1 of
        Empty ->
          Empty
        Epsilon ->
          minimizeRegexp r2
        r1' ->
          case minimizeRegexp r2 of
            Empty ->
              Empty
            Epsilon ->
              r1'
            r2' ->
              r1' ::: r2'
    minimizeRegexp (r1 :|: r2) =
      case minimizeRegexp r1 of
        Empty ->
          minimizeRegexp r2
        r1' ->
          case minimizeRegexp r2 of
            Empty ->
              r1'
            r2' ->
              case (r1', r2') of
                (r3 ::: Many r4, Epsilon) ->
                  if r3 == r4 then
                    Many r4
                  else
                    r1' :|: r2'
                (Ranges ranges1, Ranges ranges2) ->
                  Ranges $ minimizeRanges $ RBSet.toList $ RBSet.fromList $ ranges1 ++ ranges2
                _ ->
                  if r1' == r2' then
                    r1'
                  else
                    r1' :|: r2'
    minimizeRegexp (Many r) =
      case minimizeRegexp r of
        Empty -> Epsilon
        r' -> Many r'

    minimizeRanges [] = []
    minimizeRanges [r] = [r]
    minimizeRanges (r1 : r2 : rs) =
      let (c1, c2) = r1 in
      let (c3, c4) = r2 in
        if c2 == maxBound then
          [(c1, c2)]
        else if succ c2 == c3 then
          minimizeRanges ((c1, c4) : rs)
        else
          r1 : minimizeRanges (r2 : rs)

complement :: DFA -> DFA
complement dfa = dfa { dfaFinalStates = RBSet.diff states $ dfaFinalStates dfa }
  where
    states =
      RBSet.insert (dfaInitialState dfa) $
        RBSet.union (dfaFinalStates dfa) $
        RBSet.fromList $ concatMap (\((s, _), s') -> [s, s']) $
          RBMap.toList $ dfaTransition dfa

(=~) :: DFA -> String -> Bool
(=~) dfa = match $ dfaInitialState dfa
  where
    sigma = List.nub $ map (snd . fst) $ RBMap.toList $ dfaTransition dfa

    match :: DFAState -> String -> Bool
    match q "" =
      RBSet.member q $ dfaFinalStates dfa
    match q (c : s) =
      case List.find (\(DFARanges ranges) -> Prelude.any (\(c1, c2) -> c1 <= c && c <= c2) ranges) sigma of
        Nothing ->
          False
        Just lab ->
          case RBMap.lookup (q, lab) $ dfaTransition dfa of
            Nothing ->
              False
            Just q' ->
              match q' s
