module  Data.Anne.Parsing  where
import qualified Control.Monad as Monad


import qualified Data.Anne.Defn as Defn

type Anne = Defn.Anne
type Data = Defn.Data
type Datum = Defn.Datum
type Atom = Defn.Atom
type List = Defn.List
type Document = Defn.Document

type LBRACKET = (Defn.Pos, String)
type RBRACKET = (Defn.Pos, String)
type LBRACE = (Defn.Pos, String)
type RBRACE = (Defn.Pos, String)
type BLANK = (Defn.Pos, String)
type TEXT = (Defn.Pos, String)
type RAW1 = (Defn.Pos, Char, String)
type RAWN = (Defn.Pos, String, String)

data Token =
    BLANK BLANK
  | LBRACE LBRACE
  | LBRACKET LBRACKET
  | RAW1 RAW1
  | RAWN RAWN
  | RBRACE RBRACE
  | RBRACKET RBRACKET
  | TEXT TEXT
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
  deriving (Eq, Ord, Read, Show)
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_BLANK BLANK
  | StackValue_TEXT TEXT
  | StackValue_RAW1 RAW1
  | StackValue_RAWN RAWN
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_LBRACE LBRACE
  | StackValue_RBRACE RBRACE
  | StackValue_anne Anne
  | StackValue_data Data
  | StackValue_datum Datum
  | StackValue_atom Atom
  | StackValue_list List
  | StackValue_document Document

data SemanticActions m = SemanticActions
  { anne_implies_data :: Data -> m Anne
  , anne_implies_data_BLANK_anne :: Data -> BLANK -> Anne -> m Anne
  , data_implies :: m Data
  , data_implies_datum_data :: Datum -> Data -> m Data
  , datum_implies_atom :: Atom -> m Datum
  , datum_implies_list :: List -> m Datum
  , datum_implies_document :: Document -> m Datum
  , atom_implies_TEXT :: TEXT -> m Atom
  , atom_implies_RAW1 :: RAW1 -> m Atom
  , atom_implies_RAWN :: RAWN -> m Atom
  , list_implies_LBRACKET_data_RBRACKET :: LBRACKET -> Data -> RBRACKET -> m List
  , document_implies_LBRACE_anne_RBRACE :: LBRACE -> Anne -> RBRACE -> m Document }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  let s' :: Int
      s' =
        case s of
          EOF -> -1
          Token (BLANK _) -> 0
          Token (LBRACE _) -> 6
          Token (LBRACKET _) -> 4
          Token (RAW1 _) -> 2
          Token (RAWN _) -> 3
          Token (RBRACE _) -> 7
          Token (RBRACKET _) -> 5
          Token (TEXT _) -> 1
  in case compare(q,s')(9,2)of{LT->case compare(q,s')(6,3)of{LT->case compare(q,s')(2,4)of{LT->case compare(q,s')(0,4)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,0)of{LT->case compare(q,s')(0,-1)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->Nothing};EQ->Just(Reduce 0 2);GT->case compare(q,s')(0,1)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing}};EQ->Just(Shift 13);GT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}};EQ->Just(Shift 7);GT->case compare(q,s')(1,-1)of{LT->case compare(q,s')(0,6)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing};EQ->Just(Accept);GT->case compare(q,s')(2,-1)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->case compare(q,s')(2,2)of{LT->case compare(q,s')(2,0)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->case compare(q,s')(2,1)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing}};EQ->Just(Shift 13);GT->case compare(q,s')(2,3)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}}}}};EQ->Just(Shift 7);GT->case compare(q,s')(4,-1)of{LT->case compare(q,s')(3,0)of{LT->case compare(q,s')(2,7)of{LT->case compare(q,s')(2,6)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing};EQ->Just(Reduce 0 2);GT->Nothing};EQ->Just(Reduce 0 2);GT->case compare(q,s')(3,6)of{LT->case compare(q,s')(3,3)of{LT->case compare(q,s')(3,2)of{LT->case compare(q,s')(3,1)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 13);GT->Nothing};EQ->Just(Shift 14);GT->case compare(q,s')(3,4)of{LT->Nothing;EQ->Just(Shift 7);GT->Nothing}};EQ->Just(Shift 3);GT->case compare(q,s')(3,7)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->Nothing}}};EQ->Just(Reduce 3 1);GT->case compare(q,s')(6,-1)of{LT->case compare(q,s')(4,7)of{LT->Nothing;EQ->Just(Reduce 3 1);GT->case compare(q,s')(5,7)of{LT->case compare(q,s')(5,-1)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->case compare(q,s')(5,0)of{LT->Nothing;EQ->Just(Shift 2);GT->Nothing}};EQ->Just(Reduce 1 0);GT->Nothing}};EQ->Just(Reduce 0 2);GT->case compare(q,s')(6,1)of{LT->case compare(q,s')(6,0)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->Nothing};EQ->Just(Shift 12);GT->case compare(q,s')(6,2)of{LT->Nothing;EQ->Just(Shift 13);GT->Nothing}}}}};EQ->Just(Shift 14);GT->case compare(q,s')(7,4)of{LT->case compare(q,s')(6,6)of{LT->case compare(q,s')(6,5)of{LT->case compare(q,s')(6,4)of{LT->Nothing;EQ->Just(Shift 7);GT->Nothing};EQ->Just(Reduce 0 2);GT->Nothing};EQ->Just(Shift 3);GT->case compare(q,s')(6,7)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->case compare(q,s')(7,2)of{LT->case compare(q,s')(7,1)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 13);GT->case compare(q,s')(7,3)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}}}};EQ->Just(Shift 7);GT->case compare(q,s')(8,0)of{LT->case compare(q,s')(8,-1)of{LT->case compare(q,s')(7,5)of{LT->Nothing;EQ->Just(Reduce 0 2);GT->case compare(q,s')(7,6)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,-1)of{LT->case compare(q,s')(8,7)of{LT->case compare(q,s')(8,5)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 1 4);GT->case compare(q,s')(9,1)of{LT->case compare(q,s')(9,0)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing}}}}};EQ->Just(Reduce 1 4);GT->case compare(q,s')(14,3)of{LT->case compare(q,s')(11,7)of{LT->case compare(q,s')(10,5)of{LT->case compare(q,s')(10,-1)of{LT->case compare(q,s')(9,5)of{LT->case compare(q,s')(9,4)of{LT->case compare(q,s')(9,3)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->case compare(q,s')(9,7)of{LT->case compare(q,s')(9,6)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(10,2)of{LT->case compare(q,s')(10,1)of{LT->case compare(q,s')(10,0)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(10,4)of{LT->case compare(q,s')(10,3)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing}}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(11,2)of{LT->case compare(q,s')(11,-1)of{LT->case compare(q,s')(10,7)of{LT->case compare(q,s')(10,6)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 6);GT->case compare(q,s')(11,1)of{LT->case compare(q,s')(11,0)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(11,5)of{LT->case compare(q,s')(11,4)of{LT->case compare(q,s')(11,3)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->case compare(q,s')(11,6)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing}}}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(13,1)of{LT->case compare(q,s')(12,4)of{LT->case compare(q,s')(12,1)of{LT->case compare(q,s')(12,0)of{LT->case compare(q,s')(12,-1)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(12,3)of{LT->case compare(q,s')(12,2)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(12,7)of{LT->case compare(q,s')(12,6)of{LT->case compare(q,s')(12,5)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(13,0)of{LT->case compare(q,s')(13,-1)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing}}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(13,7)of{LT->case compare(q,s')(13,4)of{LT->case compare(q,s')(13,3)of{LT->case compare(q,s')(13,2)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(13,6)of{LT->case compare(q,s')(13,5)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(14,1)of{LT->case compare(q,s')(14,0)of{LT->case compare(q,s')(14,-1)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(14,2)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}}}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(17,3)of{LT->case compare(q,s')(15,6)of{LT->case compare(q,s')(15,0)of{LT->case compare(q,s')(14,6)of{LT->case compare(q,s')(14,5)of{LT->case compare(q,s')(14,4)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(15,-1)of{LT->case compare(q,s')(14,7)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 3 10);GT->Nothing}};EQ->Just(Reduce 3 10);GT->case compare(q,s')(15,3)of{LT->case compare(q,s')(15,2)of{LT->case compare(q,s')(15,1)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->case compare(q,s')(15,5)of{LT->case compare(q,s')(15,4)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->Nothing}}};EQ->Just(Reduce 3 10);GT->case compare(q,s')(17,0)of{LT->case compare(q,s')(17,-1)of{LT->case compare(q,s')(15,7)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->case compare(q,s')(16,5)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}};EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 3 11);GT->case compare(q,s')(17,2)of{LT->case compare(q,s')(17,1)of{LT->Nothing;EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 3 11);GT->Nothing}}};EQ->Just(Reduce 3 11);GT->case compare(q,s')(17,7)of{LT->case compare(q,s')(17,6)of{LT->case compare(q,s')(17,5)of{LT->case compare(q,s')(17,4)of{LT->Nothing;EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 3 11);GT->case compare(q,s')(18,7)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing}}}}}

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 1
production 3 = 1
production 4 = 2
production 5 = 2
production 6 = 2
production 7 = 3
production 8 = 3
production 9 = 3
production 10 = 4
production 11 = 5
production _ = undefined

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  let s' = production s in
    case compare(q,s')(3,2)of{LT->case compare(q,s')(2,1)of{LT->case compare(q,s')(0,3)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 5;GT->case compare(q,s')(0,2)of{LT->Nothing;EQ->Just 6;GT->Nothing}};EQ->Just 9;GT->case compare(q,s')(0,5)of{LT->case compare(q,s')(0,4)of{LT->Nothing;EQ->Just 10;GT->Nothing};EQ->Just 11;GT->case compare(q,s')(2,0)of{LT->Nothing;EQ->Just 4;GT->Nothing}}};EQ->Just 5;GT->case compare(q,s')(2,5)of{LT->case compare(q,s')(2,3)of{LT->case compare(q,s')(2,2)of{LT->Nothing;EQ->Just 6;GT->Nothing};EQ->Just 9;GT->case compare(q,s')(2,4)of{LT->Nothing;EQ->Just 10;GT->Nothing}};EQ->Just 11;GT->case compare(q,s')(3,1)of{LT->case compare(q,s')(3,0)of{LT->Nothing;EQ->Just 18;GT->Nothing};EQ->Just 5;GT->Nothing}}};EQ->Just 6;GT->case compare(q,s')(6,4)of{LT->case compare(q,s')(6,1)of{LT->case compare(q,s')(3,4)of{LT->case compare(q,s')(3,3)of{LT->Nothing;EQ->Just 9;GT->Nothing};EQ->Just 10;GT->case compare(q,s')(3,5)of{LT->Nothing;EQ->Just 11;GT->Nothing}};EQ->Just 8;GT->case compare(q,s')(6,3)of{LT->case compare(q,s')(6,2)of{LT->Nothing;EQ->Just 6;GT->Nothing};EQ->Just 9;GT->Nothing}};EQ->Just 10;GT->case compare(q,s')(7,3)of{LT->case compare(q,s')(7,1)of{LT->case compare(q,s')(6,5)of{LT->Nothing;EQ->Just 11;GT->Nothing};EQ->Just 16;GT->case compare(q,s')(7,2)of{LT->Nothing;EQ->Just 6;GT->Nothing}};EQ->Just 9;GT->case compare(q,s')(7,5)of{LT->case compare(q,s')(7,4)of{LT->Nothing;EQ->Just 10;GT->Nothing};EQ->Just 11;GT->Nothing}}}}

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Anne, [Token]))
parse actions = parse' [] where
  parse' stack tokens =
    let p =
          case stack of
            [] -> 0
            ((q, _) : _) -> q in
    let symbol =
          case tokens of
            [] -> EOF
            (token : _) -> Token token in do
      case dfaActionTransition p symbol of
        Nothing ->
          case tokens of
            [] -> return $ Left $ Nothing
            (token : _) -> return $ Left $ Just token
        Just (Shift n) ->
          let value =
                case symbol of
                  EOF ->
                    StackValue_EOF
                  Token (BLANK semanticValue) ->
                    StackValue_BLANK semanticValue
                  Token (TEXT semanticValue) ->
                    StackValue_TEXT semanticValue
                  Token (RAW1 semanticValue) ->
                    StackValue_RAW1 semanticValue
                  Token (RAWN semanticValue) ->
                    StackValue_RAWN semanticValue
                  Token (LBRACKET semanticValue) ->
                    StackValue_LBRACKET semanticValue
                  Token (RBRACKET semanticValue) ->
                    StackValue_RBRACKET semanticValue
                  Token (LBRACE semanticValue) ->
                    StackValue_LBRACE semanticValue
                  Token (RBRACE semanticValue) ->
                    StackValue_RBRACE semanticValue
          in parse' ((n, value) : stack) (tail tokens)
        Just (Reduce n m) ->
          let (pop, stack') = splitAt n stack in
            case
              case stack' of
                [] -> dfaGotoTransition 0 m
                ((q', _) : _) -> dfaGotoTransition q' m of
              Nothing ->
                case tokens of
                  [] -> return $ Left $ Nothing
                  (token : _) -> return $ Left $ Just token
              Just q -> do
                value <-
                  case m of
                    0 ->
                      Monad.liftM StackValue_anne $ anne_implies_data actions (case snd (pop !! 0) of { StackValue_data value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_anne $ anne_implies_data_BLANK_anne actions (case snd (pop !! 2) of { StackValue_data value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BLANK value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_anne value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_data $ data_implies actions
                    3 ->
                      Monad.liftM StackValue_data $ data_implies_datum_data actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_data value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_datum $ datum_implies_atom actions (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_datum $ datum_implies_list actions (case snd (pop !! 0) of { StackValue_list value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_datum $ datum_implies_document actions (case snd (pop !! 0) of { StackValue_document value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_atom $ atom_implies_TEXT actions (case snd (pop !! 0) of { StackValue_TEXT value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_atom $ atom_implies_RAW1 actions (case snd (pop !! 0) of { StackValue_RAW1 value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_atom $ atom_implies_RAWN actions (case snd (pop !! 0) of { StackValue_RAWN value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_list $ list_implies_LBRACKET_data_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_data value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_document $ document_implies_LBRACE_anne_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_anne value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    _ -> undefined
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_anne value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { anne_implies_data = \ds ->
      return (Defn.Anne [Right ds])
  , anne_implies_data_BLANK_anne = \ds (p, s) (Defn.Anne dss) ->
      return (Defn.Anne (Right ds:Left (Defn.Blank p s):dss))
  , data_implies =
      return (Defn.Data [])
  , data_implies_datum_data = \d (Defn.Data ds) ->
      return (Defn.Data (d:ds))
  , datum_implies_atom = \a ->
      return (Defn.AtomDatum a)
  , datum_implies_list = \l ->
      return (Defn.ListDatum l)
  , datum_implies_document = \d ->
      return (Defn.DocumentDatum d)
  , atom_implies_TEXT = \(p, s) ->
      return (Defn.Text p s)
  , atom_implies_RAW1 = \(p, k, s) ->
      return (Defn.Raw1 p k s)
  , atom_implies_RAWN = \(p, k, s) ->
      return (Defn.RawN p k s)
  , list_implies_LBRACKET_data_RBRACKET = \((l, _), _) ds ((_, r), _) ->
      return (Defn.List (l, r) ds)
  , document_implies_LBRACE_anne_RBRACE = \((l, _), _) anne ((_, r), _) ->
      return (Defn.Document (l, r) anne) }

