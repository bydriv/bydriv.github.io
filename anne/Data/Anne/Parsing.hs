module  Data.Anne.Parsing  where
import qualified Control.Monad as Monad


import qualified Data.Anne.Defn as Defn

type Anne = Defn.Anne
type Document = Defn.Document
type Paragraph = Defn.Paragraph
type Data = Defn.Data
type Datum = Defn.Datum
type Atom = Defn.Atom
type List = Defn.List

type LBRACKET = (Defn.Pos, String)
type RBRACKET = (Defn.Pos, String)
type BLANK = (Defn.Pos, String)
type TEXT = (Defn.Pos, String)
type RAW1 = (Defn.Pos, Char, String)
type RAWN = (Defn.Pos, String, String)

data Token =
    BLANK BLANK
  | LBRACKET LBRACKET
  | RAW1 RAW1
  | RAWN RAWN
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
  | StackValue_anne Anne
  | StackValue_document Document
  | StackValue_paragraph Paragraph
  | StackValue_datum Datum
  | StackValue_data Data
  | StackValue_atom Atom
  | StackValue_list List

data SemanticActions m = SemanticActions
  { anne_implies :: m Anne
  , anne_implies_document :: Document -> m Anne
  , document_implies_paragraph :: Paragraph -> m Document
  , document_implies_paragraph_BLANK_document :: Paragraph -> BLANK -> Document -> m Document
  , paragraph_implies_datum :: Datum -> m Paragraph
  , paragraph_implies_datum_paragraph :: Datum -> Paragraph -> m Paragraph
  , data_implies :: m Data
  , data_implies_datum_data :: Datum -> Data -> m Data
  , datum_implies_atom :: Atom -> m Datum
  , datum_implies_list :: List -> m Datum
  , atom_implies_TEXT :: TEXT -> m Atom
  , atom_implies_RAW1 :: RAW1 -> m Atom
  , atom_implies_RAWN :: RAWN -> m Atom
  , list_implies_LBRACKET_data_RBRACKET :: LBRACKET -> Data -> RBRACKET -> m List }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  let s' :: Int
      s' =
        case s of
          EOF -> -1
          Token (BLANK _) -> 0
          Token (LBRACKET _) -> 4
          Token (RAW1 _) -> 2
          Token (RAWN _) -> 3
          Token (RBRACKET _) -> 5
          Token (TEXT _) -> 1
  in case compare(q,s')(14,5)of{LT->case compare(q,s')(11,4)of{LT->case compare(q,s')(10,3)of{LT->case compare(q,s')(8,1)of{LT->case compare(q,s')(6,0)of{LT->case compare(q,s')(3,2)of{LT->case compare(q,s')(1,-1)of{LT->case compare(q,s')(0,3)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,-1)of{LT->Nothing;EQ->Just(Reduce 0 0);GT->Nothing};EQ->Just(Shift 13);GT->Nothing};EQ->Just(Shift 14);GT->Nothing};EQ->Just(Shift 15);GT->case compare(q,s')(0,4)of{LT->Nothing;EQ->Just(Shift 9);GT->Nothing}};EQ->Just(Accept);GT->case compare(q,s')(3,1)of{LT->case compare(q,s')(2,-1)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Shift 13);GT->Nothing}};EQ->Just(Shift 14);GT->case compare(q,s')(5,-1)of{LT->case compare(q,s')(3,4)of{LT->case compare(q,s')(3,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing};EQ->Just(Shift 9);GT->case compare(q,s')(4,-1)of{LT->Nothing;EQ->Just(Reduce 3 3);GT->Nothing}};EQ->Just(Reduce 1 2);GT->case compare(q,s')(5,0)of{LT->Nothing;EQ->Just(Shift 3);GT->case compare(q,s')(6,-1)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing}}}};EQ->Just(Reduce 1 4);GT->case compare(q,s')(6,3)of{LT->case compare(q,s')(6,2)of{LT->case compare(q,s')(6,1)of{LT->Nothing;EQ->Just(Shift 13);GT->Nothing};EQ->Just(Shift 14);GT->Nothing};EQ->Just(Shift 15);GT->case compare(q,s')(7,-1)of{LT->case compare(q,s')(6,4)of{LT->Nothing;EQ->Just(Shift 9);GT->Nothing};EQ->Just(Reduce 2 5);GT->case compare(q,s')(7,0)of{LT->Nothing;EQ->Just(Reduce 2 5);GT->Nothing}}}};EQ->Just(Shift 13);GT->case compare(q,s')(9,1)of{LT->case compare(q,s')(8,4)of{LT->case compare(q,s')(8,2)of{LT->Nothing;EQ->Just(Shift 14);GT->case compare(q,s')(8,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}};EQ->Just(Shift 9);GT->case compare(q,s')(8,5)of{LT->Nothing;EQ->Just(Reduce 0 6);GT->Nothing}};EQ->Just(Shift 13);GT->case compare(q,s')(10,1)of{LT->case compare(q,s')(10,-1)of{LT->case compare(q,s')(9,4)of{LT->case compare(q,s')(9,3)of{LT->case compare(q,s')(9,2)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing};EQ->Just(Shift 15);GT->Nothing};EQ->Just(Shift 9);GT->case compare(q,s')(9,5)of{LT->Nothing;EQ->Just(Reduce 0 6);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(10,0)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(10,2)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing}}}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(11,0)of{LT->case compare(q,s')(10,5)of{LT->case compare(q,s')(10,4)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(11,-1)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(11,2)of{LT->case compare(q,s')(11,1)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(11,3)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(13,4)of{LT->case compare(q,s')(13,0)of{LT->case compare(q,s')(12,5)of{LT->case compare(q,s')(11,5)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 2 7);GT->case compare(q,s')(13,-1)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(13,2)of{LT->case compare(q,s')(13,1)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(13,3)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(14,1)of{LT->case compare(q,s')(14,-1)of{LT->case compare(q,s')(13,5)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(14,0)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(14,3)of{LT->case compare(q,s')(14,2)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(14,4)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}}}}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(16,-1)of{LT->case compare(q,s')(15,2)of{LT->case compare(q,s')(15,0)of{LT->case compare(q,s')(15,-1)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(15,1)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(15,4)of{LT->case compare(q,s')(15,3)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(15,5)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing}}};EQ->Just(Reduce 3 13);GT->case compare(q,s')(16,3)of{LT->case compare(q,s')(16,1)of{LT->case compare(q,s')(16,0)of{LT->Nothing;EQ->Just(Reduce 3 13);GT->Nothing};EQ->Just(Reduce 3 13);GT->case compare(q,s')(16,2)of{LT->Nothing;EQ->Just(Reduce 3 13);GT->Nothing}};EQ->Just(Reduce 3 13);GT->case compare(q,s')(16,5)of{LT->case compare(q,s')(16,4)of{LT->Nothing;EQ->Just(Reduce 3 13);GT->Nothing};EQ->Just(Reduce 3 13);GT->case compare(q,s')(17,5)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing}}}}}

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 1
production 3 = 1
production 4 = 2
production 5 = 2
production 6 = 4
production 7 = 4
production 8 = 3
production 9 = 3
production 10 = 5
production 11 = 5
production 12 = 5
production 13 = 6
production _ = undefined

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  let s' = production s in
    case compare(q,s')(6,2)of{LT->case compare(q,s')(0,6)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 2;GT->Nothing};EQ->Just 5;GT->case compare(q,s')(0,5)of{LT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just 6;GT->Nothing};EQ->Just 10;GT->Nothing}};EQ->Just 11;GT->case compare(q,s')(3,3)of{LT->case compare(q,s')(3,2)of{LT->case compare(q,s')(3,1)of{LT->Nothing;EQ->Just 4;GT->Nothing};EQ->Just 5;GT->Nothing};EQ->Just 6;GT->case compare(q,s')(3,6)of{LT->case compare(q,s')(3,5)of{LT->Nothing;EQ->Just 10;GT->Nothing};EQ->Just 11;GT->Nothing}}};EQ->Just 7;GT->case compare(q,s')(8,5)of{LT->case compare(q,s')(6,6)of{LT->case compare(q,s')(6,5)of{LT->case compare(q,s')(6,3)of{LT->Nothing;EQ->Just 6;GT->Nothing};EQ->Just 10;GT->Nothing};EQ->Just 11;GT->case compare(q,s')(8,4)of{LT->case compare(q,s')(8,3)of{LT->Nothing;EQ->Just 8;GT->Nothing};EQ->Just 12;GT->Nothing}};EQ->Just 10;GT->case compare(q,s')(9,4)of{LT->case compare(q,s')(9,3)of{LT->case compare(q,s')(8,6)of{LT->Nothing;EQ->Just 11;GT->Nothing};EQ->Just 8;GT->Nothing};EQ->Just 17;GT->case compare(q,s')(9,6)of{LT->case compare(q,s')(9,5)of{LT->Nothing;EQ->Just 10;GT->Nothing};EQ->Just 11;GT->Nothing}}}}

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
                      Monad.liftM StackValue_anne $ anne_implies actions
                    1 ->
                      Monad.liftM StackValue_anne $ anne_implies_document actions (case snd (pop !! 0) of { StackValue_document value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_document $ document_implies_paragraph actions (case snd (pop !! 0) of { StackValue_paragraph value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_document $ document_implies_paragraph_BLANK_document actions (case snd (pop !! 2) of { StackValue_paragraph value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BLANK value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_document value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_paragraph $ paragraph_implies_datum actions (case snd (pop !! 0) of { StackValue_datum value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_paragraph $ paragraph_implies_datum_paragraph actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_paragraph value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_data $ data_implies actions
                    7 ->
                      Monad.liftM StackValue_data $ data_implies_datum_data actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_data value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_datum $ datum_implies_atom actions (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_datum $ datum_implies_list actions (case snd (pop !! 0) of { StackValue_list value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_atom $ atom_implies_TEXT actions (case snd (pop !! 0) of { StackValue_TEXT value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_atom $ atom_implies_RAW1 actions (case snd (pop !! 0) of { StackValue_RAW1 value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_atom $ atom_implies_RAWN actions (case snd (pop !! 0) of { StackValue_RAWN value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_list $ list_implies_LBRACKET_data_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_data value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    _ -> undefined
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_anne value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { anne_implies =
      return (Defn.Anne (Defn.Document []))
  , anne_implies_document = \d ->
      return (Defn.Anne d)
  , document_implies_paragraph = \par ->
      return (Defn.Document [Right par])
  , document_implies_paragraph_BLANK_document = \par (p, s) (Defn.Document dss) ->
      return (Defn.Document (Right par:Left (Defn.Blank p s):dss))
  , paragraph_implies_datum = \d ->
      return (Defn.Paragraph [d])
  , paragraph_implies_datum_paragraph = \d (Defn.Paragraph ds) ->
      return (Defn.Paragraph (d:ds))
  , data_implies =
      return (Defn.Data [])
  , data_implies_datum_data = \d (Defn.Data ds) ->
      return (Defn.Data (d:ds))
  , datum_implies_atom = \a ->
      return (Defn.AtomDatum a)
  , datum_implies_list = \l ->
      return (Defn.ListDatum l)
  , atom_implies_TEXT = \(p, s) ->
      return (Defn.Text p s)
  , atom_implies_RAW1 = \(p, k, s) ->
      return (Defn.Raw1 p k s)
  , atom_implies_RAWN = \(p, k, s) ->
      return (Defn.RawN p k s)
  , list_implies_LBRACKET_data_RBRACKET = \((l, _), _) ds ((_, r), _) ->
      return (Defn.List (l, r) ds) }

