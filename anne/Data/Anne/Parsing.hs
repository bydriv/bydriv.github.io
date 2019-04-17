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
type Wrapper = Defn.Wrapper

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
  | StackValue_document Document
  | StackValue_paragraph Paragraph
  | StackValue_datum Datum
  | StackValue_data Data
  | StackValue_atom Atom
  | StackValue_list List
  | StackValue_wrapper Wrapper

data SemanticActions m = SemanticActions
  { anne_implies_document :: Document -> m Anne
  , document_implies :: m Document
  , document_implies_paragraph :: Paragraph -> m Document
  , document_implies_BLANK_document :: BLANK -> Document -> m Document
  , document_implies_paragraph_BLANK_document :: Paragraph -> BLANK -> Document -> m Document
  , paragraph_implies_datum :: Datum -> m Paragraph
  , paragraph_implies_datum_paragraph :: Datum -> Paragraph -> m Paragraph
  , data_implies :: m Data
  , data_implies_datum_data :: Datum -> Data -> m Data
  , datum_implies_atom :: Atom -> m Datum
  , datum_implies_list :: List -> m Datum
  , datum_implies_wrapper :: Wrapper -> m Datum
  , atom_implies_TEXT :: TEXT -> m Atom
  , atom_implies_RAW1 :: RAW1 -> m Atom
  , atom_implies_RAWN :: RAWN -> m Atom
  , list_implies_LBRACKET_data_RBRACKET :: LBRACKET -> Data -> RBRACKET -> m List
  , wrapper_implies_LBRACE_document_RBRACE :: LBRACE -> Document -> RBRACE -> m Wrapper }

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
  in case compare(q,s')(17,7)of{LT->case compare(q,s')(11,3)of{LT->case compare(q,s')(4,7)of{LT->case compare(q,s')(3,4)of{LT->case compare(q,s')(3,0)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,0)of{LT->case compare(q,s')(0,-1)of{LT->Nothing;EQ->Just(Reduce 0 1);GT->Nothing};EQ->Just(Shift 3);GT->case compare(q,s')(0,1)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing}};EQ->Just(Shift 18);GT->case compare(q,s')(0,6)of{LT->case compare(q,s')(0,4)of{LT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing};EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 5);GT->case compare(q,s')(2,-1)of{LT->case compare(q,s')(1,-1)of{LT->Nothing;EQ->Just(Accept);GT->Nothing};EQ->Just(Reduce 1 0);GT->case compare(q,s')(3,-1)of{LT->Nothing;EQ->Just(Reduce 0 1);GT->Nothing}}}};EQ->Just(Shift 3);GT->case compare(q,s')(3,2)of{LT->case compare(q,s')(3,1)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing};EQ->Just(Shift 18);GT->case compare(q,s')(3,3)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing}}};EQ->Just(Shift 12);GT->case compare(q,s')(4,1)of{LT->case compare(q,s')(4,-1)of{LT->case compare(q,s')(3,6)of{LT->Nothing;EQ->Just(Shift 5);GT->case compare(q,s')(3,7)of{LT->Nothing;EQ->Just(Reduce 0 1);GT->Nothing}};EQ->Just(Reduce 0 1);GT->case compare(q,s')(4,0)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing}};EQ->Just(Shift 17);GT->case compare(q,s')(4,4)of{LT->case compare(q,s')(4,3)of{LT->case compare(q,s')(4,2)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Shift 19);GT->Nothing};EQ->Just(Shift 12);GT->case compare(q,s')(4,6)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing}}}};EQ->Just(Reduce 0 1);GT->case compare(q,s')(9,1)of{LT->case compare(q,s')(5,3)of{LT->case compare(q,s')(5,1)of{LT->case compare(q,s')(5,0)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing};EQ->Just(Shift 17);GT->case compare(q,s')(5,2)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing}};EQ->Just(Shift 19);GT->case compare(q,s')(7,7)of{LT->case compare(q,s')(6,7)of{LT->case compare(q,s')(5,7)of{LT->case compare(q,s')(5,6)of{LT->case compare(q,s')(5,4)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 5);GT->Nothing};EQ->Just(Reduce 0 1);GT->case compare(q,s')(6,-1)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(7,-1)of{LT->Nothing;EQ->Just(Reduce 3 4);GT->Nothing}};EQ->Just(Reduce 3 4);GT->case compare(q,s')(8,0)of{LT->case compare(q,s')(8,-1)of{LT->Nothing;EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Shift 4);GT->case compare(q,s')(9,-1)of{LT->case compare(q,s')(8,7)of{LT->Nothing;EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(9,0)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing}}}}};EQ->Just(Shift 17);GT->case compare(q,s')(10,0)of{LT->case compare(q,s')(9,3)of{LT->case compare(q,s')(9,2)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Shift 19);GT->case compare(q,s')(9,7)of{LT->case compare(q,s')(9,6)of{LT->case compare(q,s')(9,4)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(10,-1)of{LT->Nothing;EQ->Just(Reduce 2 6);GT->Nothing}}};EQ->Just(Reduce 2 6);GT->case compare(q,s')(11,1)of{LT->case compare(q,s')(10,7)of{LT->Nothing;EQ->Just(Reduce 2 6);GT->Nothing};EQ->Just(Shift 17);GT->case compare(q,s')(11,2)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing}}}}};EQ->Just(Shift 19);GT->case compare(q,s')(15,1)of{LT->case compare(q,s')(14,2)of{LT->case compare(q,s')(13,3)of{LT->case compare(q,s')(13,-1)of{LT->case compare(q,s')(12,1)of{LT->case compare(q,s')(11,5)of{LT->case compare(q,s')(11,4)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Reduce 0 7);GT->case compare(q,s')(11,6)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing}};EQ->Just(Shift 17);GT->case compare(q,s')(12,3)of{LT->case compare(q,s')(12,2)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Shift 19);GT->case compare(q,s')(12,5)of{LT->case compare(q,s')(12,4)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Reduce 0 7);GT->case compare(q,s')(12,6)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing}}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(13,1)of{LT->case compare(q,s')(13,0)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(13,2)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(13,7)of{LT->case compare(q,s')(13,5)of{LT->case compare(q,s')(13,4)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(13,6)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(14,0)of{LT->case compare(q,s')(14,-1)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(14,1)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}}}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(14,6)of{LT->case compare(q,s')(14,4)of{LT->case compare(q,s')(14,3)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(14,5)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(15,-1)of{LT->case compare(q,s')(14,7)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(15,0)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}}}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(17,-1)of{LT->case compare(q,s')(15,5)of{LT->case compare(q,s')(15,3)of{LT->case compare(q,s')(15,2)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(15,4)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(15,7)of{LT->case compare(q,s')(15,6)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(16,5)of{LT->Nothing;EQ->Just(Reduce 2 8);GT->Nothing}}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(17,3)of{LT->case compare(q,s')(17,1)of{LT->case compare(q,s')(17,0)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(17,2)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(17,5)of{LT->case compare(q,s')(17,4)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(17,6)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing}}}}}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(19,5)of{LT->case compare(q,s')(18,6)of{LT->case compare(q,s')(18,2)of{LT->case compare(q,s')(18,0)of{LT->case compare(q,s')(18,-1)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->case compare(q,s')(18,1)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing}};EQ->Just(Reduce 1 13);GT->case compare(q,s')(18,4)of{LT->case compare(q,s')(18,3)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->case compare(q,s')(18,5)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing}}};EQ->Just(Reduce 1 13);GT->case compare(q,s')(19,1)of{LT->case compare(q,s')(19,-1)of{LT->case compare(q,s')(18,7)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 14);GT->case compare(q,s')(19,0)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(19,3)of{LT->case compare(q,s')(19,2)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->case compare(q,s')(19,4)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing}}}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(20,4)of{LT->case compare(q,s')(20,0)of{LT->case compare(q,s')(19,7)of{LT->case compare(q,s')(19,6)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->case compare(q,s')(20,-1)of{LT->Nothing;EQ->Just(Reduce 3 15);GT->Nothing}};EQ->Just(Reduce 3 15);GT->case compare(q,s')(20,2)of{LT->case compare(q,s')(20,1)of{LT->Nothing;EQ->Just(Reduce 3 15);GT->Nothing};EQ->Just(Reduce 3 15);GT->case compare(q,s')(20,3)of{LT->Nothing;EQ->Just(Reduce 3 15);GT->Nothing}}};EQ->Just(Reduce 3 15);GT->case compare(q,s')(21,5)of{LT->case compare(q,s')(20,6)of{LT->case compare(q,s')(20,5)of{LT->Nothing;EQ->Just(Reduce 3 15);GT->Nothing};EQ->Just(Reduce 3 15);GT->case compare(q,s')(20,7)of{LT->Nothing;EQ->Just(Reduce 3 15);GT->Nothing}};EQ->Just(Shift 20);GT->case compare(q,s')(22,5)of{LT->case compare(q,s')(22,3)of{LT->case compare(q,s')(22,1)of{LT->case compare(q,s')(22,0)of{LT->case compare(q,s')(22,-1)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->case compare(q,s')(22,2)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(22,4)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(22,7)of{LT->case compare(q,s')(22,6)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->case compare(q,s')(23,7)of{LT->Nothing;EQ->Just(Shift 22);GT->Nothing}}}}}}}

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 1
production 3 = 1
production 4 = 1
production 5 = 2
production 6 = 2
production 7 = 4
production 8 = 4
production 9 = 3
production 10 = 3
production 11 = 3
production 12 = 5
production 13 = 5
production 14 = 5
production 15 = 6
production 16 = 7
production _ = undefined

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  let s' = production s in
    case compare(q,s')(5,2)of{LT->case compare(q,s')(3,5)of{LT->case compare(q,s')(0,6)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 2;GT->Nothing};EQ->Just 8;GT->case compare(q,s')(0,5)of{LT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just 9;GT->Nothing};EQ->Just 13;GT->Nothing}};EQ->Just 14;GT->case compare(q,s')(3,2)of{LT->case compare(q,s')(3,1)of{LT->case compare(q,s')(0,7)of{LT->Nothing;EQ->Just 15;GT->Nothing};EQ->Just 6;GT->Nothing};EQ->Just 8;GT->case compare(q,s')(3,3)of{LT->Nothing;EQ->Just 9;GT->Nothing}}};EQ->Just 13;GT->case compare(q,s')(4,3)of{LT->case compare(q,s')(4,1)of{LT->case compare(q,s')(3,7)of{LT->case compare(q,s')(3,6)of{LT->Nothing;EQ->Just 14;GT->Nothing};EQ->Just 15;GT->Nothing};EQ->Just 7;GT->case compare(q,s')(4,2)of{LT->Nothing;EQ->Just 8;GT->Nothing}};EQ->Just 9;GT->case compare(q,s')(4,7)of{LT->case compare(q,s')(4,6)of{LT->case compare(q,s')(4,5)of{LT->Nothing;EQ->Just 13;GT->Nothing};EQ->Just 14;GT->Nothing};EQ->Just 15;GT->case compare(q,s')(5,1)of{LT->Nothing;EQ->Just 23;GT->Nothing}}}};EQ->Just 8;GT->case compare(q,s')(11,3)of{LT->case compare(q,s')(9,2)of{LT->case compare(q,s')(5,6)of{LT->case compare(q,s')(5,5)of{LT->case compare(q,s')(5,3)of{LT->Nothing;EQ->Just 9;GT->Nothing};EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(5,7)of{LT->Nothing;EQ->Just 15;GT->Nothing}};EQ->Just 10;GT->case compare(q,s')(9,6)of{LT->case compare(q,s')(9,5)of{LT->case compare(q,s')(9,3)of{LT->Nothing;EQ->Just 9;GT->Nothing};EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(9,7)of{LT->Nothing;EQ->Just 15;GT->Nothing}}};EQ->Just 11;GT->case compare(q,s')(12,3)of{LT->case compare(q,s')(11,6)of{LT->case compare(q,s')(11,5)of{LT->case compare(q,s')(11,4)of{LT->Nothing;EQ->Just 16;GT->Nothing};EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(11,7)of{LT->Nothing;EQ->Just 15;GT->Nothing}};EQ->Just 11;GT->case compare(q,s')(12,6)of{LT->case compare(q,s')(12,5)of{LT->case compare(q,s')(12,4)of{LT->Nothing;EQ->Just 21;GT->Nothing};EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(12,7)of{LT->Nothing;EQ->Just 15;GT->Nothing}}}}}

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
                      Monad.liftM StackValue_anne $ anne_implies_document actions (case snd (pop !! 0) of { StackValue_document value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_document $ document_implies actions
                    2 ->
                      Monad.liftM StackValue_document $ document_implies_paragraph actions (case snd (pop !! 0) of { StackValue_paragraph value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_document $ document_implies_BLANK_document actions (case snd (pop !! 1) of { StackValue_BLANK value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_document value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_document $ document_implies_paragraph_BLANK_document actions (case snd (pop !! 2) of { StackValue_paragraph value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BLANK value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_document value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_paragraph $ paragraph_implies_datum actions (case snd (pop !! 0) of { StackValue_datum value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_paragraph $ paragraph_implies_datum_paragraph actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_paragraph value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_data $ data_implies actions
                    8 ->
                      Monad.liftM StackValue_data $ data_implies_datum_data actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_data value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_datum $ datum_implies_atom actions (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_datum $ datum_implies_list actions (case snd (pop !! 0) of { StackValue_list value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_datum $ datum_implies_wrapper actions (case snd (pop !! 0) of { StackValue_wrapper value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_atom $ atom_implies_TEXT actions (case snd (pop !! 0) of { StackValue_TEXT value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_atom $ atom_implies_RAW1 actions (case snd (pop !! 0) of { StackValue_RAW1 value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_atom $ atom_implies_RAWN actions (case snd (pop !! 0) of { StackValue_RAWN value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_list $ list_implies_LBRACKET_data_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_data value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_wrapper $ wrapper_implies_LBRACE_document_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_document value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    _ -> undefined
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_anne value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { anne_implies_document = \d ->
      return (Defn.Anne d)
  , document_implies =
      return (Defn.Document [])
  , document_implies_paragraph = \par ->
      return (Defn.Document [Right par])
  , document_implies_BLANK_document = \(p, s) (Defn.Document dss) ->
      return (Defn.Document (Left (Defn.Blank p s):dss))
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
  , datum_implies_wrapper = \w ->
      return (Defn.WrapperDatum w)
  , atom_implies_TEXT = \(p, s) ->
      return (Defn.Text p s)
  , atom_implies_RAW1 = \(p, k, s) ->
      return (Defn.Raw1 p k s)
  , atom_implies_RAWN = \(p, k, s) ->
      return (Defn.RawN p k s)
  , list_implies_LBRACKET_data_RBRACKET = \((l, _), _) ds ((_, r), _) ->
      return (Defn.List (l, r) ds)
  , wrapper_implies_LBRACE_document_RBRACE = \((l, _), _) d ((_, r), _) ->
      return (Defn.Wrapper (l, r) d) }

