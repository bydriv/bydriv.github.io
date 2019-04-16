module Data.Anne.Defn where

type Cursor = Int
type Pos = (Cursor, Cursor)

data Anne =
    Anne [Either Blank Data]
  deriving (Eq, Ord, Read, Show)

data Blank =
    Blank Pos String
  deriving (Eq, Ord, Read, Show)

data Data =
    Data [Datum]
  deriving (Eq, Ord, Read, Show)

data Datum =
    AtomDatum Atom
  | ListDatum List
  deriving (Eq, Ord, Read, Show)

data Atom =
    Text Pos String
  | Raw1 Pos Char String
  | RawN Pos String String
  deriving (Eq, Ord, Read, Show)

data List =
    List Pos Data
  deriving (Eq, Ord, Read, Show)
