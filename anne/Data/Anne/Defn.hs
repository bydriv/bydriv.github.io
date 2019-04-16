module Data.Anne.Defn where

type Cursor = Int
type Pos = (Cursor, Cursor)

data Anne =
    Anne Document
  deriving (Eq, Ord, Read, Show)

data Document =
    Document [Either Blank Paragraph]
  deriving (Eq, Ord, Read, Show)

data Blank =
    Blank Pos String
  deriving (Eq, Ord, Read, Show)

data Paragraph =
    Paragraph Data
  deriving (Eq, Ord, Read, Show)

data Data =
    Data [Datum]
  deriving (Eq, Ord, Read, Show)

data Datum =
    AtomDatum Atom
  | ListDatum List
  | WrapperDatum Wrapper
  deriving (Eq, Ord, Read, Show)

data Atom =
    Text Pos String
  | Raw1 Pos Char String
  | RawN Pos String String
  deriving (Eq, Ord, Read, Show)

data List =
    List Pos Data
  deriving (Eq, Ord, Read, Show)

data Wrapper =
    Wrapper Pos Document
  deriving (Eq, Ord, Read, Show)
