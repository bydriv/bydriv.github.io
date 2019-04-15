#!/usr/bin/runhaskell -isrc

import qualified Data.Anne  as Anne
import qualified Data.List  as List
import qualified Data.Maybe as Maybe

escape :: String -> String
escape [] = []
escape ('"':s) = '\\' : '"' : escape s
escape ('\\':s) = '\\' : '\\' : escape s
escape ('/':s) = '\\' : '/' : escape s
escape ('\b':s) = '\\' : 'b' : escape s
escape ('\f':s) = '\\' : 'f' : escape s
escape ('\n':s) = '\\' : 'n' : escape s
escape ('\r':s) = '\\' : 'r' : escape s
escape ('\t':s) = '\\' : 't' : escape s
escape (c:s) = c : escape s

list :: [String] -> String
list ss = concat (["["] ++ List.intersperse "," ss ++ ["]"])

right :: Either a b -> Maybe b
right (Right x) = Just x
right _ = Nothing

toJSON :: Anne.Anne -> String
toJSON (Anne.Anne dss) = list (map dataToJSON (Maybe.mapMaybe right dss))

dataToJSON :: Anne.Data -> String
dataToJSON (Anne.Data ds) = list (map datumToJSON ds)

datumToJSON :: Anne.Datum -> String
datumToJSON (Anne.AtomDatum a) = atomToJSON a
datumToJSON (Anne.ListDatum l) = listToJSON l

listToJSON :: Anne.List -> String
listToJSON (Anne.List _ ds) = dataToJSON ds

atomToJSON :: Anne.Atom -> String
atomToJSON (Anne.Text _ s) = "\"" ++ escape s ++ "\""
atomToJSON (Anne.Raw1 _ _ s) =  "\"" ++ escape s ++ "\""
atomToJSON (Anne.RawN _ _ s) =  "\"" ++ escape s ++ "\""

main :: IO ()
main = do
  s <- getContents

  case Anne.parse s of
    Right a ->
      putStrLn (toJSON a)
    Left e ->
      print e
