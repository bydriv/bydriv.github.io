import qualified Data.Anne  as Anne
import qualified Data.List  as List

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

toJSON :: Anne.Anne -> String
toJSON = concat . anneToJSON

anneToJSON :: Anne.Anne -> [String]
anneToJSON (Anne.Anne anne) = concat (concat [[["["]], map f anne, [["]"]]])
  where
    f :: Either Anne.Blank Anne.Data -> [String]
    f (Left (Anne.Blank _ s)) = [",{\"type\":\"blank\",\"value\":\"", escape s, "\"},"]
    f (Right ds) = dataToJSON ds

dataToJSON :: Anne.Data -> [String]
dataToJSON (Anne.Data ds) = concat (concat [[["["]], List.intersperse [","] (map datumToJSON ds), [["]"]]])

datumToJSON :: Anne.Datum -> [String]
datumToJSON (Anne.AtomDatum a) = atomToJSON a
datumToJSON (Anne.ListDatum l) = listToJSON l

atomToJSON :: Anne.Atom -> [String]
atomToJSON (Anne.Text _ s) = ["{\"type\":\"text\",\"value\":\"", escape s, "\"}"]
atomToJSON (Anne.Raw1 _ k s) = ["{\"type\":\"raw1\",\"delimiter\":\"", escape [k], "\",\"value\":\"", escape s, "\"}"]
atomToJSON (Anne.RawN _ k s) = ["{\"type\":\"rawn\",\"delimiter\":\"", escape k, "\",\"value\":\"", escape s, "\"}"]

listToJSON :: Anne.List -> [String]
listToJSON (Anne.List _ ds) = dataToJSON ds

main :: IO ()
main = do
  s <- getContents

  case Anne.parse s of
    Right a ->
      putStrLn (toJSON a)
    Left e ->
      print e
