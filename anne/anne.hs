import qualified Data.Anne   as Anne
import qualified Data.List   as List
import qualified System.Exit as Exit
import qualified System.IO   as IO

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
anneToJSON (Anne.Anne d) = documentToJSON d

documentToJSON :: Anne.Document -> [String]
documentToJSON (Anne.Document d) = concat (concat [[["{\"type\":\"document\",\"value\":["]], List.intersperse [","] (map f d), [["]}"]]])
  where
    f :: Either Anne.Blank Anne.Paragraph -> [String]
    f (Left (Anne.Blank _ s)) = ["{\"type\":\"blank\",\"value\":\"", escape s, "\"}"]
    f (Right (Anne.Paragraph ds)) = concat (concat [[["{\"type\":\"paragraph\",\"value\":["]], List.intersperse [","] (map datumToJSON ds), [["]}"]]])

dataToJSON :: Anne.Data -> [String]
dataToJSON (Anne.Data ds) = concat (concat [[["{\"type\":\"data\",\"value\":["]], List.intersperse [","] (map datumToJSON ds), [["]}"]]])

datumToJSON :: Anne.Datum -> [String]
datumToJSON (Anne.AtomDatum a) = atomToJSON a
datumToJSON (Anne.ListDatum l) = listToJSON l

atomToJSON :: Anne.Atom -> [String]
atomToJSON (Anne.Text _ s) = ["{\"type\":\"text\",\"value\":\"", escape s, "\"}"]
atomToJSON (Anne.Raw1 _ k s) = ["{\"type\":\"raw1\",\"delimiter\":\"", escape [k], "\",\"value\":\"", escape s, "\"}"]
atomToJSON (Anne.RawN _ k s) = ["{\"type\":\"rawn\",\"delimiter\":\"", escape k, "\",\"value\":\"", escape s, "\"}"]

listToJSON :: Anne.List -> [String]
listToJSON (Anne.List _ ds) = concat [["{\"type\":\"list\",\"value\":"], dataToJSON ds, ["}"]]

beginningOfLine :: Anne.Cursor -> String -> Int
beginningOfLine p s =
  let s1 = take p s in
  let s2 = takeWhile (\c -> c /= '\n' && c /= '\r') (reverse s1) in
    p - length s2

endOfLine :: Anne.Cursor -> String -> Int
endOfLine p s =
  let s1 = drop p s in
  let s2 = takeWhile (\c -> c /= '\n' && c /= '\r') s1 in
  case drop (length s2) s1 of
    '\r':'\n':_ -> p + length s2 + 2
    '\n':_ -> p + length s2 + 1
    '\r':_ -> p + length s2 + 1
    _ -> p + length s2

lineOf :: Anne.Cursor -> String -> Int
lineOf p =
  let f i s =
        let n = endOfLine 0 s in
          if i < p + n then
              f (i + n) (drop n s) + 1
          else
            0
  in f 0

colOf :: Anne.Cursor -> String -> Int
colOf p s =
  let bol = beginningOfLine p s in
    (p - bol) + 1

substr :: Anne.Pos -> String -> String
substr (p, q) = take (q - p) . drop p

highlight :: Anne.Pos -> String -> String
highlight (p, q) s =
  let (s1, s') = splitAt p s in
    let (s2, s3) = splitAt (q - p) s' in
    s1 ++ "\027[4m" ++ s2 ++ "\027[0m" ++ s3

main :: IO ()
main = do
  s <- getContents

  case Anne.parse s of
    Right a ->
      putStrLn (toJSON a)
    Left e ->
      let report pos' msg =
              let bol = beginningOfLine (fst pos') s in
              let eol = endOfLine (snd pos') s in
              let s'' = substr (bol, eol) s in
              let lineno1 = lineOf (fst pos') s in
              let colno1 = colOf (fst pos') s in
              let lineno2 = lineOf (snd pos') s in
              let colno2 = colOf (snd pos') s in
              let loc =
                    if lineno1 == lineno2 then
                      if colno1 == colno2 then
                        show lineno1 ++ ":" ++ show colno1
                      else
                        show lineno1 ++ ":" ++ show colno1 ++ "-" ++ show colno2
                    else
                      show lineno1 ++ ":" ++ show colno1 ++ "-" ++ show lineno2 ++ ":" ++ show colno2 in do
                IO.hPutStrLn IO.stderr $ "\027[1m" ++ loc ++ ":\027[0m \027[1;31merror:\027[0m \027[1m" ++ msg ++ "\027[0m"
                IO.hPutStr IO.stderr $ highlight (fst pos' - bol, snd pos' - bol) s''
                Exit.exitFailure
      in do
        case e of
          Anne.NonterminatingRawText (p, q) ->
            report (p, q) "nonterminating raw text"
          Anne.UnrecognizedCharacter (p, q) c ->
            report (p, q) ("unrecognized character: " ++ [c])
          Anne.UnexpectedEOF (p, q) ->
            report (p, q) "unexpected EOF"
          Anne.UnexpectedToken (p, q) t ->
            report (p, q) ("unexpected token: " ++ show t)
