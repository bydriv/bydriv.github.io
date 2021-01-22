import Text.Pandoc.JSON

f :: Maybe Format -> Inline -> Inline
f (Just format) x@(Link attr [Str txt] (src,_)) =
  case src of
    ':' : '.' : cls
      | format == Format "html" -> RawInline format $
        "<span class=\"" ++ cls ++ "\">" ++ txt ++ "</span>"
      | otherwise -> Str txt
    ':' : '#' : ident
      | format == Format "html" -> RawInline format $
        "<span id=\"" ++ ident ++ "\">" ++ txt ++ "</span>"
      | otherwise -> Str txt
    '-' : rt
      | format == Format "html" -> RawInline format $
        "<ruby>" ++ txt ++ "<rp>(</rp><rt>" ++ rt ++
        "</rt><rp>)</rp></ruby>"
      | otherwise -> Str txt
    _ -> x
f _ x = x

main :: IO ()
main = toJSONFilter f
