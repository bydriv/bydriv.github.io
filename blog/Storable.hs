import qualified Foiregn -- Foreign.Storable はリエクスポートされている

data Point = Point Int Int

instance Foreign.Storable Point where
  -- 引数は undefined なので使ってはいけない
  sizeOf _ = 8
  alignment _ = 4

  peek p = do
    x <- Foreign.peek (Foreign.castPtr p :: Foreign.Ptr Int)
    y <- Foreign.peek (Foreign.castPtr (Foreign.plusPtr p 4) :: Foreign.Ptr Int)
    return $ Point x y

  poke p (Point x y) = do
    Foreign.poke (Foreign.castPtr p) x
    Foreign.poke (Foreign.castPtr (Foreign.plusPtr p 4)) x
