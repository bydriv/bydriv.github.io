---
url: https://bydriv.github.io/blog/2020-12-12/
title: bydriv.github.io
description: "プログラミング言語に標準で単射がほしいという話"
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# プログラミング言語に標準で単射がほしいという話

プログラミング言語には普通、標準で Map という構造がある (もちろんないものもある) 。
実装は HashMap だったり二分木だったり、まちまち。
ただいずれにせよこれは普通の (部分) 写像で単射が提供されていることはほとんどない。
つまりキーから値をひくことは簡単だけど値からキーをひくことは困難なことが多い。

ナイーブに考えるならば `(Map a b, Map b a)` と実装すればいいもののこの空間効率が悪いことは
火を見るよりも明らかだ。

個人的な事情として最近趣味コードを書く時間があまりとれていないというのもあり、
ちょっと息抜きというか、気分転換に実装してみた。コードは以下。

- [Injection](../../misc/injection)

以下、解説。

まず **単射** (*injection*) を表す型クラスを定義しよう。
求められる性質は左から右も、右から左もひけるということである。

```
class Injection f where
  empty :: (Ord a, Ord b) => f a b
  insert :: (Ord a, Ord b) => a -> b -> f a b -> f a b
  lookupLeft :: (Ord a, Ord b) => a -> f a b -> Maybe b
  lookupRight :: (Ord a, Ord b) => b -> f a b -> Maybe a
```

さて自明な関数として以下のものが定義できる。

```
singleton :: (Injection f, Ord a, Ord b) => a -> b -> f a b
singleton x y = insert x y empty

memberLeft :: (Injection f, Ord a, Ord b) => a -> f a b -> Bool
memberLeft x f = Maybe.isJust (lookupLeft x f)

memberRight :: (Injection f, Ord a, Ord b) => b -> f a b -> Bool
memberRight y f = Maybe.isJust (lookupRight y f)

fromAList :: (Injection f, Ord a, Ord b) => [(a, b)] -> f a b
fromAList = foldr (\(x, y) -> insert x y) empty
```

`fromAList` は連想リストから `Injection` をつくる関数だ。

まず連想リストは自明に `Injection` になる。

```
newtype AList a b =
    AList [(a, b)]
  deriving (Eq, Ord, Read, Show)

instance Injection AList where
  empty = AList []
  insert x y (AList l) = AList ((x, y) : l)
  lookupLeft x (AList l) = fmap snd $ List.find (\(x', _) -> x == x') l
  lookupRight y (AList l) = fmap fst $ List.find (\(_, y') -> y == y') l
```

しかしこれでは `lookupLeft` も `lookupRight` も O(n) なので、なんのおもしろみもない。

四分木はどうだろう。

```
data Quadtree a b =
    Empty
  | Branch (a, b) (Quadtree a b) (Quadtree a b) (Quadtree a b) (Quadtree a b)
  deriving (Eq, Ord, Read, Show)
```

二分木をふたつあわせれば `(Map a b, Map b a)` とできるのだから、
当然これは `Injection` になりそうだ。
ただ四分木にするとすこしおもしろくて、ふたつの instance が考えられる。
仮にそれを `ReadQuadtree` と `WriteQuadtree` としてみよう。

```
newtype ReadQuadtree a b =
    ReadQuadtree (Quadtree a b)
  deriving (Eq, Ord, Read, Show)

newtype WriteQuadtree a b =
    WriteQuadtree (Quadtree a b)
  deriving (Eq, Ord, Read, Show)
```

`ReadQuadtree` は読み込み最適化、つまり書き込み時にすこしコストを払って読み込みを高速にし、
`WriteQuadtree` は書き込み最適化、つまり読み込み時にすこしコストを払って書き込みを高速にする。

`ReadQuadtree` の実装は

```
instance Injection ReadQuadtree where
  empty = ReadQuadtree Empty

  insert x y (ReadQuadtree Empty) = ReadQuadtree (Branch (x, y) Empty Empty Empty Empty)
  insert x y (ReadQuadtree (Branch (x', y') t1 t2 t3 t4))
    | x == x' || y == y' = ReadQuadtree (Branch (x, y) t1 t2 t3 t4)
    | otherwise =
        let
          ReadQuadtree t1' = if x < x' then insert x y (ReadQuadtree t1) else ReadQuadtree t1
          ReadQuadtree t2' = if x > x' then insert x y (ReadQuadtree t2) else ReadQuadtree t2
          ReadQuadtree t3' = if y < y' then insert x y (ReadQuadtree t3) else ReadQuadtree t3
          ReadQuadtree t4' = if y > y' then insert x y (ReadQuadtree t4) else ReadQuadtree t4
        in
          ReadQuadtree (Branch (x', y') t1' t2' t3' t4')

  lookupLeft _ (ReadQuadtree Empty) = Nothing
  lookupLeft x (ReadQuadtree (Branch (x', y) t1 t2 t3 t4))
    | x == x' = Just y
    | x < x' = lookupLeft x (ReadQuadtree t1)
    | x > x' = lookupLeft x (ReadQuadtree t2)

  lookupRight _ (ReadQuadtree Empty) = Nothing
  lookupRight y (ReadQuadtree (Branch (x, y') t1 t2 t3 t4))
    | y == y' = Just x
    | y < y' = lookupRight y (ReadQuadtree t3)
    | y > y' = lookupRight y (ReadQuadtree t4)
```

となる。この場合、常に値が複製されるので
空間効率が悪い代わりに `lookupLeft`, `lookupRight` が高速になる。
要はこれは `(Map a b, Map b a)` とほとんど同じだ。

```
> fromAList [(1, 2), (3, 4), (5, 6)] :: ReadQuadtree Int Int
ReadQuadtree (Branch (5,6) (Branch (3,4) (Branch (1,2) Empty Empty Empty Empty) Empty (Branch (1,2) Empty Empty Empty Empty) Empty) Empty (Branch (3,4) (Branch (1,2) Empty Empty Empty Empty) Empty (Branch (1,2) Empty Empty Empty Empty) Empty) Empty)
```

`WriteQuadtree` はどうだろう。

```
instance Injection WriteQuadtree where
  empty = WriteQuadtree Empty

  insert x y (WriteQuadtree Empty) = WriteQuadtree (Branch (x, y) Empty Empty Empty Empty)
  insert x y (WriteQuadtree (Branch (x', y') t1 t2 t3 t4))
    | x == x' || y == y' = WriteQuadtree (Branch (x, y) t1 t2 t3 t4)
    | otherwise =
        let
          WriteQuadtree t1' = if x < x' && y < y' then insert x y (WriteQuadtree t1) else WriteQuadtree t1
          WriteQuadtree t2' = if x < x' && y > y' then insert x y (WriteQuadtree t2) else WriteQuadtree t2
          WriteQuadtree t3' = if x > x' && y < y' then insert x y (WriteQuadtree t3) else WriteQuadtree t3
          WriteQuadtree t4' = if x > x' && y > y' then insert x y (WriteQuadtree t4) else WriteQuadtree t4
        in
          WriteQuadtree (Branch (x', y') t1' t2' t3' t4')

  lookupLeft _ (WriteQuadtree Empty) = Nothing
  lookupLeft x (WriteQuadtree (Branch (x', y) t1 t2 t3 t4))
    | x == x' = Just y
    | x < x' = Monad.mplus (lookupLeft x (WriteQuadtree t1)) (lookupLeft x (WriteQuadtree t2))
    | x > x' = Monad.mplus (lookupLeft x (WriteQuadtree t3)) (lookupLeft x (WriteQuadtree t4))

  lookupRight _ (WriteQuadtree Empty) = Nothing
  lookupRight y (WriteQuadtree (Branch (x, y') t1 t2 t3 t4))
    | y == y' = Just x
    | y < y' = Monad.mplus (lookupRight y (WriteQuadtree t1)) (lookupRight y (WriteQuadtree t3))
    | y > y' = Monad.mplus (lookupRight y (WriteQuadtree t2)) (lookupRight y (WriteQuadtree t4))
```

これは値が複製されない代わりに、探索範囲が若干広いので、読み込みは遅くなる。
遅いといっても連想リストや Map の values を線形探索するよりは爆速だ。
もちろん木のバランスがいい場合。加えて言えば依存関係がないので明らかに並列化や分散処理が可能である。

```
> fromAList [(1, 2), (3, 4), (5, 6)] :: WriteQuadtree Int Int
WriteQuadtree (Branch (5,6) (Branch (3,4) (Branch (1,2) Empty Empty Empty Empty) Empty Empty Empty) Empty Empty Empty)
```

さてここまで考えることができたなら、これをバランシングしたものやハッシュ値の比較を順序関係と考えたもの、
キーと値を 2 個ではなく n 個に拡張したものも考えられる。
それはそれでおもしろそうなものの、続きはまた今度考えよう。今日はここまで。
（データ構造とアルゴリズムが大好き。ちょっと楽しかった）
