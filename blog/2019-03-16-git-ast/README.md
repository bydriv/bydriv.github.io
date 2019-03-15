# git ast diff がほしい

git ast diff みたいなサブコマンドがほしい(切実)。

ほんとはこういうの実装してみたぜ！
みたいなブログが書けたらかっこいいんだけどそういう余力もないので案だけ書いてみる。

## 動作イメージ

こういうファイルがあったとして:

**hello.rs.xml**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<seq>
  <token>fn</token>
  <sep> </sep>
  <token>main</token>
  <token>(</token>
  <token>)</token>
  <token>{</token>
  <token>println!</token>
  <token>(</token>
  <token>"hello world"</token>
  <token>)</token>
  <token>;</token>
  <token>}</token>
</seq>
```

こうできる

```
# ふつうの git diff
$ git diff hello.rs.xml
diff --git a/blog/2019-03-16-git-ast/hello.rs.xml b/blog/2019-03-16-git-ast/hello.rs.xml
index d5733fb..85df1bf 100644
--- a/blog/2019-03-16-git-ast/hello.rs.xml
+++ b/blog/2019-03-16-git-ast/hello.rs.xml
@@ -8,7 +8,7 @@
   <token>{</token>
   <token>println!</token>
   <token>(</token>
-  <token>"hello world"</token>
+  <token>"Hello, World!"</token>
   <token>)</token>
   <token>;</token>
   <token>}</token>
# git ast diff
$ git ast diff hello.rs.xml
diff --git a/blog/2019-03-16-git-ast/hello.rs b/blog/2019-03-16-git-ast/hello.rs
index ad379d6..0672e51 100644
--- a/blog/2019-03-16-git-ast/hello.rs
+++ b/blog/2019-03-16-git-ast/hello.rs
@@ -1,3 +1,3 @@
 fn main() {
-    println!("hello world");
+    println!("Hello, World!");
 }
```

まあつまりリポジトリには `hello.rs.xml` ってファイルを置いとく。
それを `git ast` で透過的に diff を見たりできるようにしたい。

`git ast` が認識する xml を dtd とかで定義しておいて:

**git-ast.dtd**:

```
<!ELEMENT seq (token|sep)+>
<!ELEMENT token (#PCDATA)>
<!ELEMENT sep (#PCDATA)>
```

この書式で保存されている xml の場合、 `git ast diff` が自動的に `rustfmt` とか呼んでフォーマットしてから差分を表示してくれる。

こういうのほしいんだけど、 git のコードベースの知識ないしだれかやってくれないかな～(ﾁﾗｯ

## なぜほしいのか

- code format は表示側がやってほしい。
  リポジトリに置くファイルに formatter をかける必要はない(とくに設定を変更したときのコンフリクトがつらい)。
- コードスタイルとかの議論から解放されたい。
- ついでに GitHub にもできれば対応してほしい（わがまま

## AST じゃねーだろ

ハイスイマセン。
上記の例はただの字句列ですね。
実用上はこの程度の(木構造ではなく字句列程度の)表現で問題ないと思う。
ただやりたいことの目的意識としては ast って言ったほうがキモチは伝わりやすいと思いました。

というかそもそも xml で保存する必要もなくて git diff のときにフォーマッタかけてくれればいい。
でも正直、 plain text で保存しておくとぜったい保存時にも format したくなるからもう潔く xml で保存してほしいと思いました。
