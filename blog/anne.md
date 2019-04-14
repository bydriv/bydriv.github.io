<h2>Anne: 軽量データ構造</h2><p>Anne というシリアライズフォーマットを開発したので紹介する。</p><h3>動機</h3><p>もうすこし機械的に処理するのが簡単な軽量マークアップ言語がほしい。
というよりも、マークアップ言語くらい楽に書けるシリアライズフォーマットがほしかった。</p><h3>なぜ文章をシリアライズフォーマットで書くのか</h3><p>Markdown のようなマークアップ言語で文章を書くと、
どうしても Markdown で表現できないなにかを書きたくなる。
これは Markdown の問題ではなくて、そもそも人間が書く自然言語の文章というのは、
想像以上に複雑な対象なのである。</p><p>たとえば数式を書きたいという要求があったりする。
Pandoc なら <code>--mathml</code> オプションをつけたりすると <code>$</code> でかこった部分を数式扱いで処理してくれる。
でも当然 GitHub の Markdown は対応してなかったり、
結局処理系が場当たり的に拡張しているだけで、いろいろつらいものがある。</p><p>もちろん、数式を書きたいという要求がなければそういうものに対応する必要はない。
それに <code>`</code> でかこうというようなコードを表す記法も、プログラマでなければ必要ないだろう。
数学徒やプログラマなら <code>--mathml</code> オプションや <code>`</code> といった記法だけで
要求のほとんどを満たすことができるが、たとえば小説を書くのに Markdown が適しているかというと、
やはり、足りない機能が多いということになりがちな気がする。</p><p>要するに、文章は文章でも数学の文章やコンピュータに関する文章、
小説のような文章では要請される機能が違いすぎるのであり、
同じフォーマットでさまざまな分野の文章を書くというのは、土台無理な話なのだと思う。</p><p>かと言って、分野ごとにそれ専用のフォーマットを書くというのも、あまりよい解決方法には思えない。
むしろ必要なのは設定次第でいくらでも拡張でき、方言を無数に定義できるフォーマットにほかならない。</p><p>S式はその要求に応えられる可能性はあるし、何度か試してもみた。
S式はシリアライズフォーマットの一種だといえるし、方向性としてはまちがっていなかったと思う。
ただ結論としては実際のところ、プログラムならともかく文章を書く用途としては、
もうすこし軽量な構文で書きたいというのが本音だと思う。</p><h3>Anne の概説</h3><p>Anne の構文を概説する。</p><p>まずソースコードは空行によって段落(<em>paragraph</em>)に分けられる。
複数の空行はひとつの空行と同じものとして解釈される。
以下の例では、段落は3つあるものとなる。</p><pre>段落1
段落1

段落2



段落3
段落3
段落3
</pre><p>段落は <em>datum</em> に分けられる。
datum はアトム、リストの2種類がある。</p><pre>アトム
&bsol;[&bsol;] (1バイトエスケープする)
&lt;&lt;ATOM
ヒアドキュメント
ATOM
[リスト]
[木 [枝] [枝]]
</pre><p>アトムはヒアドキュメントの記法を使うこともできる。
リストは untyped なので木をつくることもできる。</p><p>以上ですべてである。</p><h3>処理系</h3><h4>anne</h4><p>anne は Anne を JSON に変換する処理系でである。
たとえば、先述の例なら</p><pre>$ cat tmp.json | anne
[["段落1&bsol;n段落1"],["段落2"],["段落3&bsol;n段落3&bsol;n段落3"],["アトム&bsol;n[] (1バイトエスケープする)&bsol;n","ヒアドキュメント&bsol;n","&bsol;n",["リスト"],"&bsol;n",["木 ",["枝"]," ",["枝"]]]]
</pre><p>というふうに変換される。</p><h4>shirley</h4><p>shirley は Anne を JSON に変換したものを HTML に変換する処理系である。
たとえば、つぎのような Anne を用意したとする。</p><pre>&#x23;h2 Hello World

hello world

#pre &lt;&lt;CODE
&bsol;#h2 Hello World

hello world
CODE
</pre><p>さらに、これを変換するためのつぎのような定義ファイルを与える。</p><pre>[
    ["paragraph", "&bsol;&bsol;A#(&bsol;&bsol;S+)&bsol;&bsol;s*(.*)&bsol;&bsol;z", "&lt;&bsol;&bsol;1&gt;&bsol;&bsol;2&lt;/&bsol;&bsol;1&gt;"],
    ["paragraph", "&bsol;&bsol;A(.*)&bsol;&bsol;z", "&lt;p&gt;&bsol;&bsol;1&lt;/p&gt;"],
    ["inline", "&bsol;&bsol;A#(&bsol;&bsol;S+)&bsol;&bsol;s*(.*)&bsol;&bsol;z", "&lt;&bsol;&bsol;1&gt;&bsol;&bsol;2&lt;/&bsol;&bsol;1&gt;"],
    ["atom", "&bsol;&bsol;A&bsol;&bsol;&bsol;&bsol;#", "&amp;#x23;"],
    ["atom", "&amp;", "&amp;amp;"],
    ["atom", "&bsol;&bsol;&bsol;&bsol;", "&amp;bsol;"],
    ["atom", "&lt;", "&amp;lt;"],
    ["atom", "&gt;", "&amp;gt;"]
]
</pre><p>この定義ファイルは、 <code>#tag</code> のようなコードでタグを表す Anne の方言を定義することを意味する。</p><p>そのうえで shirley で anne を html を変換すると、つぎのようになる。</p><pre>&lt;h2&gt;Hello World&lt;/h2&gt;&lt;p&gt;hello world&lt;/p&gt;&lt;pre&gt;&amp;#x23;h2 Hello World

hello world
&lt;/pre&gt;
</pre>
