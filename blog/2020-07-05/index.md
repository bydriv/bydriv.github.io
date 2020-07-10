---
url: https://bydriv.github.io/blog/2020-07-05/
title: bydriv.github.io
description: A-Frame & WebVR 周辺について
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# A-Frame & WebVR 周辺について

先日 Oculus Quest が届いたので、 A-Frame というものを使ってみた。

まず VR App を開発するには Native で開発するか WebVR で開発するかという選択肢がある。
WebVR で開発する場合 WebVR の API を生で使ってももちろんいいが、
フレームワークを使うこともできる。 A-Frame はそういうフレームワークのひとつ、
という位置付けらしい。

開発したものはここ: [bydriv.github.io/vr](/vr)

いまのところ、上古の時代にモデリングの練習につくったテーリのモデルが置いてある。

基本的には公式のチュートリアルや Web 上の記事を読めばいい。
しかし API がけっこう頻繁に変わっていたようで、
最新バージョンで動かないコードも散見された
(たとえば、 `<a-cube>` タグは `<a-box>` タグに変更されたようだ)。
なので最終的に信頼すべきものは公式の API ドキュメント
([https://aframe.io/docs/1.0.0/introduction/](https://aframe.io/docs/1.0.0/introduction/)) だ。

## とりあえずモデルを表示する

まず Blender でモデリングをする(このへんについては割愛)。
Export するときは gltf というフォーマットで出力。
aframe の JS ファイルは `/vendor/aframe-v1.0.4.min.js` に、
model の glTF ファイルは `model.gltf` にそれぞれ置かれているものとする。
すると以下の html ファイルで WebVR に対応したページが作成できる。

```html
<!DOCTYPE html>
<html>
  <head>
    <script src="/vendor/aframe-v1.0.4.min.js"></script>
  </head>
  <body>
    <a-scene>
      <a-assets>
        <a-asset-item id="model" src="model.gltf"></a-asset-item>
      </a-assets>
      <a-gltf-model src="#model" position="0 1 0"></a-gltf-model>
      <a-camera
         position="0 1 4"
         cursor-visible="true"
         cursor-scale="2"
         cursor-color="#0095DD"
         cursor-opacity="0.5">
      </a-camera>
  </body>
</html>
```

(もし表示されない場合、モデルの大きさなどの関係で表示範囲外に配置されてしまっている可能性がある。
`<a-gltf-model>` や `<a-camera>` の `position` パラメタを適宜調整すること)

こうすると PC のブラウザで表示すると単なる 3D モデルが表示された web page に見えるが、
VR 機器ではフルスクリーン（？）にすることで VR で表示できる。

たとえば Oculus Quest の場合 Oculus Browser で当該ページにアクセスし、右下の VR ボタンを押す。

## glTF とは

glTF は 3D モデルのファイルフォーマットのひとつらしい。
この形式のファイルフォーマットを得るには、

1. Blender で直接 gltf を出力する
2. COLLADA2GLTF を利用する

などの方法があるようだ。
