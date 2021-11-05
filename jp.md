# ggplot2_GUi

ggplot2をshinyベースでGUI化したものです。

# Features

このソースコードは[この論文の考え](https://www.ahajournals.org/doi/full/10.1161/CIRCULATIONAHA.118.037777)のもと、作成されています。

したがって、ドットプロットおよび箱ひげ図が用意されていますが、棒グラフはありません。

色やフォント、グラフのサイズなど細かい設定を用意しています。

さまざまな値をブックマークし、復元するオプション機能があります。これによって、なんども値の設定をせずに済みます。

 - 1.オプションタブでブックマースし、RDSファイルで保存します。

 - 2.復元する際、そのRDSファイルをアップロードします。仕様上、設定を復元したあとにデータファイルをアップロードしなければなりません。

# Requirement

R.studioは2021.09.0+351、Rは4.1.1です。

データファイルはCSVファイルか、xlsxファイルが対応しています。

また、最初の行には、データラベルをつけ、縦に長いデータ配列にする必要があります。サンプルデータをつけています。ご確認ください。

# Setting

1. [Use online](https://shun4423.shinyapps.io/ggplot2_GUi/)

 　　 - これが最も簡単な方法です。ただし、オンラインでの使用を想定していないため、一部の機能（フォントの変更など）が利用できない場合があります。
   
2. [Use offline](https://shun4423.github.io/ggplot2_GUi/)
   
   a. 不足しているライブラリーをインストールする。
   
   b. C:\Users\your user name\Documents\Rにフォルダを作成し、各Rファイルとｗｗｗを収納する。
   
   c. Rstudioを起動し、右下のウィンドウからダウンロードしたRファイルをダブルクリックする。
    
   d. ▶Run App をクリックする。
# Author

作成情報を列挙する

* 伏見駿亮(Fushimi Shunsuke)
* Kyoto U
* f.shunsuke0402@gmail.com

# License
ライセンスを明示する

[MIT license](https://en.wikipedia.org/wiki/MIT_License).
