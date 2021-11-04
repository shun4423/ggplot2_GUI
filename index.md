# ggplot2　GUI

A shiny based GUI version of ggplot2.

# Features

This source code is based on [the idea of this paper](https://www.ahajournals.org/doi/full/10.1161/CIRCULATIONAHA.118.037777).

Therefore, dot plots and box plots are provided, but no bar plots.

Detailed settings such as color, font, and graph size are available.


<img src="https://user-images.githubusercontent.com/60542816/139782310-db2d0282-ab8c-46c5-a68a-a43b2f01cee4.png" width="50%" height="50%"> 
<img src="https://user-images.githubusercontent.com/60542816/139782345-d69d8e7a-3a04-4177-8c0e-9f320fc843f6.png" width="50%" height="50%">


There is an optional feature to bookmark and restore various values. This saves you from having to set the values again and again.

 - Bookmark them in the Options tab and save them in an RDS file.

 - When you restore, upload that RDS file. Due to specifications, you have to upload the data file after restoring the configuration.

# Requirement

R.studio is 2021.09.0+351, R is 4.1.1.

The data file can be a CSV file or an xlsx file.

Also, the first row should have a data label and be a long vertical data array. A sample data is attached. Please check it out.

# Setting

Install the missing libraries.

Create a folder in C:\Users\your user name\Documents\R and store each R file and www.

Start Rstudio and double-click the downloaded R file in the lower right window.

Click ▶ Run App.



# ggplot2_GUI

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

不足しているライブラリーをインストールする。

C:\Users\your user name\Documents\Rにフォルダを作成し、各Rファイルとｗｗｗを収納する。

Rstudioを起動し、右下のウィンドウからダウンロードしたRファイルをダブルクリックする。

▶Run App をクリックする。

# Author

作成情報を列挙する

* 伏見駿亮(Fushimi Shunsuke)
* Kyoto U
* f.shunsuke0402@gmail.com

# License
ライセンスを明示する

[MIT license](https://en.wikipedia.org/wiki/MIT_License).
