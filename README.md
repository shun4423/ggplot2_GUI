# ggplot2_GUi

A shiny based GUI version of ggplot2.

# Features

This source code is based on [the idea of this paper](https://www.ahajournals.org/doi/full/10.1161/CIRCULATIONAHA.118.037777).

Therefore, dot plots and box plots are provided, but no bar plots.

Detailed settings such as color, font, and graph size are available.
![](https://user-images.githubusercontent.com/60542816/139782310-db2d0282-ab8c-46c5-a68a-a43b2f01cee4.png) ![](https://user-images.githubusercontent.com/60542816/139782345-d69d8e7a-3a04-4177-8c0e-9f320fc843f6.png)

There is an optional feature to bookmark and restore various values. This saves you from having to set the values again and again.

 - Bookmark them in the Options tab and save them in an RDS file.

 - When you restore, upload that RDS file. Due to specifications, you have to upload the data file after restoring the configuration.

# Requirement

R.studio is 2021.09.0+351, R is 4.1.1.

The data file can be a CSV file or an xlsx file.

Also, the first row should have a data label and be a long vertical data array. A sample data is attached. Please check it out.

# Setting

1. [Use online](https://shun4423.shinyapps.io/ggplot2_GUi/)

 　　 - This is the easiest way. However, it is not designed for online use, and some features may not be available (e.g., changing fonts).
   
2. Use offline
   
   a. Install the missing .
   
   b. Create a folder in C:\Users\your user name\Documents\R and store each R file and www.
   
   c. Start Rstudio and double-click the downloaded R file in the lower right window.
    
   d. Click ▶ Run App.


Secondary distribution is strictly prohibited.


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

1. [Use online](https://shun4423.shinyapps.io/ggplot2_GUi/)

 　　 - This is the easiest way. However, it is not designed for online use, and some features may not be available (e.g., changing fonts).
   
2. Use offline
   
   a. 不足しているパッケージをインストールする。
   
   b. C:\Users\your user name\Documents\Rにフォルダを作成し、各Rファイルとｗｗｗを収納する。
   
   c. Rstudioを起動し、右下のウィンドウからダウンロードしたRファイルをダブルクリックする。
    
   d. ▶Run App をクリックする。


# Author

* 伏見駿亮(Fushimi Shunsuke)
* Kyoto U
* f.shunsuke0402@gmail.com

# License

[MIT license](https://en.wikipedia.org/wiki/MIT_License).

二次配布を固く禁じます。
