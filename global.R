#global.R
library(shiny)
library(shinyjqui)
library(ggplot2)
library(shinythemes)
library(htmlwidgets)
library(colourpicker)
library(DT)
library(openxlsx)
library(systemfonts)
library(fontregisterer)
library(cowplot)
library(ggpubr)

library(RColorBrewer)

library(shinyjs)
library(utils)
library(tools)
library(stringi)
library(wesanderson)
library(grDevices)
library(stringr)
library(ggbeeswarm)
enableBookmarking(store = "server")

#devtools::session_info()
#ggplotgui::ggplot_shiny(iris)

palet <- list("one" = list("YlOrRd","YlOrBr","YlGnBu","YlGn","Reds","RdPu","Purples","PuRd","PuBuGn","PuBu","Oranges","OrRd","Greys","Greens","GnBu","Blues","BuPu","BuGn"),
              "two"= list("Paired","Spectral","RdYlGn","RdYlBu","RdGy","RdBu","PuOr","PRGn","PiYG","BrBG"),
              "wesanderson" = list("GrandBudapest1","GrandBudapest2","Moonrise1","Moonrise2","Moonrise3","Royal1","Royal2","Cavalcanti1",
                                   "Chevalier1","Zissou1","FantasticFox1","Darjeeling1","Darjeeling2","Rushmore1"))


font <-c("BIZ UDPGothic", "BIZ UDPMincho", "Calibri", "Meiryo","Meiryo UI","Segoe UI","Times New Roman" ,"Yu Gothic","Yu Gothic UI","Yu Mincho")



#shokiti
shokiti_summ <- "SE"
type_fomu <- y ~ x
div4 <- 1
w <-7
data_type <- "csv"
sheet_name <- 1

