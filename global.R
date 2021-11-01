#global.R
library(shiny)
library(shinyjqui)
library(ggplot2)
#library(tidyverse)
#library(DT)
#library(colourpicker)
library(openxlsx)
library(systemfonts)
library(fontregisterer)
library(RColorBrewer)
#ggiraph vector
library(shinyjs)
library(utils)
library(tools)
library(stringi)
library(wesanderson)
library(grDevices)
library(stringr)
library(ggbeeswarm)


enableBookmarking(store = "server")




palet <- list("one" = list("Blues", "Greens", "Greys","Purples","Reds", "Oranges"),
              "two"= list("BuGn","BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy","OrRd", "PuBu","BuPu", "GnBu","PuRd", "PuBuGn",   "RdPu",  "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "RdYlBu", "RdYlGn", "Spectral","Paired"),
              "wesanderson" = list("GrandBudapest1","GrandBudapest2","Moonrise1","Moonrise2","Moonrise3","Royal1","Royal2","Cavalcanti1",
                                   "Chevalier1","Zissou1","FantasticFox1","Darjeeling1","Darjeeling2","Rushmore1"))


font <-c("BIZ UDPGothic", "BIZ UDPMincho", "Calibri","Helvetica", "Meiryo","Meiryo UI","Segoe UI","Times New Roman" ,"Yu Gothic","Yu Gothic UI","Yu Mincho")