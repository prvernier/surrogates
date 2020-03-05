library(sf)
library(shiny)
library(tidyverse)

sppList = c("all_birds","for_birds","caribou","boch","brcr","btnw","cawa","cmwa","pigr","rubl","swth","wwcr")
ecor = st_read("data/pba_ecor.shp")
ecop = st_read("data/pba_ecop.shp")
ecoz = st_read("data/pba_ecoz.shp")

