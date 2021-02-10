library(tidyverse)
library(effectsize)

x = read_csv('input/bcr4_deciduousbirds.csv')
cohens_d(x$dissim[x$rep==0], x$dissim[x$rep==1])
