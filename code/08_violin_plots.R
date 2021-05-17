# Generate violin plots
# PV 2021-05-11

library(ggpubr)
library(tidyverse)

z = read_csv('output/eco_bcr_data_by_spp.csv') %>% select(bcr, allbirds:overwaternesters, rep)
names(z) = c('bcr','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep')
gz = gather(z, species, Dissimilarity, -bcr, -rep) %>% 
    filter(!bcr=='BCR10') %>%
    mutate(Networks = if_else(rep==1,"Rep","Non-rep"), rep=NULL)

hab = filter(gz, species %in% c('ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds')) %>% select(bcr, species, Dissimilarity, Networks)
mig = filter(gz, species %in% c('NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds')) %>% select(bcr, species, Dissimilarity, Networks)
con = filter(gz, species %in% c('AllBirds','ForestBirds','DecliningBirds','LowConcernBirds')) %>% select(bcr, species, Dissimilarity, Networks)
wfo = filter(gz, species %in% c('AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters')) %>% select(bcr, species, Dissimilarity, Networks)

p_hab = ggboxplot(hab, x="Networks", y="Dissimilarity", fill="Networks", palette=c('#f0f0f0','#bdbdbd','#636363')) +
    geom_hline(yintercept=0.2, linetype="dashed", colour="blue", size=0.5)
p_hab = facet(p_hab + theme_bw(), facet.by = c("species","bcr"))
p_hab

v_hab = ggviolin(hab, x="Networks", y="Dissimilarity", fill="Networks", 
    palette=c('#f0f0f0','#bdbdbd','#636363'), add = "median") +
    geom_hline(yintercept=0.2, linetype="dashed", colour="black", size=0.5)
v_hab = facet(v_hab + theme_bw(), facet.by = c("species","bcr"))
v_hab
    
v_mig = ggviolin(mig, x="Networks", y="Dissimilarity", fill="Networks", 
    palette=c('#f0f0f0','#bdbdbd','#636363'), add = "median") +
    geom_hline(yintercept=0.2, linetype="dashed", colour="black", size=0.5)
v_mig = facet(v_mig + theme_bw(), facet.by = c("species","bcr"))
v_mig

v_con = ggviolin(con, x="Networks", y="Dissimilarity", fill="Networks", 
    palette=c('#f0f0f0','#bdbdbd','#636363'), add = "median") +
    geom_hline(yintercept=0.2, linetype="dashed", colour="black", size=0.5)
v_con = facet(v_con + theme_bw(), facet.by = c("species","bcr"))
v_con

v_wfo = ggviolin(wfo, x="Networks", y="Dissimilarity", fill="Networks", 
    palette=c('#f0f0f0','#bdbdbd','#636363'), add = "median") +
    geom_hline(yintercept=0.2, linetype="dashed", colour="black", size=0.5)
v_wfo = facet(v_wfo + theme_bw(), facet.by = c("species","bcr"))
v_wfo
