library(tidyverse)

#x <- read_csv('output/eco_bcr_data_by_spp.csv') %>%
#    select(network,ks_cmi,ks_gpp,ks_led,bc_lcc,caribou,allbirds,forestbirds,coniferbirds,deciduousbirds,mixedwoodbirds,grasslandbirds,neomigrantbirds,shortmigrantbirds,nomadicbirds,residentbirds,decliningbirds,lowconcernbirds,allwaterfowl,cavitynesters,groundnesters,overwaternesters,rep,ecoregion,surrogates,bcr)
#names(x) = c('network','ks_cmi','ks_gpp','ks_led','bc_lcc','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep','ecoregion','surrogates','bcr')

z = read_csv('output/eco_bcr_data_by_spp.csv') %>% select(bcr, blbw:overwaternesters, rep)
names(z) = c('bcr','BLBW','BOCH','BRCR','BTNW','CAWA','CMWA','OSFL','PIGR','RUBL','SWTH','WWCR','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep')
gz = gather(z, species, Dissimilarity, -bcr, -rep) %>% 
    filter(!bcr=='BCR10') %>%
    mutate(Networks = if_else(rep==1,"Rep","Non-rep"), rep=NULL)

write_csv(gz, 'surrogates/rep.csv')
