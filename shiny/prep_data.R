library(tidyverse)

#x <- read_csv('output/eco_bcr_data_by_spp.csv') %>%
#    select(network,ks_cmi,ks_gpp,ks_led,bc_lcc,caribou,allbirds,forestbirds,coniferbirds,deciduousbirds,mixedwoodbirds,grasslandbirds,neomigrantbirds,shortmigrantbirds,nomadicbirds,residentbirds,decliningbirds,lowconcernbirds,allwaterfowl,cavitynesters,groundnesters,overwaternesters,rep,ecoregion,surrogates,bcr)
#names(x) = c('network','ks_cmi','ks_gpp','ks_led','bc_lcc','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep','ecoregion','surrogates','bcr')

# Create "Representativeness" data
z1 = read_csv('output/eco_bcr_data_by_spp.csv') %>% 
    select(bcr, blbw:overwaternesters, rep)
names(z1) = c('bcr','BLBW','BOCH','BRCR','BTNW','CAWA','CMWA','OSFL','PIGR','RUBL','SWTH','WWCR','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep')
gz1 = gather(z1, species, Dissimilarity, -bcr, -rep) %>% 
    filter(!bcr=='BCR10') %>%
    mutate(Networks = if_else(rep==1,"Rep","Non-rep"), rep=NULL)
write_csv(gz1, 'shiny/rep_meanDissim.csv')

z2a = read_csv('output/eco_bcr_data_by_spp.csv') %>% select(network, blbw:wwcr)
z2b = read_csv('output/eco_bcr_data.csv')
z2 = left_join(z2a, z2b) %>%
    select(bcr, blbw:wwcr, caribou:overwaternesters, rep)
names(z2) = c('bcr','BLBW','BOCH','BRCR','BTNW','CAWA','CMWA','OSFL','PIGR','RUBL','SWTH','WWCR','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep')
gz2 = gather(z2, species, Dissimilarity, -bcr, -rep) %>% 
    filter(!bcr=='BCR10') %>%
    mutate(Networks = if_else(rep==1,"Rep","Non-rep"), rep=NULL)
write_csv(gz2, 'shiny/rep_sumDensity.csv')

# Create "Surrogates importance" data
y1 = read_csv('output/obj3_bcr_birds_full_output_by_spp.csv')
write_csv(y1, 'shiny/vi_meanDissim.csv')
y2 = read_csv('output/obj3_bcr_birds_full_output.csv')
write_csv(y2, 'shiny/vi_sumDensity.csv')


# create meanKS and sumDensity tables for supplementary material
x1 = read_csv('output/eco_bcr_data.csv') %>% select(bcr, caribou:overwaternesters, rep)
names(x1) = c('BCR','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep')
write_csv(x1, 'supp/TableS3b_rep_sumDensity.csv')

x2 = read_csv('output/eco_bcr_data_by_spp.csv') %>% select(bcr, caribou:overwaternesters, rep)
names(x2) = c('BCR','Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','rep')
write_csv(x2, 'supp/TableS3a_rep_meanDissim.csv')


# sample regression output
bcr8 = read_csv('output/eco_bcr_data_by_spp.csv') %>%
    select(ks_cmi, ks_gpp, ks_led, bc_lcc, caribou, bcr, rep) %>%
    filter(bcr=='BCR8')
m8 = lm(caribou ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=bcr8)

