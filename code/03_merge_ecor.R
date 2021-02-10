# Merge eco_xxx_nets_spp_ks files by ecozone
# PV 2020-08-25

library(sf)
library(tidyverse)

pba_nwb_birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
spp_to_use = c('AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters',pba_nwb_birds)
pba = read_csv('input/pan_eco_mdr_v4.csv')
ecozones = c("4","5","6A","6B","9","11","12","14","15")

z = 1
for (ecoz in ecozones) {
    cat("Ecozone",ecoz,"...\n")
    flush.console()
    ecoregions = filter(pba, ecozone==ecoz) %>% pull(ecoregion) %>% sort()
    i = 1
    for (eco in ecoregions) {
        if(file.exists(paste0('input/ecoregions/eco_',eco,'_nets_spp_ks.csv'))) {
            x = read_csv(paste0('input/ecoregions/eco_',eco,'_nets_spp_ks.csv'))
            #x = dplyr::select(x, 1:5 , spp_to_use)
            x = x%>% mutate(ecoregion=eco)
            x$ecoprovince=pba$ecoprovince[pba$ecoregion==eco]
            if (i==1) {
                xx = x
            } else {
                xx = rbind(xx, x)
            }
            i = i + 1
        }
    }
    write_csv(xx, paste0('input/ecozones/ecoz_',ecoz,'_nets_spp_ks.csv'))
    xx$ecozone=ecoz
    if (z==1) {
        zz = xx
    } else {
        zz = rbind(zz, xx)
        #print(dim(zz))
    }
    z = z + 1
}

# drop these - not needed for ecoregion level
#zzz = mutate(zz, ecozone=paste0("z",ecozone), ecoprovince=NULL) %>%
#    rename(caribou=Caribou,allbirds=AllBirds,forestbirds=ForestBirds,
#    coniferbirds=ConiferBirds,deciduousbirds=DeciduousBirds,mixedwoodbirds=MixedwoodBirds,grasslandbirds=GrasslandBirds,
#    neomigrantbirds=NeoMigrantBirds,shortmigrantbirds=ShortMigrantBirds,nomadicbirds=NomadicBirds,residentbirds=ResidentBirds,
#    decliningbirds=DecliningBirds,lowconcernbirds=LowConcernBirds,
#    allwaterfowl=AllWaterfowl,cavitynesters=CavityNesters,groundnesters=GroundNesters,overwaternesters=OverwaterNesters)
    #mutate(rep2=if_else(ks_cmi<=0.2 & ks_gpp<=0.2 & ks_led<=0.2 & bc_lcc<=0.2,1,0))

# add intactness (PV 2020-04-14, formely intact_eco from species_surrogates_ks.csv)
#intact = read_csv('code/output/tables/ecoregion_statistics.csv') %>%
#    dplyr::select(ecoregion, intactness) %>% unique()
#zzz = left_join(zzz, intact)

# write merged ks file
#write_csv(zzz, 'code/output/tables/species_surrogates_ks_rnr.csv')
