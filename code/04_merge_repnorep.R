# Merge nets_pba_spp_ks_1000 files by ecozone
# Pierre Vernier
# 2019-08-16

library(sf)
library(tidyverse)

pba_nwb_birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
spp_to_use = c('AllBirds','ForestBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters',pba_nwb_birds)
pba = read_csv('code/input/pan_eco_mdr_v4.csv')
ecozones = c("4","5","6A","6B","9","11","12","14","15")
rnd = 1000

z = 1
for (ecoz in ecozones) {
    cat("Ecozone",ecoz,"...\n")
    flush.console()
    ecoregions = filter(pba, ecozone==ecoz) %>% pull(ecoregion) %>% sort()
    i = 1
    for (eco in ecoregions) {
        if(file.exists(paste0("output/ecoregions_repnorep/",eco,"_nets_pba_spp_ks_",rnd,".csv"))) {
            x = read_csv(paste0("output/ecoregions_repnorep/",eco,"_nets_pba_spp_ks_",rnd,".csv"))
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
    write_csv(xx, paste0("output/ecozones_repnorep/ecoz_",ecoz,"_networks_spp_",rnd,".csv"))
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
zzz = mutate(zz, ecozone=paste0("z",ecozone), ecoprovince=NULL) %>%
    rename(allbirds=AllBirds,forestbirds=ForestBirds,allwaterfowl=AllWaterfowl,cavitynesters=CavityNesters,groundnesters=GroundNesters,overwaternesters=OverwaterNesters) %>%
    mutate(rep=if_else(ks_cmi<=0.2 & ks_gpp<=0.2 & ks_led<=0.2 & bc_lcc<=0.2,1,0))
# write merged ks file
write_csv(zzz, "output/tables/species_surrogates_ks_repnorep.csv")
