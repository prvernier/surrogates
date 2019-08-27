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
        if(file.exists(paste0("output/ecoregions/",eco,"_nets_pba_spp_ks_",rnd,".csv"))) {
            x = read_csv(paste0("output/ecoregions/",eco,"_nets_pba_spp_ks_",rnd,".csv"))
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
    write_csv(xx, paste0("output/ecozones/ecoz_",ecoz,"_networks_spp_",rnd,".csv"))
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
zzz = mutate(zz, ecozone=paste0("z",ecozone), mdr=NULL, intact=NULL, intact_eco=NULL, coverage=NULL, overlap=NULL, net_km2=NULL, eco_km2=NULL, ecoprovince=NULL) %>%
    rename(sumgap90=sumGap90,allbirds=AllBirds,forestbirds=ForestBirds,allwaterfowl=AllWaterfowl,cavitynesters=CavityNesters,groundnesters=GroundNesters,overwaternesters=OverwaterNesters)
# write merged ks file
write_csv(zzz, "output/tables/species_surrogates_ks.csv")

notUsefulAtEcoregionLevel = function() {
    # add ecoregion centroids
    v = st_read("data/vector/pba_ecoregions.shp")
    vcentroids = st_centroid(v)
    vcoords = as_tibble(st_coordinates(vcentroids))
    vv = tibble(ecoregion=as.numeric(v$ecoreg),xcoord=vcoords$X,ycoord=vcoords$Y) %>%
        mutate(xcoord=round(xcoord,0),ycoord=round(ycoord,0))
    zz = left_join(zz, vv)

    # join with species_stats_clipped_cv.csv
    stats = read_csv("code/input/species_stats_clipped_cv.csv") %>%
        mutate(ecoregion=zone, cv_mean=mean) %>% 
        dplyr::select(ecoregion, species, cv_mean) %>%
        spread(species, cv_mean)
    names(stats) = c("ecoregion", paste0(pba_nwb_birds,"_cv"))
    zzz = left_join(zz, stats)
    zzz = mutate(zzz, ecozone=as.character(paste0("z",ecozone)))
    zzz = mutate(zzz, ecoprovince=as.character(paste0("p",ecoprovince)))
    zzz = mutate(zzz, ecoregion=as.character(paste0("r",ecoregion)))
    zzz = rename(zzz, allbirds=AllBirds,forestbirds=ForestBirds,allwaterfowl=AllWaterfowl,cavitynesters=CavityNesters,groundnesters=GroundNesters,overwaternesters=OverwaterNesters,
        allbirds_eco_pct=AllBirds_eco_pct,forestbirds_eco_pct=ForestBirds_eco_pct,allwaterfowl_eco_pct=AllWaterfowl_eco_pct,cavitynesters_eco_pct=CavityNesters_eco_pct,groundnesters_eco_pct=GroundNesters_eco_pct,overwaternesters_eco_pct=OverwaterNesters_eco_pct,
        allbirds_net_pct=AllBirds_net_pct,forestbirds_net_pct=ForestBirds_net_pct,allwaterfowl_net_pct=AllWaterfowl_net_pct,cavitynesters_net_pct=CavityNesters_net_pct,groundnesters_net_pct=GroundNesters_net_pct,overwaternesters_net_pct=OverwaterNesters_net_pct)
}

# write merged ks file
#write_csv(zzz, "output/tables/species_surrogates_ks.csv")
