# Generate shiny data
# PV 2019-09-24

library(sf)
library(tidyverse)

# Species stats table
#stats = read_csv("code/input/species_stats_clipped.csv")
#save(stats, file = "shiny/stats.Rdata")

# Ecoregions map
#v = st_read("data/vector/pba_ecoregions.shp")
#ecor_maps =  v %>% st_transform(4326)
#save(ecor_maps, file = "shiny/ecor_maps.Rdata")

### KS table (random sample of 1000 networks)
###ks = read_csv("output/tables/species_surrogates_ks.csv")
###save(ks, file = "shiny/ks.Rdata")

# KS table (rep and nonrep networks)
ks = read_csv("output/tables/species_surrogates_ks_rnr.csv")
nets = read_csv("code/input/ecoregion_statistics.csv") %>% dplyr::select(ecoregion, intactness, mdr,
    blbw_dens, boch_dens, brcr_dens, btnw_dens, cawa_dens, cmwa_dens, osfl_dens, pigr_dens, rubl_dens, swth_dens, wwcr_dens,
    caribou_dens,allbirds_dens,forestbirds_dens,allwaterfowl_dens,cavitynesters_dens,groundnesters_dens,overwaternesters_dens,
    blbw_cv, boch_cv, brcr_cv, btnw_cv, cawa_cv, cmwa_cv, osfl_cv, pigr_cv, rubl_cv, swth_cv, wwcr_cv,
    caribou_cv,allbirds_cv,forestbirds_cv,allwaterfowl_cv,cavitynesters_cv,groundnesters_cv,overwaternesters_cv)
ks = left_join(ks, nets)
save(ks, file = "shiny/ks.Rdata")

### KS table (mix of random sample and all rep and nonrep networks)
###x1 = mutate(ks, sumgap90=NULL, rep=0.5)
###x2 = ks
###x1 = filter(x1, !network %in% x2$network)
###ks_mix = bind_rows(x2,x1)
###save(ks_mix, file = "shiny/ks_mix.Rdata")
