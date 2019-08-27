library(sf)
library(tidyverse)

# Species stats table
stats = read_csv("code/input/species_stats_clipped.csv")
save(stats, file = "shiny/stats.Rdata")

# Ecoregions map
v = st_read("../data/vector/pba_ecoregions.shp")
ecor_maps =  v %>% st_transform(4326)
save(ecor_maps, file = "../shiny/ecor_maps.Rdata")

# KS table (random sample of 1000 networks)
ks = read_csv("output/tables/species_surrogates_ks.csv")
save(ks, file = "shiny/ks.Rdata")

# KS table (rep and nonrep networks)
ks_repnorep = read_csv("output/tables/species_surrogates_ks_repnorep.csv")
save(ks_repnorep, file = "shiny/ks_repnorep.Rdata")

# KS table (mix of random sample and all rep and nonrep networks)
x1 = mutate(ks, distance=NULL, sumgap90=NULL, rep=0.5)
x2 = ks_repnorep
x1 = filter(x1, !network %in% x2$network)
ks_mix = bind_rows(x2,x1)
save(ks_mix, file = "shiny/ks_mix.Rdata")
