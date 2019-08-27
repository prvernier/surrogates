# Calculate stats for each species in each ecoregion
# Pierre Vernier
# 2019-08-21

library(raster)
library(tidyverse)

#birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
birds = read_csv("code/input/bam_density_80_species.csv") %>% pull(Code) %>% tolower()
reco = raster("data/raster/ecoregions.tif")
pba = read_csv("code/input/pan_eco_mdr_v4.csv") %>% arrange(ecoregion)
ecoregions = pba$ecoregion
net = read_csv("output/tables/ecozone_models_1000_ecoregions.csv")
net_eco = unique(net$ecoregion)

x <- tibble(ecozone=as.character(),ecoprovince=as.numeric(),ecoregion=as.integer(), spp=as.character(), eco_cells=as.integer(), spp_cells=as.integer(), min=as.numeric(), mean=as.numeric(), median=as.numeric(), p80=as.numeric(), p90=as.numeric(), p95=as.numeric(), max=as.numeric(), std_dev=as.numeric())
for (eco in sort(ecoregions)) {
    cat("Ecoregion",eco,"\n")
    # Extract ecoregion and assign value of 1
    r = trim(calc(reco, fun=function(x) {x[!x==eco]=NA; return(x/x)}))
    r_ncells = ncell(r[r>-Inf & r<Inf])
    for (spp in birds) {
        # Extract spp raster
        rspp = raster(paste0("data/songbirds/",spp,".tif"))
        r1 = mask(crop(rspp, r),r)
        r1_ncells = ncell(r1[r1>-Inf & r1<Inf])
        if (ncells>0) {
            x = add_row(x, ecozone=pba$ecozone[pba$ecoregion==eco], ecoprovince=pba$ecoprovince[pba$ecoregion==eco], ecoregion=eco, spp=spp,
                eco_cells = r_ncells,
                spp_cells = r1_ncells,
                min = round(cellStats(r1, min),4),
                mean = round(cellStats(r1, mean),4),
                median = round(cellStats(r1, median),4),
                p80 = round(quantile(r1, probs=c(0.8)),4),
                p90 = round(quantile(r1, probs=c(0.9)),4),
                p95 = round(quantile(r1, probs=c(0.95)),4),
                max = round(cellStats(r1, max),4),
                std_dev = round(cellStats(r1, sd),4))
        } else {
            cat("      ... no data\n")
        }
    }
}
write_csv(x, paste0("code/input/species_stats80.csv"))
