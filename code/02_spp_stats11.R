# Calculate stats for each species in each ecoregion
# The pct field uses the range maps to estimate proportion of range in ecoregion
# However all the stats (mean, p90, etc.) are based on the whole density mapped, not clipped to range
# Pierre Vernier
# 2019-09-11

library(raster)
library(tidyverse)

birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
reco = raster("data/raster/ecoregions.tif")
pba = read_csv("code/input/pan_eco_mdr_v4.csv") %>% arrange(ecoregion)
ecoregions = pba$ecoregion
net = read_csv("output/tables/ecozone_models_1000_ecoregions.csv")
net_eco = unique(net$ecoregion)

x <- tibble(ecozone=as.character(),ecoprovince=as.numeric(),ecoregion=as.integer(), species=as.character(), eco_cells=as.integer(), spp_cells=as.integer(), pct_range=as.integer(), min=as.numeric(), mean=as.numeric(), median=as.numeric(), p80=as.numeric(), p90=as.numeric(), p95=as.numeric(), max=as.numeric(), std_dev=as.numeric())
for (eco in sort(ecoregions)) {
    cat("Ecoregion",eco,"\n")
    flush.console()
    # Extract ecoregion and assign value of 1
    r = trim(calc(reco, fun=function(x) {x[!x==eco]=NA; return(x/x)}))
    r_ncells = ncell(r[r>-Inf & r<Inf])
    for (spp in birds) {
        cat("...",spp,"\n")
        flush.console()
        # Extract spp raster
        rspp = raster(paste0("data/songbirds/",spp,".tif"))
        r1 = mask(crop(rspp, r),r)
        r1_ncells = ncell(r1[r1>-Inf & r1<Inf])
        rrange = raster(paste0("data/songbirds_range/",spp,"_range.tif"))
        r1range = mask(crop(rrange, r),r)
        r1range_ncells = ncell(r1range[r1range>-Inf & r1range<Inf])
        r1range_pct = round(r1range_ncells/r_ncells*100,0)
        #if (ncells>0) {
            x = add_row(x, ecozone=pba$ecozone[pba$ecoregion==eco], ecoprovince=pba$ecoprovince[pba$ecoregion==eco], ecoregion=eco, species=spp,
                eco_cells = r_ncells,
                spp_cells = r1_ncells,
                pct_range = r1range_pct,
                min = round(cellStats(r1, min),4),
                mean = round(cellStats(r1, mean),4),
                median = round(cellStats(r1, median),4),
                p80 = round(quantile(r1, probs=c(0.8)),4),
                p90 = round(quantile(r1, probs=c(0.9)),4),
                p95 = round(quantile(r1, probs=c(0.95)),4),
                max = round(cellStats(r1, max),4),
                std_dev = round(cellStats(r1, sd),4))
        #} else {
        #    cat("      ... no data\n")
        #}
    }
}
write_csv(x, paste0("code/input/species_stats11.csv"))
