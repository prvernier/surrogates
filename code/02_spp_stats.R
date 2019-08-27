# Calculate stats for each species in each ecoregion
# Pierre Vernier
# 2019-07-23

library(raster)
library(tidyverse)

birds = list.files("data/songbirds", pattern="tif")
birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
reco = raster("data/raster/ecoregions.tif")

rs = stack()
for (spp in birds) {
    # select full density map or range-restricted map
    #r = raster(paste0('data/songbirds/',spp,'.tif'))
    r = raster(paste0('data/songbirds/',spp,'_range.tif'))
    rs = addLayer(rs, r)
}
names(rs) = birds

q80 = function(x, probs=p, na.rm=TRUE) {raster::quantile(x, probs=0.80, na.rm=TRUE)}
q90 = function(x, probs=p, na.rm=TRUE) {raster::quantile(x, probs=0.90, na.rm=TRUE)}
q95 = function(x, probs=p, na.rm=TRUE) {raster::quantile(x, probs=0.95, na.rm=TRUE)}
ncells1 = function(x, na.rm=TRUE) {ncell(x)*4}
fun1 = function(x) { x[x-Inf & x<+Inf]=4; return(x) }
rs1 = calc(rs, fun1)

z_n1 = as.tibble(zonal(rs, reco, fun=ncells1)) %>% gather(species, area, 2:12) %>% arrange(zone)
z_n2 = as.tibble(zonal(rs1, reco, fun=sum)) %>% gather(species, data, 2:12) %>% arrange(zone)
z_min = as.tibble(zonal(rs, reco, fun=min))  %>% gather(species, min, 2:12) %>% arrange(zone) %>% mutate(min=if_else(min %in% c(Inf,NaN,NA,-Inf),0,round(min,4)))
z_mean = as.tibble(zonal(rs, reco, fun=mean)) %>% gather(species, mean, 2:12) %>% arrange(zone) %>% mutate(mean=if_else(mean %in% c(Inf,NaN,NA,-Inf),0,round(mean,4)))
z_median = as.tibble(zonal(rs, reco, fun=median)) %>% gather(species, median, 2:12) %>% arrange(zone) %>% mutate(median=if_else(median %in% c(Inf,NaN,NA,-Inf),0,round(median,4)))
z_max = as.tibble(zonal(rs, reco, fun=max)) %>% gather(species, max, 2:12) %>% arrange(zone) %>% mutate(max=if_else(max %in% c(Inf,NaN,NA,-Inf),0,round(max,4)))
z_80 = as.tibble(zonal(rs, reco, fun=q80)) %>% gather(species, p80, 2:12) %>% arrange(zone) %>% mutate(p80=if_else(p80 %in% c(Inf,NaN,NA,-Inf),0,round(p80,4)))
z_90 = as.tibble(zonal(rs, reco, fun=q90)) %>% gather(species, p90, 2:12) %>% arrange(zone) %>% mutate(p90=if_else(p90 %in% c(Inf,NaN,NA,-Inf),0,round(p90,4)))
z_95 = as.tibble(zonal(rs, reco, fun=q95)) %>% gather(species, p95, 2:12) %>% arrange(zone) %>% mutate(p95=if_else(p95 %in% c(Inf,NaN,NA,-Inf),0,round(p95,4)))

z = left_join(z_n1, z_n2) %>% mutate(pct=round(data/area*100,0)) %>% left_join(z_min) %>% left_join(z_mean) %>% left_join(z_median) %>% left_join(z_80) %>% left_join(z_90) %>% left_join(z_95) %>% left_join(z_max)

# select full density map or range-restricted map
#write_csv(z, "input/species_stats.csv")
write_csv(z, "input/species_stats_clipped.csv")


stophere = function() {
    x <- tibble(ecozone=as.character(),ecoprovince=as.numeric(),ecoregion=as.integer(), spp=as.character(), ncells=as.integer(), min=as.numeric(), mean=as.numeric(), median=as.numeric(), p60=as.numeric(), p90=as.numeric(), p95=as.numeric(), max=as.numeric(), std_dev=as.numeric())
    for (eco in sort(ecoregions)) {
        cat("Ecoregion",eco,"\n")
        # Extract ecoregion and assign value of 1
        r = trim(calc(reco, fun=function(x) {x[!x==eco]=NA; return(x/x)}))
        for (spp in birds) {
            cat("   ...",spp,"\n")
            flush.console()
            r1 = raster(paste0("data/songbirds/",spp,".tif"))
            ncells = ncell(r1[r1>-Inf & r1<Inf])
            if (ncells>0) {
                x = add_row(x, ecozone=pba$ecozone[pba$ecoregion==eco], ecoprovince=pba$ecoprovince[pba$ecoregion==eco], ecoregion=eco, spp=spp,
                    ncells = ncells,
                    min = round(cellStats(r1, min),3),
                    mean = round(cellStats(r1, mean),3),
                    median = round(cellStats(r1, median),3),
                    p80 = round(quantile(r1, probs=c(0.8)),3),
                    p90 = round(quantile(r1, probs=c(0.9)),3),
                    p95 = round(quantile(r1, probs=c(0.95)),3),
                    max = round(cellStats(r1, max),3),
                    std_dev = round(cellStats(r1, sd),3))
            } else {
                cat("      ... no data\n")
            }
        }
    }
    write_csv(x, paste0("input/species_stats.csv"))
}