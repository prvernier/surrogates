# Download cv maps and use them to evaluate R2 patterns
# Pierre Vernier
# 2019-07-25

bamDir = "https://bam-climate-change-predictions.s3-us-west-1.amazonaws.com/climatepredictions-ascii/"
birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
fda_raster = raster("data/raster/bnd.tif")

for (b in birds) {
    
    b = toupper(b)

    # Download and extract CV maps for 11 focal species
    download.file(paste0(bamDir,b,"_current.zip"), destfile = paste0("data/zip/",b,"_current.zip"))
    unzip(paste0("data/zip/",b,"_current.zip"), exdir = "data/zip")

    # Project and clip to boreal boundary
    r = raster(paste0("data/zip/",b,"_currcv.asc"))
    r = projectRaster(r, fda_raster)
    r = raster::mask(r, fda_raster) 
    
    # Clip to range boundary
    range_mask = raster(paste0('data/range_maps/',b,'.tif'))
    r2 = raster::mask(r, range_mask)
    writeRaster(r2, paste0("data/songbirds/",tolower(b),"_range_cv.tif"), overwrite=TRUE)

    # Cleanup
    #file.remove("data/zip/!readme.txt")
}

library(raster)
library(tidyverse)

birds = list.files("data/songbirds", pattern="tif")
birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
reco = raster("data/raster/ecoregions.tif")

rs = stack()
for (spp in birds) {
    #r = raster(paste0('data/songbirds/',spp,'.tif'))
    r = raster(paste0('data/songbirds/',spp,'_range_cv.tif'))
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

#write_csv(z, "input/species_stats.csv")
write_csv(z, "input/species_stats_clipped_cv.csv")


