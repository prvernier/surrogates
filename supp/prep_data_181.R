# Test representation analysis (BC Albers projection)
# PV 2021-04-24

library(sf)
library(raster)
library(tidyverse)
library(fasterize)

fda = st_read('eco181/fda.shp', quiet=T) %>% mutate(one=1)

################################################################################
# Prepare 1000m rasters
################################################################################

bnd1 = fasterize(sf=fda, raster=raster(fda, res=1000), field="one")

# List of rasters to use and directory name
raster1000 = c('cmi','gpp','led','lcc')
dir1000 = '../gisdata/canada/raster1000/'
# Generate data for study region
for (i in raster1000) {
    print(i); flush.console()
    r = raster(paste0(dir1000,i,'.tif'))
    if (i %in% c('lcc','dhi14','dhi40')) {
        r = projectRaster(from=r, to=bnd1, res=1000, crs=crs(bnd1), method='ngb')
    } else {
        r = projectRaster(from=r, to=bnd1, res=1000, crs=crs(bnd1), method='bilinear')
    }
    r = mask(r, bnd1)
    writeRaster(r, paste0('data/eco181/', i, '.tif'), overwrite=T)
}


################################################################################
# Prepare 4000m rasters (case study: change to extract directly - see canada.R)
################################################################################

bnd4 = fasterize(sf=fda, raster=raster(fda, res=4000), field="one")

# Bird groups (canada albers)
guilds = list.files('../github/surrogates/data/songbirds_groups/')
guilds = substr(guilds, 1, nchar(guilds)-4)
for (i in guilds) {
    print(i); flush.console()
    r <- raster(paste0('../github/surrogates/data/songbirds_groups/',i,'.tif'))
    r <- projectRaster(from=r, to=bnd4, res=4000, crs=crs(bnd4), method='bilinear')
    r <- mask(r, bnd4)
    writeRaster(r, paste0('data/eco181/',i,'.tif'), overwrite=T)
}

# Birds from case study (canada albers)
#for (i in c('boch','cmwa','rubl')) {
#    r <- raster(paste0('../../../github/surrogates/data/songbirds/',i,'.tif'))
#    r <- projectRaster(from=r, to=bnd4, res=4000, crs=crs(bnd4), method='bilinear')
#    r <- mask(r, bnd4)
#    writeRaster(r, paste0('eco181/',i,'.tif'), overwrite=T)
#}
