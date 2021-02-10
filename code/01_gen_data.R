# Generate GIS database with common extent, projection & resolution
# PV 2020-08-24

library(sf)
library(raster)
library(tidyverse)

# Species folders and projection
dropDir = 'C:/Users/PIVER37/Dropbox (BEACONs)/'
songDir = paste0(dropDir,'gisdata/bam/densityProjections/')
fowlDir = paste0(dropDir,'gisdata/bam/waterfowl/density/')
prj = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
guilds = read_csv('input/guilds.csv')

# Original datasets
fda_shp = st_read('data/vector/pan_ecoregions_fda_v3.shp') %>% st_transform(crs=prj)
eco_shp = st_read('data/vector/pba_eco.shp') %>% st_transform(crs=prj)

# Create raster template of study area (ecoregions + fda) and ecoregions
raster_template = raster(extent(fda_shp), res=4000, crs=prj)
fda_raster = rasterize(fda_shp, raster_template, field=1)
writeRaster(fda_raster, "data/raster/bnd.tif", overwrite=TRUE)
# ecoregions need to have same dimensions as fda and species rasters
eco_raster = rasterize(eco_shp, raster_template, field="Ecoreg")
writeRaster(eco_raster, "data/raster/ecoregions.tif", overwrite=TRUE)

# Create large lakes mask
lakes = raster(paste0(dropDir,'BEACONs Share/surrogates/data/raster/large_lakes.tif'))
lakes = projectRaster(lakes, fda_raster)
lakes = mask(lakes, fda_raster)
writeRaster(lakes, "data/raster/lakes.tif", overwrite=TRUE)


# CARIBOU
caribou = raster(paste0(dropDir,'BEACONs Share/surrogates/data/raster/caribou.tif'))
caribou = projectRaster(caribou, fda_raster)
caribou = raster::mask(caribou, fda_raster)
writeRaster(caribou, paste0("data/caribou/caribou.tif"), overwrite=TRUE)


# WATERFOWL

# Extract waterfowl guilds and species to boreal ecozones/fdas
fowlList = list.files(fowlDir, pattern=".tif")
fowlList = substr(fowlList, 1, nchar(fowlList)-4)
for (spp in fowlList) {
    r = raster(paste0(fowlDir, spp, ".tif"))
    r = projectRaster(r, fda_raster)
    r = raster::mask(r, fda_raster)
    writeRaster(r, paste0("data/waterfowl/",spp,".tif"), overwrite=TRUE)
}


# SONGBIRDS

# Extract range boundaries for the 11 focal species
# NOTE: Swainson's Thrush uses scientific name "Catharus swainsoni" not "Catharus ustulatus" in range maps
range = st_read("C:/Users/PIVER37/Documents/gisdata/NorthAmerica/BOTW.gdb", "All_Species")
birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
genspp = c('Setophaga fusca','Poecile hudsonicus','Certhia americana','Setophaga virens','Cardellina canadensis','Setophaga tigrina','Contopus cooperi','Pinicola enucleator','Euphagus carolinus','Catharus swainsoni','Loxia leucoptera')
i = 1
for (b in birds) {
    cat(genspp[i], "...\n")
    flush.console()
    if (b=='btnw') { # for some reason this doesn't work with the filter approach so needed to do it in ArcGIS
        v = st_read('../data/range_maps/btnw_arcgis.shp') %>% st_transform(crs=prj)
    } else {
        v = filter(range, SCINAME==genspp[i]) %>% st_transform(crs=prj)
    }
    st_write(v, paste0('../data/range_maps/',b,'.shp'), delete_dsn=TRUE)
    r = rasterize(v, raster_template, field=1)
    r2 = raster::mask(r, fda_raster)
    writeRaster(r2, paste0('../data/range_maps/',b,'.tif'), overwrite=TRUE)
    i = i + 1
}

# Extract songbird rasters to boreal ecozones/fdas
songList = read_csv("input/bam_density_80_species.csv") %>% pull(Code) %>% tolower()
for (spp in songList) {
    cat("Processing", spp, "\n")
    flush.console()
    
    r = raster(paste0(songDir, spp, "_currmean.asc"))
    r = projectRaster(r, fda_raster)
    r = raster::mask(r, fda_raster) 
    
    # Mask out lakes and save to full extent of boreal
    r1 = raster::mask(r, lakes, inverse=TRUE)
    writeRaster(r1, paste0("../data/songbirds/",spp,".tif"), overwrite=TRUE)
    
    # Do not mask out lake and clip to range boundary
    if (spp %in% birds) {
        range_mask = raster(paste0('../data/range_maps/',spp,'.tif'))
        r2 = raster::mask(r, range_mask)
        writeRaster(r2, paste0("../data/songbirds_range/",spp,"_range.tif"), overwrite=TRUE)
    }
}

# Combine into all_birds and forest_birds

# all birds
#grids = list.files("data/songbirds", pattern="*.tif")
grids = paste0(songList,".tif")
s = raster::stack(paste0("../data/songbirds/", grids))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/AllBirds.tif", format="GTiff", overwrite=TRUE)

# forest birds (PV 2020-04-15: note that previously YBFL was wrongly included while YWAR was wrongly excluded)
fbGrids = filter(guilds, forest==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/ForestBirds.tif", format="GTiff", overwrite=TRUE)

# conifer guild
fbGrids = filter(guilds, conifer==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/ConiferBirds.tif", format="GTiff", overwrite=TRUE)

# deciduous guild
fbGrids = filter(guilds, deciduous==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/DeciduousBirds.tif", format="GTiff", overwrite=TRUE)

# mixedwood guild
fbGrids = filter(guilds, mixedwood==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/MixedwoodBirds.tif", format="GTiff", overwrite=TRUE)

# grassland guild
fbGrids = filter(guilds, grassland==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/GrasslandBirds.tif", format="GTiff", overwrite=TRUE)

# neotropical migrants guild
fbGrids = filter(guilds, neo_migrant==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/NeoMigrantBirds.tif", format="GTiff", overwrite=TRUE)

# short distance migrants guild
fbGrids = filter(guilds, short_migrant==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/ShortMigrantBirds.tif", format="GTiff", overwrite=TRUE)

# nomadic guild
fbGrids = filter(guilds, nomadic==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/NomadicBirds.tif", format="GTiff", overwrite=TRUE)

# resident guild
fbGrids = filter(guilds, resident==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/ResidentBirds.tif", format="GTiff", overwrite=TRUE)

# declining guild
fbGrids = filter(guilds, declining==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/DecliningBirds.tif", format="GTiff", overwrite=TRUE)

# low concern guild
fbGrids = filter(guilds, low_concern==1) %>% pull(Code)
s = raster::stack(paste0("../data/songbirds/", fbGrids,".tif"))
r = sum(s)
writeRaster(r, filename = "../data/songbirds/LowConcernBirds.tif", format="GTiff", overwrite=TRUE)
