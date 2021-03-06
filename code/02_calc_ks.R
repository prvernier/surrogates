# Calculate KS for caribou and species groups in rep and nonrep networks
# PV 2021-02-16

library(sf)
library(raster)
library(tidyverse)
library(exactextractr)

#memory.limit(320000)
prj = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#dropDir = 'C:/Users/PIVER37/Dropbox (BEACONs)/'
sppList = c('Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters')
fda_shp = read_sf('data/vector/pan_ecoregions_fda_v3.shp') %>% st_transform(crs=prj)
netstats = read_csv('code/input/ecoregion_statistics.csv')
ecoList = sort(netstats$ecoregion)

for (eco in ecoList) {
    cat("Ecoregion",eco,"...\n")
    flush.console()

    #netDir = paste0(dropDir,'BEACONs Share/manuscripts/surrogates/data/networks/clusters2/')
    netDir = 'data/input/networks/'
    fda = filter(fda_shp, Ecoregion==eco)
    v_nets_rep = paste0(netDir,"eco_",eco,"_networks_rep.shp")
    v_nets_nonrep = paste0(netDir,"eco_",eco,"_networks_nonrep.shp")

    if (file.exists(v_nets_rep) & file.exists(v_nets_nonrep)) {
        
        # Read network shapefile and extract required attributes to a tibble
        v_eco = read_sf("data/vector/pan_eco_mdr_v4.shp") %>% st_transform(crs=prj) %>% filter(Ecoreg==eco)
        v_fda = read_sf("data/vector/pan_ecoregions_fda_v3.shp") %>% st_transform(crs=prj) %>% filter(Ecoregion==eco)

        nets_rep = read_sf(v_nets_rep) %>% st_transform(crs=prj)
        nets_nonrep = read_sf(v_nets_nonrep) %>% st_transform(crs=prj)
        x_rep = nets_rep %>% st_set_geometry(NULL)
        x_nonrep = nets_nonrep %>% st_set_geometry(NULL)

        # Create raster stack
        fda_stack = eco_stack = stack()
        for (spp in sppList) {
            if (spp=="Caribou") {
                r = raster(paste0('data/raster/caribou.tif'))
            } else if (spp %in% c('AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds')) {
                r = raster(paste0('data/songbirds_groups/',spp,".tif"))
            } else {
                r = raster(paste0('data/waterfowl/',spp,".tif"))
            }
            r_eco <- raster::mask(crop(r, extent(v_eco)), v_eco)
            eco_stack = addLayer(eco_stack, r_eco)
            r_fda <- raster::mask(crop(r, extent(v_fda)), v_fda)
            fda_stack = addLayer(fda_stack, r_fda)
        }
        eco_vals = values(eco_stack)
        
        # Extract raster stack values using REPRESENTATIVE networks
        cat("...representative networks\n"); flush.console()
        nets_rep = dplyr::select(nets_rep, network)
        v = exact_extract(fda_stack, nets_rep)
        
        # calculate species KS for random networks
        for (j in 1:length(sppList)) {
            x1 = eco_vals[,j][!is.na(eco_vals[,j])] # non-NA ecoregion raster values
            for (i in 1:nrow(nets_rep)) {
                x2 = v[[i]][,j][!is.na(v[[i]][,j])] # non-NA network raster values
                if (length(x1)>0 & length(x2)>0) {
                    x_rep[i,sppList[j]] = round(ks.test(x1, x2)$statistic,3)
                } else {
                    x_rep[i,sppList[j]] = NaN
                }
            }   
        }

        # Extract raster stack values using NON-REPRESENTATIVE networks
        cat("...non-representative networks\n"); flush.console()
        v = exact_extract(fda_stack, nets_nonrep)
        
        # calculate species KS for random networks
        for (j in 1:length(sppList)) {
            x1 = eco_vals[,j][!is.na(eco_vals[,j])] # non-NA ecoregion raster values
            for (i in 1:nrow(nets_nonrep)) {
                x2 = v[[i]][,j][!is.na(v[[i]][,j])] # non-NA network raster values
                if (length(x1)>0 & length(x2)>0) {
                    x_nonrep[i,sppList[j]] = round(ks.test(x1, x2)$statistic,3)
                } else {
                    x_nonrep[i,sppList[j]] = NaN
                }
            }   
        }
        
        x_rep = mutate(x_rep, rep=1)
        x_nonrep = mutate(x_nonrep, rep=0)
        x = bind_rows(x_rep, x_nonrep)
        write_csv(x, paste0("output/ecoregions/eco_",eco,"_nets_spp_ks.csv"))
    }
}
