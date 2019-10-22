# Prepare rep and nonrep shapefiles prior to calculating KS statistics
# PV 2019-10-21

library(sf)
library(tidyverse)

prj = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
old_dir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/clusters/'
new_dir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/clusters2/'
marc_dir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/revised_networks_sept2019/networks/'
#netstats = read_csv('C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/revised_networks_sept2019/network_summary.csv') %>%
#    filter(use_in_analysis==1) %>% 
#    select(ecozone, ecoprovince, ecoregion, v1_rep_gd10km, v1_nonrep_gd10km, v2_rep_gd10km, v2_nonrep_gd10km) %>%
#    mutate(v2_rep_gd10km=if_else(is.na(v2_rep_gd10km), 0, v2_rep_gd10km), v2_nonrep_gd10km=if_else(is.na(v2_nonrep_gd10km), 0, v2_nonrep_gd10km)) %>%
#    mutate(rep=if_else(v2_rep_gd10km>v1_rep_gd10km, v2_rep_gd10km, v1_rep_gd10km)) %>%
#    mutate(nonrep=if_else(v2_nonrep_gd10km>v1_nonrep_gd10km, v2_nonrep_gd10km, v1_nonrep_gd10km)) %>%
#    #mutate(rep_to_use=if_else(rep>nonrep, nonrep, rep)) %>%
#    #mutate(nonrep_to_use=if_else(v2_nonrep_gd10km>v1_nonrep_gd10km, v2_nonrep_gd10km, v1_nonrep_gd10km)) %>%
#    arrange(ecoregion) %>%
#    write_csv("code/input/network_summary.csv")
#eco_list = sort(netstats$ecoregion)
ecostats = read_csv('output/tables/ecoregion_statistics.csv')
eco_list = sort(ecostats$ecoregion)

for (eco in eco_list) {
    flush.console()
    print(paste("Ecoregion",eco))
    r_zero = 0
    nr_zero = 0

    # representative networks
    pb2 = paste0(marc_dir,"eco_",eco,"_networks_i80_t100_pb2_pas0_representative_filtered.shp")
    pb3 = paste0(marc_dir,"eco_",eco,"_networks_i80_t100_pb3_pas0_representative_filtered.shp")
    pb4 = paste0(marc_dir,"eco_",eco,"_networks_i80_t100_pb4_pas0_representative_filtered.shp")
    if (file.exists(pb2)) {
        r = read_sf(pb2) %>% st_transform(crs=prj) %>%
            mutate(network=netName, ks_cmi=ks_net_cmi, ks_gpp=ks_net_gpp, ks_led=ks_net_led, bc_lcc=bc_net_lcc) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else if (file.exists(pb3)) {
        r = read_sf(pb3) %>% st_transform(crs=prj) %>%
            mutate(network=netName, ks_cmi=ks_net_cmi, ks_gpp=ks_net_gpp, ks_led=ks_net_led, bc_lcc=bc_net_lcc) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else if (file.exists(pb4)) {
        r = read_sf(pb4) %>% st_transform(crs=prj) %>%
            mutate(network=netName, ks_cmi=ks_net_cmi, ks_gpp=ks_net_gpp, ks_led=ks_net_led, bc_lcc=bc_net_lcc) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else if (file.exists(paste0(old_dir,"eco_",eco,"_networks_rep.shp"))) {
        r = read_sf(paste0(old_dir,"eco_",eco,"_networks_rep.shp")) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else {
        print("...rep networks missing!")
        r_zero = 1
    }

    # non-representative networks
    pb2 = paste0(marc_dir,"eco_",eco,"_networks_i80_t100_pb2_pas0_non_representative_filtered.shp")
    pb3 = paste0(marc_dir,"eco_",eco,"_networks_i80_t100_pb3_pas0_non_representative_filtered.shp")
    pb4 = paste0(marc_dir,"eco_",eco,"_networks_i80_t100_pb4_pas0_non_representative_filtered.shp")
    if (file.exists(pb2)) {
        nr = read_sf(pb2) %>% st_transform(crs=prj) %>%
            mutate(network=netName, ks_cmi=ks_net_cmi, ks_gpp=ks_net_gpp, ks_led=ks_net_led, bc_lcc=bc_net_lcc) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else if (file.exists(pb3)) {
        nr = read_sf(pb3) %>% st_transform(crs=prj) %>%
            mutate(network=netName, ks_cmi=ks_net_cmi, ks_gpp=ks_net_gpp, ks_led=ks_net_led, bc_lcc=bc_net_lcc) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else if (file.exists(pb4)) {
        nr = read_sf(pb4) %>% st_transform(crs=prj) %>%
            mutate(network=netName, ks_cmi=ks_net_cmi, ks_gpp=ks_net_gpp, ks_led=ks_net_led, bc_lcc=bc_net_lcc) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else if (file.exists(paste0(old_dir,"eco_",eco,"_networks_nonrep.shp"))) {
        nr = read_sf(paste0(old_dir,"eco_",eco,"_networks_nonrep.shp")) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else {
        print("...nonrep networks missing!")
        nr_zero = 1
    }

    # Write new shapefile and update ecoregion_statistics.csv file
    if (r_zero==1 & nr_zero==1) {
        ecostats$nets_rep[x$ecoregion==eco] = 0
        ecostats$nets_nonrep[x$ecoregion==eco] = 0
    } else if (r_zero==0 & nr_zero==0) {
        write_sf(r, paste0(new_dir,"eco_",eco,"_networks_rep.shp"))
        write_sf(nr, paste0(new_dir,"eco_",eco,"_networks_nonrep.shp"))
        ecostats$nets_rep[x$ecoregion==eco] = nrow(r)
        ecostats$nets_nonrep[x$ecoregion==eco] = nrow(nr)
    } else {
        print("...not sure what happened!")
    }
}

ecostats = mutate(networks = if_else(nets_rep < nets_nonrep, nets_rep, nets_nonrep)
write_csv(ecostats, 'code/input/ecoregion_statistics.csv')
