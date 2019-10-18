# Prepare rep and nonrep shapefiles prior to calculating KS statistics
# PV 2019-10-17

library(sf)
library(tidyverse)

prj = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
x = read_csv('C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/revised_networks_sept2019/network_summary.csv')
eco_list = sort(x$ecoregion[x$use_in_analysis==1])
old_dir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/clusters/'
new_dir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/clusters2/'
marc_dir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/revised_networks_sept2019/networks/'

for (eco in eco_list) {
    flush.console()
    print(eco)

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
        print("...shapefile is missing!")
    }
    write_sf(r, paste0(new_dir,"eco_",eco,"_networks_rep.shp"))

    # non-representative networks
    if (file.exists(paste0(old_dir,"eco_",eco,"_networks_nonrep.shp"))) {
        nr = read_sf(paste0(old_dir,"eco_",eco,"_networks_nonrep.shp")) %>%
            select(network, ks_cmi, ks_gpp, ks_led, bc_lcc)
    } else {
        print("...shapefile is missing!")
    }
    write_sf(nr, paste0(new_dir,"eco_",eco,"_networks_nonrep.shp"))
}
