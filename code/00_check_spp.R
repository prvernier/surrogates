# Check to make sure we have all the proper scientific names and make modifications where needed
# Requires BOTW.gdb from http://datazone.birdlife.org/species/requestdis
# PV 2021-02-10

library(sf)
library(tidyverse)

# Change to local directory where BOTW.gdb is located
range_maps = st_read("C:/Users/PIVER37/Documents/gisdata/NorthAmerica/BOTW.gdb", "All_Species")
scinames = as.character(pull(range_maps, SCINAME))

spp_list = read_csv("code/input/bam_density_80_species.csv") %>% pull(Scientific_name)
n = 1
for(spp in spp_list) {
    if (!spp %in% scinames) {
        # should return nothing if names match
        cat(n,"-",spp,"\n")
    }
    n = n + 1
}
