# Check to make sure we have all the proper scientific names and make modifications where needed
# PV 2020-04-13

library(sf)
library(tidyverse)

# uncomment to read (sloooow!)
#range_maps = st_read("C:/Users/PIVER37/Documents/gisdata/NorthAmerica/BOTW.gdb", "All_Species")
#scinames = as.character(pull(range_maps, SCINAME))

spp_list = read_csv("input/bam_density_80_species.csv") %>% pull(Scientific_name)
n = 1
for(spp in spp_list) {
    if (spp %in% scinames) {
        # should return nothing if all is ok
        cat(n,"-",spp,"\n")
    }
    n = n + 1
}
