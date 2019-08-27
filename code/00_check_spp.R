# Check to make sure we have all the proper scientific names and make modifications where needed
library(sf)
library(tidyverse)

# uncomment to read (sloooow!)
#range_maps = st_read("../../gisdata/NorthAmerica/BOTW.gdb", "All_Species")
scinames = as.character(pull(range_maps, SCINAME))

spp_list = read_csv("input/bam_density_80_species.csv") %>% pull(Scientific_name)
for(spp in spp_list) {
    if (!spp %in% scinames) {
        cat(spp,"\n")
    }
}
