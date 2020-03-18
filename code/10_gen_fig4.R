# Create maps showing species' model R2 over range
# PV 2020-03-10

library(sf)
library(tmap)
library(tidyverse)

# Figure 1 - map of the number of songbirds models that are decent (adj-R2 >= 0.2)
songbirds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')

v = st_read("data/vector/pba_ecoregions.shp") %>%
    mutate(ecoregion=ecoreg) # %>% st_transform(4326)

z = read_csv(paste0('output/tables/models_r2_by_ecoregion_rnr.csv')) %>%
    select(ecoregion, songbirds) %>%
    mutate(present=0, decent=0)

for (i in songbirds) {
    z = mutate(z, present = if_else(is.na(z[[i]]), present, present+1),
        decent = if_else(!is.na(z[[i]]) & z[[i]]>=0.2, decent+1, decent),
        percent = round(decent/present,2))

}

ecor = left_join(v,z)
#nullr = mutate(ecor, NoData="")
#map = tm_shape(nullr) + tm_polygons("NoData", palette="#f0f0f0") +
map = tm_shape(ecor) + tm_polygons("percent", breaks=c(0,0.2,0.4,0.6,0.8,1), title="Proportion", textNA="No networks") + # palette="RdYlBu"
    tm_layout(legend.position = c("right", "top"))
    #tm_layout(title = "Songbirds with adj-R2 >= 0.2", title.position = c("center", "top"), legend.position = c("right", "top"))
tmap_save(map, filename="code/figs/songbirds_r2.png", height=1260, width=2100)
