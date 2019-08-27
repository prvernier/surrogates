library(tidyverse)

birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')

x = read_csv("input/species_stats_clipped_cv.csv") %>% 
    mutate(keep=if_else(pct>=50,1,0)) %>% 
    filter(keep==1) %>%
    rename(ecoregion=zone)

y = read_csv("input/pan_eco_mdr_v4.csv") %>% 
    select(ecoregion, ecozone)

xy = left_join(x, y) %>%
    filter(!ecozone=="10") %>% 
    group_by(ecozone, species) %>% 
    mutate(wt=data/sum(data), wt_mean = weighted.mean(mean, wt)) %>%
    summarize(
        cv_mean = round(mean(mean),4),
        cv_wt_mean = round(mean(wt_mean),4))

z = read_csv("results/ecozone_models_1000.csv") %>%
    filter(species %in% toupper(birds)) %>%
    mutate(species = tolower(species))

xyz = right_join(xy, z) %>% arrange(species)

write_csv(xyz, "results/variation_by_ecozone.csv")

for (b in birds) {
    cat(b,xyz$ecoregions[xyz$species==b],":",round(cor(xyz$cv_mean[xyz$species==b], xyz$m1_r2[xyz$species==b]),3),round(cor(xyz$cv_wt_mean[xyz$species==b], xyz$m1_r2[xyz$species==b]),3),"\n")
}


# Create table to join to pba_ecozones.shp

long = select(xyz, ecozone, species, cv_wt_mean, m1_r2) %>%
    rename(cv=cv_wt_mean, r2=m1_r2) %>%
    mutate(cv=NULL, species=paste0(species,"_r2"))
r2_wide = spread(long, species, r2)
long = select(xyz, ecozone, species, cv_wt_mean, m1_r2) %>%
    rename(cv=cv_wt_mean, r2=m1_r2) %>%
    mutate(r2=NULL, species=paste0(species,"_cv"))
cv_wide = spread(long, species, cv)

wide = bind_cols(r2_wide, cv_wide)
write_csv(wide, "results/cv_r2_by_zone_wide.csv")

library(sf)
v = st_read("data/vector/pba_ecozones.shp")
v = right_join(v, wide)
st_write(v, "results/results2.shp")

