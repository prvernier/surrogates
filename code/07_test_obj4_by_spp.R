# Obj4: Is the relationship between surrogates and non-target species influenced by other variables?
#   Test ecoregion membership, intactness, MDR size, density, and density CV
#   Calculate AIC for model with and without covariate
#   Use representative networks only
# PV 2021-05-26

library(tidyverse)
library(broom)

# Bird assemblages by BCR
x = read_csv('output/eco_bcr_data_by_spp.csv')
y1a = gather(x, species, density, c(allbirds_dens,forestbirds_dens,allwaterfowl_dens,cavitynesters_dens,groundnesters_dens,overwaternesters_dens,coniferbirds_dens,deciduousbirds_dens,mixedwoodbirds_dens,grasslandbirds_dens,neomigrantbirds_dens,shortmigrantbirds_dens,nomadicbirds_dens,residentbirds_dens,decliningbirds_dens,lowconcernbirds_dens)) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    mutate(id=1:227824) %>%
    select(id, density)
y1b = gather(x, species, dens_cv, c(allbirds_cv,forestbirds_cv,allwaterfowl_cv,cavitynesters_cv,groundnesters_cv,overwaternesters_cv,coniferbirds_cv,deciduousbirds_cv,mixedwoodbirds_cv,grasslandbirds_cv,neomigrantbirds_cv,shortmigrantbirds_cv,nomadicbirds_cv,residentbirds_cv,decliningbirds_cv,lowconcernbirds_cv)) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    mutate(id=1:227824) %>%
    select(id, dens_cv)
y1 = gather(x, species, dissim, c(allbirds,forestbirds,allwaterfowl,cavitynesters,groundnesters,overwaternesters,coniferbirds,deciduousbirds,mixedwoodbirds,grasslandbirds,neomigrantbirds,shortmigrantbirds,nomadicbirds,residentbirds,decliningbirds,lowconcernbirds)) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    select(bcr, ks_cmi, ks_gpp, ks_led, bc_lcc, species, dissim, mdr, intactness, ecoregion) %>%
    mutate(id=1:227824) %>%
    left_join(y1a, by="id") %>%
    left_join(y1b, by="id") %>%
    group_by(species, bcr) %>% mutate(id=NULL)
y2 = y1 %>%
    summarize(AIC0 = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc)),
        ecoregion = if_else(AIC0 < AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + factor(ecoregion))), 1, 0),
        mdr = if_else(AIC0 < AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + mdr)), 1, 0),
        intactness = if_else(AIC0 < AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + intactness)), 1, 0),
        density = if_else(AIC0 < AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + density)), 1, 0),
        dens_cv = if_else(AIC0 < AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + dens_cv)), 1, 0)) %>%
        mutate(AIC0=NULL)
write_csv(y2, 'output/obj4_bcr_birds_by_spp.csv')
b = read_csv('output/obj4_bcr_birds_by_spp.csv') %>%
    group_by(species) %>%
    summarize(Ecoregion=sum(ecoregion), MDR=sum(mdr), Intactness=sum(intactness), Density=sum(density), Density_CV=sum(dens_cv))
bb = mutate(b, id = c(1,13,14,5,6,3,2,8,15,4,7,9,11,16,12,10)) %>%
    arrange(id) %>% mutate(id=NULL)
write_csv(bb, 'output/table4_birds_by_spp.csv')

# Bird assemblages by BCR - full results
y2b = y1 %>%
    summarize(surrogates = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc)),
        ecoregion = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + factor(ecoregion))),
        mdr = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + mdr)),
        intactness = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + intactness)),
        density = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + density)),
        dens_cv = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + dens_cv)))
write_csv(y2b, 'output/obj4_bcr_birds_full_output_by_spp.csv')


# Caribou by BCR
eco_list = c(51,52,53,55,59,60,62,68,69,70,71,72,74,77,78,80,87,88,89,90,94,95,100,103,104,105,136,215,216,217)
y1a = filter(x, ecoregion %in% eco_list) %>%
    gather(species, density, caribou_dens) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    mutate(id=1:10510) %>%
    select(id, density)
y1b = filter(x, ecoregion %in% eco_list) %>%
    gather(species, dens_cv, caribou_cv) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    mutate(id=1:10510) %>%
    select(id, dens_cv)
y1 = filter(x, ecoregion %in% eco_list) %>% 
    gather(species, dissim, caribou) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    select(bcr, ks_cmi, ks_gpp, ks_led, bc_lcc, species, dissim, rep, mdr, intactness, ecoregion) %>%
    mutate(id=1:10510) %>%
    left_join(y1a, by="id") %>%
    left_join(y1b, by="id") %>%
    group_by(species, bcr) %>%
    drop_na()
y2 = y1 %>%
    summarize(AIC0 = AIC(lm(dissim ~ 1)),
        ecoregion = if_else(AIC0 < AIC(lm(dissim ~ 1 + factor(ecoregion))), 1, 0),
        mdr = if_else(AIC0 < AIC(lm(dissim ~ 1 + mdr)), 1, 0),
        intactness = if_else(AIC0 < AIC(lm(dissim ~ 1 + intactness)), 1, 0),
        density = if_else(AIC0 < AIC(lm(dissim ~ 1 + density)), 1, 0),
        dens_cv = if_else(AIC0 < AIC(lm(dissim ~ 1 + dens_cv)), 1, 0)) %>%
        mutate(AIC0=NULL)
write_csv(y2, 'output/obj4_bcr_caribou_by_spp.csv')

# Caribou by BCR - full results
y2b = y1 %>%
    summarize(surrogates = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc)),
        ecoregion = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + factor(ecoregion))),
        mdr = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + mdr)),
        intactness = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + intactness)),
        density = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + density)),
        dens_cv = AIC(lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc + dens_cv)))
write_csv(y2b, 'output/obj4_bcr_caribou_full_output_by_spp.csv')
