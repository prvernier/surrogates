# Obj3: Does the relative importance of environmental surrogates vary by groups of species?
#   Use representative networks only
#   Model species DM as a function of CMI + GPP + LED + LCC
#   Count number of times each surrogate is the most important variable
# PV 2021-05-26

library(tidyverse)
library(broom)

x = read_csv('output/eco_bcr_data_by_spp.csv')

# Bird assemblages by BCR
y1a = gather(x, species, dissim, allbirds:overwaternesters) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    select(bcr, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    drop_na() %>%
    group_by(species, bcr) %>%
    do(tidy(lm(.$dissim ~ .$ks_cmi + .$ks_gpp + .$ks_led + .$bc_lcc))) %>%
    filter(!term=='(Intercept)') %>%
    top_n(4, statistic) %>% 
    arrange(desc(statistic), .by_group=TRUE) %>%
    #filter(p.value*nrow(gz1) <= 0.05) %>%
    mutate(top=order(desc(statistic)), surrogate=substr(term,6,9)) %>%
    select(species, bcr, surrogate, estimate, top) %>%
    ungroup() %>%
    select(species, surrogate, top) %>%
    filter(top==1) %>%
    group_by(species) %>%
    count(surrogate) %>%
    spread(surrogate, n, fill=0)
write_csv(y1a, 'output/obj3_bcr_birds_by_spp.csv')

# Bird assemblages by BCR - full results
y1b = gather(x, species, dissim, allbirds:overwaternesters) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    select(bcr, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    drop_na() %>%
    group_by(species, bcr) %>%
    do(tidy(lm(.$dissim ~ .$ks_cmi + .$ks_gpp + .$ks_led + .$bc_lcc))) %>%
    mutate(estimate=round(estimate,2),
        std.error=round(std.error,2),
        statistic=round(statistic,2),
        p.value=round(p.value,3),
        term=recode(term, 
        "(Intercept)"="Intcpt",
        ".$ks_cmi"="CMI",
        ".$ks_gpp"="GPP",
        ".$ks_led"="LED",
        ".$bc_lcc"="LCC")
)
write_csv(y1b, 'output/obj3_bcr_birds_full_output_by_spp.csv')

# Caribou by BCR
eco_list = c(51,52,53,55,59,60,62,68,69,70,71,72,74,77,78,80,87,88,89,90,94,95,100,103,104,105,136,215,216,217)
y2a = filter(x, ecoregion %in% eco_list) %>% 
    gather(species, dissim, caribou) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    select(bcr, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    drop_na() %>%
    group_by(species, bcr) %>%
    #summarize((lm(dissim ~ ks_cmi + ks_gpp + ks_led + bc_lcc))) %>%
    do(tidy(lm(.$dissim ~ .$ks_cmi + .$ks_gpp + .$ks_led + .$bc_lcc))) %>%
    filter(!term=='(Intercept)') %>%
    top_n(4, statistic) %>% 
    arrange(desc(statistic), .by_group=TRUE) %>%
    #filter(p.value*nrow(gz1) <= 0.05) %>%
    mutate(top=order(desc(statistic)), surrogate=substr(term,6,9)) %>%
    select(species, bcr, surrogate, estimate, top) %>%
    ungroup() %>%
    select(species, surrogate, top) %>%
    filter(top==1) %>%
    group_by(species) %>%
    count(surrogate) %>%
    spread(surrogate, n, fill=0)
write_csv(y2a, 'output/obj3_bcr_caribou_by_spp.csv')

# Caribou by BCR - full results
y2b = filter(x, ecoregion %in% eco_list) %>% 
    gather(species, dissim, caribou) %>%
    filter(!bcr=="BCR10" & rep==1) %>%
    select(bcr, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    drop_na() %>%
    group_by(species, bcr) %>%
    do(tidy(lm(.$dissim ~ .$ks_cmi + .$ks_gpp + .$ks_led + .$bc_lcc))) %>%
    mutate(estimate=round(estimate,2),
        std.error=round(std.error,2),
        statistic=round(statistic,2),
        p.value=round(p.value,3),
        term=recode(term, 
        "(Intercept)"="Intcpt",
        ".$ks_cmi"="CMI",
        ".$ks_gpp"="GPP",
        ".$ks_led"="LED",
        ".$bc_lcc"="LCC")
)
write_csv(y2b, 'output/obj3_bcr_caribou_full_output_by_spp.csv')
