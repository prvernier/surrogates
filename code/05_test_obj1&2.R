# Obj1: Are networks representative of surrogates also representative of non-target species?
#   Use representative networks only
# Obj2: Are representative networks more effective than non-representative networks for target species?
  # Use representative and non-representative networks
  # Use t-test or wilcox.test (t-test should be fine due to large sample sizes)
  # Calculate effect size = (mean of DMrep) – (mean of DMnonrep) / std dev of DM
# PV 2020-08-26

library(tidyverse)
library(broom)
library(effectsize)

x = read_csv('input/eco_bcr_data.csv')

# Bird assemblages by BCR
gz1 = gather(x, species, dissim, allbirds:overwaternesters) %>% #, factor_key=TRUE)
    filter(!bcr=="BCR10") %>%
    select(bcr, rep, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    group_by(species,bcr) %>% 
    summarize(
        n = n(),
        rep_mean = round(mean(dissim[rep==1]),2),
        is_rep_mean=if_else(rep_mean <= 0.2, 1, 0),
        nonrep_mean = round(mean(dissim[rep==0]),2),
        rep_median = round(median(dissim[rep==1]),2),
        is_rep_median=if_else(rep_median <= 0.2, 1, 0),
        nonrep_median = round(median(dissim[rep==0]),2),
        diff_size_mean = round(mean(dissim[rep==0]) - mean(dissim[rep==1]),2),
        diff_size_median = round(median(dissim[rep==0]) - median(dissim[rep==1]),2),
        effect_size_mean = round((mean(dissim[rep==0]) - mean(dissim[rep==1])) / sd_pooled(dissim[rep==0], dissim[rep==1]),1),
        effect_size_median = round((median(dissim[rep==0]) - median(dissim[rep==1])) / mad_pooled(dissim[rep==0], dissim[rep==1]),1),
        d = round(cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d,1),
        d.lci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_low,1),
        d.uci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_high,1),
        w.est = round(wilcox.test(dissim ~ rep, conf.int=T)$estimate,2),
        w.pval = round(wilcox.test(dissim ~ rep, conf.int=T)$p.value*64,4),
        w.lci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[1],2),
        w.uci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[2],2))
write_csv(gz1, 'output/obj1&2_bcr_birds.csv')

# Caribou by BCR
eco_list = c(51,52,53,55,59,60,62,68,69,70,71,72,74,77,78,80,87,88,89,90,94,95,100,103,104,105,136,215,216,217)
gz3 = filter(x, ecoregion %in% eco_list) %>% 
    gather(species, dissim, caribou)  %>% 
    select(bcr, rep, species, dissim) %>% drop_na()
gzz3 = gz3 %>% group_by(species,bcr) %>% 
    summarize(
        n = n(),
        rep_mean = round(mean(dissim[rep==1]),2),
        is_rep_mean=if_else(rep_mean <= 0.2, 1, 0),
        nonrep_mean = round(mean(dissim[rep==0]),2),
        rep_median = round(median(dissim[rep==1]),2),
        is_rep_median=if_else(rep_median <= 0.2, 1, 0),
        nonrep_median = round(median(dissim[rep==0]),2),
        diff_size_mean = round(mean(dissim[rep==0]) - mean(dissim[rep==1]),2),
        diff_size_median = round(median(dissim[rep==0]) - median(dissim[rep==1]),2),
        effect_size_mean = round((mean(dissim[rep==0]) - mean(dissim[rep==1])) / sd_pooled(dissim[rep==0], dissim[rep==1]),1),
        effect_size_median = round((median(dissim[rep==0]) - median(dissim[rep==1])) / mad_pooled(dissim[rep==0], dissim[rep==1]),1),
        d = round(cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d,1),
        d.lci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_low,1),
        d.uci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_high,1),
        w.est = round(wilcox.test(dissim ~ rep, conf.int=T)$estimate,2),
        w.pval = round(wilcox.test(dissim ~ rep, conf.int=T)$p.value*64,4),
        w.lci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[1],2),
        w.uci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[2],2))
write_csv(gzz3, 'output/obj1&2_bcr_caribou.csv')
