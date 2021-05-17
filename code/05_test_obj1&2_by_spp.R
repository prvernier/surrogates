# Obj1: Are networks representative of surrogates also representative of non-target species?
#   Use representative networks only
# Obj2: Are representative networks more effective than non-representative networks for target species?
  # Use representative and non-representative networks
  # Use t-test or wilcox.test (t-test should be fine due to large sample sizes)
  # Calculate effect size = (mean of DMrep) – (mean of DMnonrep) / std dev of DM
  # https://easystats.github.io/effectsize/articles/interpret.html
# PV 2021-03-22

library(tidyverse)
library(broom)
library(effectsize)

x = read_csv('output/eco_bcr_data_by_spp.csv')

# Bird assemblages by BCR
gz1 = gather(x, species, dissim, allbirds:overwaternesters) %>% #, factor_key=TRUE)
    filter(!bcr=="BCR10") %>%
    select(bcr, rep, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    group_by(species,bcr) %>% 
    summarize(
        n_rep = sum(rep),
        KS_rep_mean = sprintf('%.2f', mean(dissim[rep==1])),
        KS_rep_sd = sprintf('%.2f', sd(dissim[rep==1])),
        KS_rep = paste0(sprintf('%.2f', mean(dissim[rep==1])),' (', sprintf('%.2f', sd(dissim[rep==1])),')'),
        KS_rep_pct = sprintf('%.0f', sum(rep[dissim<=0.2])/sum(rep)*100),
        n_nonrep = n() - sum(rep),
        KS_nonrep_mean = sprintf('%.2f', mean(dissim[rep==0])),
        KS_nonrep_sd = sprintf('%.2f', sd(dissim[rep==0])),
        KS_nonrep = paste0(sprintf('%.2f', mean(dissim[rep==0])),' (', sprintf('%.2f', sd(dissim[rep==0])),')'),
        d = paste0(sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d),' (', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_low), ', ', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_high),')'))
dz1 = as.data.frame(gz1)
dz1$id = rep(c(1,13,14,5,6,3,2,8,15,4,7,9,11,16,12,10),each=4)
z1 = tibble(dz1) %>%
    arrange(id) %>%
    mutate(Test_feature=species,
    Group=c('Conservation','','','','','','','','','','','','','','','','Habitat','','','','','','','','','','','','','','','','Migration','','','','','','','','','','','','','','','','Waterfowl','','','','','','','','','','','','','','',''),
    Test_feature=c('AllBirds','','','','ForestBirds','','','','DecliningBirds','','','','LowConcernBirds','','','','ConiferBirds','','','','DeciduousBirds','','','','MixedwoodBirds','','','','GrasslandBirds','','','','NeoMigrantBirds','','','','ShortMigrantBirds','','','','NomadicBirds','','','','ResidentBirds','','','','AllWaterfowl','','','','CavityNesters','','','','GroundNesters','','','','OverwaterNesters','','',''),
    species=NULL, id=NULL, BCR=bcr, bcr=NULL) %>%
    relocate(Group, Test_feature, BCR, n_rep, KS_rep_pct, KS_rep_mean, KS_rep_sd, KS_rep, n_nonrep, KS_nonrep_mean, KS_nonrep_sd, KS_nonrep, d)

# Caribou by BCR
eco_list = c(51,52,53,55,59,60,62,68,69,70,71,72,74,77,78,80,87,88,89,90,94,95,100,103,104,105,136,215,216,217)
gz2 = filter(x, ecoregion %in% eco_list) %>% 
    gather(species, dissim, caribou)  %>% 
    select(bcr, rep, species, dissim) %>% drop_na()
gz2 = gz2 %>% group_by(species,bcr) %>% 
    summarize(
        n_rep = sum(rep),
        KS_rep_mean = sprintf('%.2f', mean(dissim[rep==1])),
        KS_rep_sd = sprintf('%.2f', sd(dissim[rep==1])),
        KS_rep = paste0(sprintf('%.2f', mean(dissim[rep==1])),' (', sprintf('%.2f', sd(dissim[rep==1])),')'),
        KS_rep_pct = sprintf('%.0f', sum(rep[dissim<=0.2])/sum(rep)*100),
        n_nonrep = n() - sum(rep),
        KS_nonrep_mean = sprintf('%.2f', mean(dissim[rep==0])),
        KS_nonrep_sd = sprintf('%.2f', sd(dissim[rep==0])),
        KS_nonrep = paste0(sprintf('%.2f', mean(dissim[rep==0])),' (', sprintf('%.2f', sd(dissim[rep==0])),')'),
        d = paste0(sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d),' (', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_low), ', ', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_high),')'))

dz2 = as.data.frame(gz2)
z2 = tibble(dz2) %>%
    mutate(Test_feature=species,
    Group=c('Caribou','',''),
    Test_feature=c('Caribou','',''),
    species=NULL, id=NULL, BCR=bcr, bcr=NULL) %>%
    relocate(Group, Test_feature, BCR, n_rep, KS_rep_pct, KS_rep_mean, KS_rep_sd, KS_rep, n_nonrep, KS_nonrep_mean, KS_nonrep_sd, KS_nonrep, d)

# Bind avian and caribou tables and save
z12 = bind_rows(z2, z1)
write_csv(z12, 'output/table2_by_spp.csv')
