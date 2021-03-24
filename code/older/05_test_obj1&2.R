# Obj1: Are networks representative of surrogates also representative of non-target species?
#   Use representative networks only
# Obj2: Are representative networks more effective than non-representative networks for target species?
  # Use representative and non-representative networks
  # Use t-test or wilcox.test (t-test should be fine due to large sample sizes)
  # Calculate effect size = (mean of DMrep) – (mean of DMnonrep) / std dev of DM
  # https://easystats.github.io/effectsize/articles/interpret.html
# PV 2021-03-09

library(tidyverse)
library(broom)
library(effectsize)

x = read_csv('output/eco_bcr_data.csv')

# Bird assemblages by BCR
gz1 = gather(x, species, dissim, allbirds:overwaternesters) %>% #, factor_key=TRUE)
    filter(!bcr=="BCR10") %>%
    select(bcr, rep, species, dissim, ks_cmi, ks_gpp, ks_led, bc_lcc) %>%
    group_by(species,bcr) %>% 
    summarize(
        n = n(),
        rep_mean = round(mean(dissim[rep==1]),3),
        rep_sd = round(se(dissim[rep==1]),3),
        #rep_mean_sd = paste0(sprintf('%.3f', mean(dissim[rep==1])),' (', sprintf('%.3f', se(dissim[rep==1])),')'),
        rep_mean_ci = paste0(sprintf('%.3f', mean(dissim[rep==1])),' (', sprintf('%.3f', mean(dissim[rep==1]) - 1.96*se(dissim[rep==1])), ', ', sprintf('%.3f', mean(dissim[rep==1]) + 1.96*se(dissim[rep==1])),')'),
        #is_rep_mean=if_else(rep_mean <= 0.2, 1, 0),
        nonrep_mean = round(mean(dissim[rep==0]),3),
        #rep_median = round(median(dissim[rep==1]),3),
        #is_rep_median=if_else(rep_median <= 0.2, 1, 0),
        #nonrep_median = round(median(dissim[rep==0]),3),
        #diff_size_mean = round(mean(dissim[rep==0]) - mean(dissim[rep==1]),3),
        #diff_size_median = round(median(dissim[rep==0]) - median(dissim[rep==1]),3),
        #effect_size_mean = round((mean(dissim[rep==0]) - mean(dissim[rep==1])) / sd_pooled(dissim[rep==0], dissim[rep==1]),2),
        #effect_size_median = round((median(dissim[rep==0]) - median(dissim[rep==1])) / mad_pooled(dissim[rep==0], dissim[rep==1]),2),
        d = round(cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d,1),
        d.lci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_low,1),
        d.uci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_high,1),
        d_ci = paste0(sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d),' (', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_low), ', ', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_high),')'))
        #w.est = round(wilcox.test(dissim ~ rep, conf.int=T)$estimate,2),
        #w.pval = round(wilcox.test(dissim ~ rep, conf.int=T)$p.value*64,4),
        #w.lci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[1],2),
        #w.uci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[2],2))

#write_csv(gz1, 'output/obj1&2_bcr_birds.csv')
z1 = select(gz1, species, bcr, rep_mean_ci) %>%
    spread(bcr, rep_mean_ci)
z1 = as.data.frame(z1)
z1$id = c(1,13,14,5,6,3,2,8,15,4,7,9,11,16,12,10)
z1 = tibble(z1) %>%
    arrange(id) %>%
    mutate(id=NULL, Test_feature=species, Group=c('Conservation','','','','Habitat','','','','Migration','','','','Waterfowl','','',''), species=NULL) %>%
    relocate(Group, Test_feature, BCR4, BCR6, BCR7, BCR8)
#write_csv(z1, 'output/results_table2_birds.csv')
z11 = select(gz1, species, bcr, d_ci) %>%
    spread(bcr, d_ci)
z11 = as.data.frame(z11)
z11$id = c(1,13,14,5,6,3,2,8,15,4,7,9,11,16,12,10)
z11 = tibble(z11) %>%
    arrange(id) %>%
    mutate(id=NULL, Test_feature=species, Group=c('Conservation','','','','Habitat','','','','Migration','','','','Waterfowl','','',''), species=NULL) %>%
    relocate(Group, Test_feature, BCR4, BCR6, BCR7, BCR8)

# Caribou by BCR
eco_list = c(51,52,53,55,59,60,62,68,69,70,71,72,74,77,78,80,87,88,89,90,94,95,100,103,104,105,136,215,216,217)
gz3 = filter(x, ecoregion %in% eco_list) %>% 
    gather(species, dissim, caribou)  %>% 
    select(bcr, rep, species, dissim) %>% drop_na()
gzz3 = gz3 %>% group_by(species,bcr) %>% 
    summarize(
        n = n(),
        rep_mean = round(mean(dissim[rep==1]),3),
        rep_sd = round(se(dissim[rep==1]),3),
        #rep_mean_sd = paste0(sprintf('%.3f', mean(dissim[rep==1])),' (', sprintf('%.3f', se(dissim[rep==1])),')'),
        rep_mean_ci = paste0(sprintf('%.3f', mean(dissim[rep==1])),' (', sprintf('%.3f', mean(dissim[rep==1]) - 1.96*se(dissim[rep==1])), ', ', sprintf('%.3f', mean(dissim[rep==1]) + 1.96*se(dissim[rep==1])),')'),
        #is_rep_mean=if_else(rep_mean <= 0.2, 1, 0),
        nonrep_mean = round(mean(dissim[rep==0]),3),
        #rep_median = round(median(dissim[rep==1]),3),
        #is_rep_median=if_else(rep_median <= 0.2, 1, 0),
        #nonrep_median = round(median(dissim[rep==0]),3),
        #diff_size_mean = round(mean(dissim[rep==0]) - mean(dissim[rep==1]),3),
        #diff_size_median = round(median(dissim[rep==0]) - median(dissim[rep==1]),3),
        #effect_size_mean = round((mean(dissim[rep==0]) - mean(dissim[rep==1])) / sd_pooled(dissim[rep==0], dissim[rep==1]),2),
        #effect_size_median = round((median(dissim[rep==0]) - median(dissim[rep==1])) / mad_pooled(dissim[rep==0], dissim[rep==1]),2),
        d = round(cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d,2),
        d.lci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_low,2),
        d.uci = round(cohens_d(dissim[rep==0], dissim[rep==1])$CI_high,2),
        d_ci = paste0(sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$Cohens_d),' (', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_low), ', ', sprintf('%.2f', cohens_d(dissim[rep==0], dissim[rep==1])$CI_high),')'))
        #w.est = round(wilcox.test(dissim ~ rep, conf.int=T)$estimate,2),
        #w.pval = round(wilcox.test(dissim ~ rep, conf.int=T)$p.value*64,4),
        #w.lci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[1],2),
        #w.uci = round(wilcox.test(dissim ~ rep, conf.int=T)$conf.int[2],2))

#write_csv(gzz3, 'output/obj1&2_bcr_caribou.csv')
z3 = select(gzz3, species, bcr, rep_mean_ci) %>%
    spread(bcr, rep_mean_ci) %>%
    mutate(Test_feature=species, BCR4="N/A", Group="Caribou") %>%
    relocate(Group, Test_feature, BCR4, BCR6, BCR7, BCR8)
z3$species=NULL
z33 = select(gzz3, species, bcr, d_ci) %>%
    spread(bcr, d_ci) %>%
    mutate(Test_feature=species, BCR4="N/A", Group="Caribou") %>%
    relocate(Group, Test_feature, BCR4, BCR6, BCR7, BCR8)
z33$species=NULL

z1z3 = bind_rows(z3, z1)
z1z3$Test_feature=c('Caribou','AllBirds','ForestBirds','DecliningBirds','LowConcernBirds','ConiferBirds','DeciduousBirds','GrasslandBirds','MixedwoodBirds','NeoMigrantBirds','NomadicBirds','ResidentBirds','ShortMigrantBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters')
write_csv(z1z3, 'output/table2.csv')

z11z33 = bind_rows(z33, z11)
z11z33$Test_feature=c('Caribou','AllBirds','ForestBirds','DecliningBirds','LowConcernBirds','ConiferBirds','DeciduousBirds','GrasslandBirds','MixedwoodBirds','NeoMigrantBirds','NomadicBirds','ResidentBirds','ShortMigrantBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters')
write_csv(z11z33, 'output/table3.csv')
