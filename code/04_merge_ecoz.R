# Prepare data for statistical analysis
# Merge ecozone-level data containing dissimilarity metrics for species and surrogates
# For each ecozone, randomly select up to 500 representative and non-representative networks
# PV 2021-02-16

library(tidyverse)
library(broom)

# First, bind ecozone-level tables and randomly select appropriate samples from rep and nonrep networks
set.seed(20191021)
for(i in c('4','5','6A','6B','9','11','12','14','15')) {
    x = read_csv(paste0('output/ecozones/ecoz_',i,'_nets_spp_ks.csv')) %>%
        mutate(ecozone=i)
    for (j in unique(x$ecoregion)) {
        xx = filter(x, ecoregion==j & (rep==0 | rep==1))
        nr = sum(xx$rep) # number of rep networks
        nnr = nrow(xx) - nr # number of non-rep networks
        if (nr > 500 & nnr > 500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,500,500)) %>% ungroup()
        } else if (nnr > 10*nr & nnr>500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,nr,500)) %>% ungroup()
        } else if (nnr > 10*nr & nnr<=500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,nr,10*nr)) %>% ungroup()
        } else if (nr > 10*nnr & nr>500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,500,nnr)) %>% ungroup()
        } else if (nr > 10*nnr & nr<=500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,10*nnr,nnr)) %>% ungroup()
        } else if (nnr >= nr & nnr>500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,nr,500)) %>% ungroup()
        } else if (nr > nnr & nr>500) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,500,nnr)) %>% ungroup()
        }
        if (j==unique(x$ecoregion[1])) {
            xxx = xx
        } else {
            xxx = bind_rows(xxx, xx)
        }
    }
    if (i=='4') {
        z = xxx
    } else {
        z = bind_rows(z, xxx)
    }
}
z = mutate(z, surrogates=(ks_cmi+ks_gpp+ks_led+bc_lcc)/4,
    #Networks = if_else(rep==1,"Rep","Non-rep"),
    ecozone = factor(ecozone, levels=c('4','5','6A','6B','9','11','12','14','15')),
    bcr = recode(ecozone, `4`="BCR6", `5`="BCR7", `6A`="BCR8", `6B`="BCR8",
        `9`="BCR6", `11`="BCR4", `12`="BCR4", `14`="BCR10", `15`="BCR7"))
y = read_csv('code/input/ecoregion_data.csv')
zout = left_join(z, y)
names(zout) = tolower(names(zout))
write_csv(zout, 'output/eco_bcr_data.csv')
