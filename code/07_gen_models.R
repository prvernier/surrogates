# Model network-level relationship between species KS and CMI + GPP + LED + LCC
# PV 2019-10-22

library(tidyverse)
library(caret)

set.seed(20191021)
songbirds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
spp_to_use = c('caribou','allbirds','forestbirds','allwaterfowl','cavitynesters','groundnesters','overwaternesters',songbirds)
#x = read_csv("../output/ecozones/ecozones_networks_spp_1000.csv")
x = read_csv("output/tables/species_surrogates_ks_rnr.csv")
#zone = select(x, ecozone, ecoregion, intactness) %>% unique()
zone = dplyr::select(x, ecozone, ecoregion) %>% unique()
netstats = read_csv('code/input/ecoregion_statistics.csv')
ecoList = sort(netstats$ecoregion)
stats = read_csv("code/input/species_stats11.csv") %>% filter(ecoregion %in% ecoList)

# Create new tibble
z = tibble(ecozone=as.character(),
            ecoregion=as.character(),
            #intactness=as.numeric(),
            #networks=as.integer(), 
            nets_rep=as.integer(),
            nets_nonrep=as.integer(),
            species=as.character(), 
            density_mean=as.numeric(),
            density_cv=as.numeric(),
            cmi_coef=as.numeric(), 
            cmi_tstat=as.numeric(),
            gpp_coef=as.numeric(), 
            gpp_tstat=as.numeric(), 
            led_coef=as.numeric(), 
            led_tstat=as.numeric(), 
            lcc_coef=as.numeric(), 
            lcc_tstat=as.numeric(), 
            r2 = as.numeric(),
            rmse = as.numeric())
i = 1
for (species in spp_to_use) {
    cat(toupper(species),"...\n"); flush.console()
    flush.console()

    # individual species or groups of species
    if (species %in% songbirds) {
        eco_list = stats$ecoregion[stats$species==species & stats$pct_range>0]
    } else {
        eco_list = unique(x$ecoregion)
    }

    for (eco in eco_list) {
        cat("...ecoregion",eco,"...\n"); flush.console()
        xx = filter(x, ecoregion==eco & (rep==0 | rep==1))
        nr = sum(xx$rep) # number of rep networks
        nnr = nrow(xx) - nr # number of non-rep networks
        if (nnr >= nr) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,nr,nr)) %>% ungroup()
        } else if (nr > nnr) {
            xx = group_by(xx, rep) %>% sample_n(if_else(rep==1,nnr,nnr)) %>% ungroup()
        }

        if (sum(!is.na(xx[species])) > 2) {
            xxx = filter(xx, !is.na(xx[[species]]))
            spp = xxx[[species]]

            # Model species KS as a function of surrogates - all networks
            m1 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=xxx)
            vi = varImp(m1)

            z[i,"ecozone"] = substr(zone$ecozone[zone$ecoregion==eco],2,nchar(zone$ecozone[zone$ecoregion==eco]))
            z[i,"ecoregion"] = eco
            #z[i,"intactness"] = zone$intact_eco[zone$ecoregion==eco]
            rnet1 = length(xx$rep[xx$rep==1])
            rnet0 = length(xx$rep[xx$rep==0])
            #z[i,"networks"] = paste0("rep=",rnet1,", nrep=",rnet0)
            z[i,"nets_rep"] = rnet1
            z[i,"nets_nonrep"] = rnet0
            z[i,"species"] = species
            if (species %in% songbirds) {
                z[i,"density_mean"] = stats$mean[stats$ecoregion==eco & stats$species==species]
                z[i,"density_cv"] = stats$std_dev[stats$ecoregion==eco & stats$species==species]
            }
            z[i,"cmi_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["ks_cmi"])
            z[i,"cmi_tstat"] = sprintf("%.2f",vi["ks_cmi",1])
            z[i,"gpp_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["ks_gpp"])
            z[i,"gpp_tstat"] = sprintf("%.2f",vi["ks_gpp",1])
            z[i,"led_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["ks_led"])
            z[i,"led_tstat"] = sprintf("%.2f",vi["ks_led",1])
            z[i,"lcc_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["bc_lcc"])
            z[i,"lcc_tstat"] = sprintf("%.2f",vi["bc_lcc",1])
            z[i,"r2"] = sprintf("%.3f",summary(m1)$r.squared)
            z[i,"rmse"] = sprintf("%.3f",modelr::rmse(m1,xxx))

            i = i + 1
        }
    }
    #write_csv(z, paste0("output/tables/ecoregion_models_tmp.csv"))
}
# Write model results to csv file
write_csv(z, paste0("output/tables/ecoregion_models_rnr.csv"))

