# Model network-level relationship between species KS and CMI + GPP + LED + LCC
# Pierre Vernier
# 2019-08-09

library(tidyverse)
library(caret)

pba_nwb_birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
spp_to_use = c('AllBirds','ForestBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters',pba_nwb_birds)
x = read_csv("../output/ecozones/ecozones_networks_spp_1000.csv")
stats = read_csv("code/input/species_stats_clipped.csv")
rnd = 1000

# Create new tibble
z = tibble(ecoregion=as.character(),
            networks=as.integer(), 
            species=as.character(), 
            cmi_coef=as.numeric(), 
            cmi_t=as.numeric(), 
            gpp_coef=as.numeric(), 
            gpp_t=as.numeric(), 
            led_coef=as.numeric(), 
            led_t=as.numeric(), 
            lcc_coef=as.numeric(), 
            lcc_t=as.numeric(), 
            m1_r2 = as.numeric(),
            m1_rmse = as.numeric())
i = 1
for (eco in unique(x$ecoregion)) {
    cat("Ecoregion",eco,"...\n"); flush.console()
    xx = filter(x, ecoregion==eco)

    for (species in spp_to_use) {
        
        #if (sum(!is.na(xx[species]) & xx[[paste0(species,"_eco_pct")]]>=50 & xx[[paste0(species,"_net_pct")]]>=50) > 2) {
        if (sum(!is.na(xx[species]) & xx[[paste0(species,"_eco_pct")]]>=50 & xx[[paste0(species,"_net_pct")]]>=50) > 2) {
            cat("...",species,"...\n"); flush.console()
            flush.console()
            xxx = filter(xx, !is.na(xx[[species]]))
            spp = xxx[[species]]

            # Model species KS as a function of surrogates - all networks
            m1 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=xxx)
            vi = varImp(m1)

            z[i,"ecoregion"] = substr(eco,2,nchar(eco))
            z[i,"networks"] = nrow(xxx)
            z[i,"species"] = toupper(species)
            z[i,"cmi_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["ks_cmi"])
            z[i,"cmi_t"] = sprintf("%.2f",vi[1,1])
            z[i,"gpp_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["ks_gpp"])
            z[i,"gpp_t"] = sprintf("%.2f",vi[2,1])
            z[i,"led_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["ks_led"])
            z[i,"led_t"] = sprintf("%.2f",vi[3,1])
            z[i,"lcc_coef"] = sprintf("%.3f",summary(m1)$coefficients[,1]["bc_lcc"])
            z[i,"lcc_t"] = sprintf("%.2f",vi[4,1])
            z[i,"m1_r2"] = sprintf("%.3f",summary(m1)$r.squared)
            z[i,"m1_rmse"] = sprintf("%.3f",modelr::rmse(m1,xxx))

            i = i + 1
        }
    }
    
}
# Write model results to csv file
write_csv(z, paste0("../output/tables/ecozone_models_",rnd,"_ecoregions.csv"))

