# Model network-level relationship between species KS and CMI + GPP + LED + LCC
# PV 2019-10-24

library(tidyverse)
#library(caret)

lmp <- function (modelobject) {
    # function to calculate overall p-value from a linear regression model
    # https://www.gettinggeneticsdone.com/2011/01/rstats-function-for-extracting-f-test-p.html
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

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
            rmse = as.numeric(),
            pval = as.numeric(),
            cmi_p = as.numeric(),
            gpp_p = as.numeric(),
            led_p = as.numeric(),
            lcc_p = as.numeric())
i = 1
for (species in spp_to_use) {
    cat(toupper(species),"...\n"); flush.console()
    flush.console()

    # individual species or groups of species
    if (species %in% songbirds) {
        eco_list = stats$ecoregion[stats$species==species & stats$pct_range>0]
    } else if (species=="caribou") {
        eco_list = c(51,52,53,55,59,60,62,68,69,70,71,72,74,77,78,80,87,88,89,90,94,95,100,103,104,105,136,215,216,217)
    } else {
        eco_list = unique(x$ecoregion)
    }

    for (eco in eco_list) {
        cat("...ecoregion",eco,"...\n"); flush.console()
        xx = filter(x, ecoregion==eco & (rep==0 | rep==1))
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

        if (sum(!is.na(xx[species])) > 2) {
            xxx = filter(xx, !is.na(xx[[species]]))
            spp = xxx[[species]]

            # Model species KS as a function of surrogates - all networks
            m1 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=xxx)
            co = summary(m1)$coefficients
            #vi = varImp(m1)

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
            if ("ks_cmi" %in% rownames(co)) {
                z[i,"cmi_coef"] = sprintf("%.3f",co["ks_cmi",1])
                z[i,"cmi_tstat"] = sprintf("%.2f",co["ks_cmi",3])
                z[i,"cmi_p"] = sprintf("%.4f",co["ks_cmi",4])
            } else {
                z[i,"cmi_coef"] = NA
                z[i,"cmi_tstat"] = NA
                z[i,"cmi_p"] = NA
            }
            if ("ks_gpp" %in% rownames(co)) {
                z[i,"gpp_coef"] = sprintf("%.3f",co["ks_gpp",1])
                z[i,"gpp_tstat"] = sprintf("%.2f",co["ks_gpp",3])
                z[i,"gpp_p"] = sprintf("%.4f",co["ks_gpp",4])
            } else {
                z[i,"gpp_coef"] = NA
                z[i,"gpp_tstat"] = NA
                z[i,"gpp_p"] = NA
            }
            if ("ks_led" %in% rownames(co)) {
                z[i,"led_coef"] = sprintf("%.3f",co["ks_led",1])
                z[i,"led_tstat"] = sprintf("%.2f",co["ks_led",3])
                z[i,"led_p"] = sprintf("%.4f",co["ks_led",4])
            } else {
                z[i,"led_coef"] = NA
                z[i,"led_tstat"] = NA
                z[i,"led_p"] = NA
            }
            if ("bc_lcc" %in% rownames(co)) {
                z[i,"lcc_coef"] = sprintf("%.3f",co["bc_lcc",1])
                z[i,"lcc_tstat"] = sprintf("%.2f",co["bc_lcc",3])
                z[i,"lcc_p"] = sprintf("%.4f",co["bc_lcc",4])
            } else {
                z[i,"lcc_coef"] = NA
                z[i,"lcc_tstat"] = NA
                z[i,"lcc_p"] = NA
            }
                z[i,"r2"] = sprintf("%.3f",summary(m1)$adj.r.squared)
                z[i,"rmse"] = sprintf("%.3f",modelr::rmse(m1,xxx))
                z[i,"pval"] = sprintf("%.4f",lmp(m1))

            i = i + 1
        }
    }
    #write_csv(z, paste0("output/tables/ecoregion_models_tmp.csv"))
}
# Write model results to csv file
write_csv(z, paste0("output/tables/ecoregion_models_rnr.csv"))
