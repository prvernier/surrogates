# Test models with additional factors

library(tidyverse)

x = read_csv("output/ecozones/ecozones_networks_spp_1000.csv") %>%
	mutate(ecoregion = as.factor(ecoregion), ecozone = as.factor(ecozone))

birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
species = c('AllBirds','ForestBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters',birds)
for (i in species) {
    if (i %in% birds) {
        xx = filter(x, x[[paste0(i,"_net_pct")]]>=50 & x[[paste0(i,"_eco_pct")]]>=50)
        spp_cv = pull(xx, paste0(i,"_cv"))
        spp = pull(xx, i)
        m1 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=xx)
        m2 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc + spp_cv, data=xx)
        m3 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc + ecoregion, data=xx)
        cat(toupper(i),": M1=",sprintf("%0.3f",summary(m1)$r.squared), ", M2=", sprintf("%0.3f",summary(m2)$r.squared),", M3=", sprintf("%0.3f",summary(m3)$r.squared),"\n",sep="")
        flush.console()
    } else {
        xx = x
        spp = pull(xx, i)
        m1 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=xx)
        #m2 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc + spp_cv, data=xx)
        m3 = lm(spp ~ ks_cmi + ks_gpp + ks_led + bc_lcc + ecoregion, data=xx)
        cat(toupper(i),": M1=",sprintf("%0.3f",summary(m1)$r.squared), ", M3=", sprintf("%0.3f",summary(m3)$r.squared),"\n",sep="")
        flush.console()
    }
}
