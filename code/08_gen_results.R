# Create tables and figures for results section of manuscript
# PV 2019-09-24

library(tidyverse)

# ECOREGIONS

z = read_csv(paste0('output/tables/ecoregion_models_rnr.csv'))


# Summarize coefficients
x1 = group_by(z, species) %>% summarize(
        n_eco = sum(n()),
        cmi = paste0(sprintf("%.2f",mean(cmi_coef))," (",sprintf("%.2f",min(cmi_coef)),"-",sprintf("%.2f",max(cmi_coef)),")"),
        gpp = paste0(sprintf("%.2f",mean(gpp_coef))," (",sprintf("%.2f",min(gpp_coef)),"-",sprintf("%.2f",max(gpp_coef)),")"),
        led = paste0(sprintf("%.2f",mean(led_coef))," (",sprintf("%.2f",min(led_coef)),"-",sprintf("%.2f",max(led_coef)),")"),
        lcc = paste0(sprintf("%.2f",mean(lcc_coef))," (",sprintf("%.2f",min(lcc_coef)),"-",sprintf("%.2f",max(lcc_coef)),")"),
        r2 = paste0(sprintf("%.2f",mean(r2))," (",sprintf("%.2f",min(r2)),"-",sprintf("%.2f",max(r2)),")"),
        rmse = paste0(sprintf("%.2f",mean(rmse))," (",sprintf("%.2f",min(rmse)),"-",sprintf("%.2f",max(rmse)),")")) %>%
    mutate(rank=case_when(
        species=="allbirds" ~ 1,
        species=="forestbirds" ~ 2,
        species=="allwaterfowl" ~ 3,
        species=="cavitynesters" ~ 4,
        species=="groundnesters" ~ 5,
        species=="overwaternesters" ~ 6,
        species=="blbw" ~ 7,
        species=="boch" ~ 8,
        species=="brcr" ~ 9,
        species=="btnw" ~ 10,
        species=="cawa" ~ 11,
        species=="cmwa" ~ 12,
        species=="osfl" ~ 13,
        species=="pigr" ~ 14,
        species=="rubl" ~ 15,
        species=="swth" ~ 16,
        species=="wwcr" ~ 17,
        species=="caribou" ~ 18,
        TRUE ~ 0)) %>%
     arrange(rank) %>% mutate(rank=NULL)
write_csv(x1, paste0('output/tables/models_range_of_values_rnr.csv'))


# Summarize R2 by species x ecoregion
x2 = dplyr::select(z, ecozone, ecoregion, species, r2) %>% spread(species, r2) %>% arrange(ecoregion) %>%
    dplyr::select(ecozone, ecoregion, caribou, blbw, boch, brcr, btnw, cawa, cmwa, osfl, pigr, rubl, swth, wwcr, allbirds, forestbirds, allwaterfowl, cavitynesters, groundnesters, overwaternesters)
write_csv(x2, paste0('output/tables/models_r2_by_ecoregion_rnr.csv'))

# Summarize RMSE by species x ecoregion
x2 = dplyr::select(z, ecozone, ecoregion, species, rmse) %>% spread(species, rmse) %>% arrange(ecoregion) %>%
    dplyr::select(ecozone, ecoregion, caribou, blbw, boch, brcr, btnw, cawa, cmwa, osfl, pigr, rubl, swth, wwcr, allbirds, forestbirds, allwaterfowl, cavitynesters, groundnesters, overwaternesters)
write_csv(x2, paste0('output/tables/models_rmse_by_ecoregion_rnr.csv'))

# Summarize R2 and RMSE by species x ecoregion
#x2a = dplyr::select(z, ecoregion, species, r2) %>% spread(ecoregion, r2) %>% mutate(metric="R2", id=as.integer(seq(1,36,2)))
#x2b = dplyr::select(z, ecoregion, species, rmse) %>% spread(ecoregion, rmse) %>% mutate(metric="RMSE", id=as.integer(seq(2,37,2)))
#x2 = bind_rows(x2a, x2b) %>% arrange(id)
#names(x2) = c('Species', 'Metric', paste0("Ecor",unique(z$ecoregion)),"id")
#x2 = mutate(x2, id=NULL)
#write_csv(x2, paste0('output/tables/models_r2rmse_by_ecoregion_rnr.csv'))


# Summarize variable importance by species x ecoregion
#x2a = dplyr::select(z, ecoregion, species, cmi_tstat) %>% spread(ecoregion, cmi_tstat) %>% mutate(surrogate="CMI", id=as.integer(seq(1,72,4)))
#x2b = dplyr::select(z, ecoregion, species, gpp_tstat) %>% spread(ecoregion, gpp_tstat) %>% mutate(surrogate="GPP", id=as.integer(seq(2,73,4)))
#x2c = dplyr::select(z, ecoregion, species, led_tstat) %>% spread(ecoregion, led_tstat) %>% mutate(surrogate="LED", id=as.integer(seq(3,74,4)))
#x2d = dplyr::select(z, ecoregion, species, lcc_tstat) %>% spread(ecoregion, lcc_tstat) %>% mutate(surrogate="LCC", id=as.integer(seq(4,75,4)))
#x2 = bind_rows(x2a, x2b, x2c, x2d) %>% arrange(id)
#names(x2) = c('Species', 'Metric', paste0("Ecor",unique(z$ecoregion)),"id")
#x2 = mutate(x2, id=NULL)
#write_csv(x2, paste0('output/tables/surrogate_importance_rnr.csv'))


# Summarize variable importance - second try
x3 = dplyr::select(z, ecoregion, species, cmi_tstat, gpp_tstat, led_tstat, lcc_tstat) %>%
    mutate(cmi=as.integer(0),gpp=as.integer(0),led=as.integer(0),lcc=as.integer(0)) %>%
    mutate(cmi_tstat=if_else(is.na(cmi_tstat),0,abs(cmi_tstat))) %>%
    mutate(gpp_tstat=if_else(is.na(gpp_tstat),0,abs(gpp_tstat))) %>%
    mutate(led_tstat=if_else(is.na(led_tstat),0,abs(led_tstat))) %>%
    mutate(lcc_tstat=if_else(is.na(lcc_tstat),0,abs(lcc_tstat)))
for (i in 1:nrow(x3)) {
    print(i)
    if (max(x3[i,"cmi_tstat"],x3[i,"gpp_tstat"],x3[i,"led_tstat"],x3[i,"lcc_tstat"])==x3[i,"cmi_tstat"]) {
        x3[i,"cmi"]=1
    } else if (max(x3[i,"cmi_tstat"],x3[i,"gpp_tstat"],x3[i,"led_tstat"],x3[i,"lcc_tstat"])==x3[i,"gpp_tstat"]) {
        x3[i,"gpp"]=1
    } else if (max(x3[i,"cmi_tstat"],x3[i,"gpp_tstat"],x3[i,"led_tstat"],x3[i,"lcc_tstat"])==x3[i,"led_tstat"]) {
        x3[i,"led"]=1
    } else if (max(x3[i,"cmi_tstat"],x3[i,"gpp_tstat"],x3[i,"led_tstat"],x3[i,"lcc_tstat"])==x3[i,"lcc_tstat"]) {
        x3[i,"lcc"]=1
    }
}
write_csv(x3, paste0('output/tables/surrogate_importance_counts.csv'))


# Sum variable importance by species
x = group_by(z, species) %>% #mutate(one=1) %>% summarize(eco=sum(one))
    mutate(cmi_p=if_else(cmi_p <=0.05, 1, 0),gpp_p=if_else(gpp_p <=0.05, 1, 0),led_p=if_else(led_p <=0.05, 1, 0),lcc_p=if_else(lcc_p <=0.05, 1, 0)) %>%
    summarize(cmi=sum(cmi_p,na.rm=T), gpp=sum(gpp_p,na.rm=T), led=sum(led_p,na.rm=T), lcc=sum(lcc_p,na.rm=T)) %>%
    mutate(rank=case_when(
        species=="allbirds" ~ 13,
        species=="forestbirds" ~ 12,
        species=="allwaterfowl" ~ 18,
        species=="cavitynesters" ~ 15,
        species=="groundnesters" ~ 16,
        species=="overwaternesters" ~ 17,
        species=="blbw" ~ 1,
        species=="boch" ~ 2,
        species=="brcr" ~ 3,
        species=="btnw" ~ 4,
        species=="cawa" ~ 5,
        species=="cmwa" ~ 6,
        species=="osfl" ~ 7,
        species=="pigr" ~ 8,
        species=="rubl" ~ 9,
        species=="swth" ~ 10,
        species=="wwcr" ~ 11,
        TRUE ~ 0)) %>%
     arrange(rank) %>% mutate(rank=NULL)
write_csv(x, paste0('output/tables/surrogate_significance_counts_sum.csv'))

# Sum number of times coefficients are significant
x = read_csv('output/tables/surrogate_importance_counts.csv') %>%
    group_by(species) %>%
    summarize(cmi=sum(cmi), gpp=sum(gpp), led=sum(led), lcc=sum(lcc)) %>%
    mutate(rank=case_when(
        species=="allbirds" ~ 13,
        species=="forestbirds" ~ 12,
        species=="allwaterfowl" ~ 18,
        species=="cavitynesters" ~ 15,
        species=="groundnesters" ~ 16,
        species=="overwaternesters" ~ 17,
        species=="blbw" ~ 1,
        species=="boch" ~ 2,
        species=="brcr" ~ 3,
        species=="btnw" ~ 4,
        species=="cawa" ~ 5,
        species=="cmwa" ~ 6,
        species=="osfl" ~ 7,
        species=="pigr" ~ 8,
        species=="rubl" ~ 9,
        species=="swth" ~ 10,
        species=="wwcr" ~ 11,
        TRUE ~ 0)) %>%
     arrange(rank) %>% mutate(rank=NULL)
write_csv(x, paste0('output/tables/surrogate_importance_counts_sum.csv'))


# Format ecoregion_models_rnr.csv for supp_info
zz = mutate(z,
    Ecoregion = ecoregion,
    Species = toupper(species),
    Networks = paste0(nets_rep+nets_nonrep," (",nets_rep,",",nets_nonrep,")"),
    CMI = paste0(cmi_coef," (", cmi_tstat, ")"),
    GPP = paste0(gpp_coef," (", gpp_tstat, ")"),
    LED = paste0(led_coef," (", led_tstat, ")"),
    LCC = paste0(lcc_coef," (", lcc_tstat, ")"),
    R2 = r2,
    RMSE = rmse) %>%
    dplyr::select(Species, Ecoregion, Networks, CMI, GPP, LED, LCC, R2, RMSE)
write_csv(zz, 'output/tables/table_s1.csv')

