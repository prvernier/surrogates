# Create tables and figures for results section of manuscript
# Pierre Vernier
# 2019-08-16

library(tidyverse)

# ECOZONES

z = read_csv(paste0('output/tables/ecozone_models_1000.csv'))

# Summarize coefficients
x1 = group_by(z, species) %>% summarize(
        n_ecoz = sum(n()),
        cmi = paste0(sprintf("%.2f",mean(cmi_coef))," (",sprintf("%.2f",min(cmi_coef)),"-",sprintf("%.2f",max(cmi_coef)),")"),
        gpp = paste0(sprintf("%.2f",mean(gpp_coef))," (",sprintf("%.2f",min(gpp_coef)),"-",sprintf("%.2f",max(gpp_coef)),")"),
        led = paste0(sprintf("%.2f",mean(led_coef))," (",sprintf("%.2f",min(led_coef)),"-",sprintf("%.2f",max(led_coef)),")"),
        lcc = paste0(sprintf("%.2f",mean(lcc_coef))," (",sprintf("%.2f",min(lcc_coef)),"-",sprintf("%.2f",max(lcc_coef)),")"),
        rsquared = paste0(sprintf("%.2f",mean(m1_r2))," (",sprintf("%.2f",min(m1_r2)),"-",sprintf("%.2f",max(m1_r2)),")"),
        rmse = paste0(sprintf("%.2f",mean(m1_rmse))," (",sprintf("%.2f",min(m1_rmse)),"-",sprintf("%.2f",max(m1_rmse)),")")) %>%
    mutate(rank=case_when(
        species=="ALLBIRDS" ~ 1,
        species=="FORESTBIRDS" ~ 2,
        species=="ALLWATERFOWL" ~ 3,
        species=="CAVITYNESTERS" ~ 4,
        species=="GROUNDNESTERS" ~ 5,
        species=="OVERWATERNESTERS" ~ 6,
        species=="BLBW" ~ 7,
        species=="BOCH" ~ 8,
        species=="BRCR" ~ 9,
        species=="BTNW" ~ 10,
        species=="CAWA" ~ 11,
        species=="CMWA" ~ 12,
        species=="OSFL" ~ 13,
        species=="PIGR" ~ 14,
        species=="RUBL" ~ 15,
        species=="SWTH" ~ 16,
        species=="WWCR" ~ 17,
        TRUE ~ 0)) %>%
     arrange(rank)    
write_csv(x1, paste0('output/tables/models_summary_ecozones.csv'))

# Summarize R2 and RMSE by species x ecozone
x2a = dplyr::select(z, ecozone, species, m1_r2) %>% spread(ecozone, m1_r2) %>% mutate(metric="R2", id=as.integer(seq(1,34,2)))
x2b = dplyr::select(z, ecozone, species, m1_rmse) %>% spread(ecozone, m1_rmse) %>% mutate(metric="RMSE", id=as.integer(seq(2,35,2)))
x2 = bind_rows(x2a, x2b) %>% arrange(id) %>% dplyr::select(species, metric, '4', '5', '6A', '6B', '9', '11', '12', '14', '15')
names(x2) = c('Species', 'Metric', 'Ecoz4', 'Ecoz5', 'Ecoz6A', 'Ecoz6B', 'Ecoz9', 'Ecoz11', 'Ecoz12', 'Ecoz14', 'Ecoz15')
write_csv(x2, paste0('output/tables/models_summary_both_ecozones.csv'))

# Summarize variable importance by species x ecozone
x2a = dplyr::select(z, ecozone, species, cmi_t) %>% spread(ecozone, cmi_t) %>% mutate(surrogate="CMI", id=as.integer(seq(1,68,4)))
x2b = dplyr::select(z, ecozone, species, gpp_t) %>% spread(ecozone, gpp_t) %>% mutate(surrogate="GPP", id=as.integer(seq(2,69,4)))
x2c = dplyr::select(z, ecozone, species, led_t) %>% spread(ecozone, led_t) %>% mutate(surrogate="LED", id=as.integer(seq(3,70,4)))
x2d = dplyr::select(z, ecozone, species, lcc_t) %>% spread(ecozone, lcc_t) %>% mutate(surrogate="LCC", id=as.integer(seq(4,71,4)))
x2 = bind_rows(x2a, x2b, x2c, x2d) %>% arrange(id) %>% dplyr::select(species, surrogate, '4', '5', '6A', '6B', '9', '11', '12', '14', '15')
names(x2) = c('Species', 'Surrogate', 'Ecoz4', 'Ecoz5', 'Ecoz6A', 'Ecoz6B', 'Ecoz9', 'Ecoz11', 'Ecoz12', 'Ecoz14', 'Ecoz15')
write_csv(x2, paste0('output/tables/surrogate_importance_ecozones.csv'))

# Summarize variable importance - second try
x3 = select(z, ecozone, species, cmi_t, gpp_t, led_t, lcc_t) %>%
    mutate(cmi=as.integer(0),gpp=as.integer(0),led=as.integer(0),lcc=as.integer(0))
for (i in 1:nrow(x3)) {
    if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"cmi_t"]) {
        x3[i,"cmi"]=1
    } else if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"gpp_t"]) {
        x3[i,"gpp"]=1
    } else if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"led_t"]) {
        x3[i,"led"]=1
    } else if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"lcc_t"]) {
        x3[i,"lcc"]=1
    }
}
write_csv(x3, paste0('output/tables/surrogate_importance_ecozones_counts.csv'))


# ECOREGIONS

z = read_csv(paste0('output/tables/ecozone_models_1000_ecoregions.csv'))

# Summarize coefficients
x1 = group_by(z, species) %>% summarize(
        n_ecoz = sum(n()),
        cmi = paste0(sprintf("%.2f",mean(cmi_coef))," (",sprintf("%.2f",min(cmi_coef)),"-",sprintf("%.2f",max(cmi_coef)),")"),
        gpp = paste0(sprintf("%.2f",mean(gpp_coef))," (",sprintf("%.2f",min(gpp_coef)),"-",sprintf("%.2f",max(gpp_coef)),")"),
        led = paste0(sprintf("%.2f",mean(led_coef))," (",sprintf("%.2f",min(led_coef)),"-",sprintf("%.2f",max(led_coef)),")"),
        lcc = paste0(sprintf("%.2f",mean(lcc_coef))," (",sprintf("%.2f",min(lcc_coef)),"-",sprintf("%.2f",max(lcc_coef)),")"),
        rsquared = paste0(sprintf("%.2f",mean(m1_r2))," (",sprintf("%.2f",min(m1_r2)),"-",sprintf("%.2f",max(m1_r2)),")"),
        rmse = paste0(sprintf("%.2f",mean(m1_rmse))," (",sprintf("%.2f",min(m1_rmse)),"-",sprintf("%.2f",max(m1_rmse)),")")) %>% 
    mutate(rank=case_when(
        species=="ALLBIRDS" ~ 1,
        species=="FORESTBIRDS" ~ 2,
        species=="ALLWATERFOWL" ~ 3,
        species=="CAVITYNESTERS" ~ 4,
        species=="GROUNDNESTERS" ~ 5,
        species=="OVERWATERNESTERS" ~ 6,
        species=="BLBW" ~ 7,
        species=="BOCH" ~ 8,
        species=="BRCR" ~ 9,
        species=="BTNW" ~ 10,
        species=="CAWA" ~ 11,
        species=="CMWA" ~ 12,
        species=="OSFL" ~ 13,
        species=="PIGR" ~ 14,
        species=="RUBL" ~ 15,
        species=="SWTH" ~ 16,
        species=="WWCR" ~ 17,
        TRUE ~ 0)) %>%
     arrange(rank)    
write_csv(x1, paste0('output/tables/models_summary_ecoregions.csv'))


# Summarize R2 and RMSE by species x ecoregion
x2a = dplyr::select(z, ecoregion, species, m1_r2) %>% spread(ecoregion, m1_r2) %>% mutate(metric="R2", id=as.integer(seq(1,34,2)))
x2b = dplyr::select(z, ecoregion, species, m1_rmse) %>% spread(ecoregion, m1_rmse) %>% mutate(metric="RMSE", id=as.integer(seq(2,35,2)))
x2 = bind_rows(x2a, x2b) %>% arrange(id)
names(x2) = c('Species', 'Metric', paste0("Ecor",unique(z$ecoregion)),"id")
x2 = mutate(x2, id=NULL)
write_csv(x2, paste0('output/tables/models_summary_both_ecoregions.csv'))

# Summarize variable importance by species x ecoregion
x2a = dplyr::select(z, ecoregion, species, cmi_t) %>% spread(ecoregion, cmi_t) %>% mutate(surrogate="CMI", id=as.integer(seq(1,68,4)))
x2b = dplyr::select(z, ecoregion, species, gpp_t) %>% spread(ecoregion, gpp_t) %>% mutate(surrogate="GPP", id=as.integer(seq(2,69,4)))
x2c = dplyr::select(z, ecoregion, species, led_t) %>% spread(ecoregion, led_t) %>% mutate(surrogate="LED", id=as.integer(seq(3,70,4)))
x2d = dplyr::select(z, ecoregion, species, lcc_t) %>% spread(ecoregion, lcc_t) %>% mutate(surrogate="LCC", id=as.integer(seq(4,71,4)))
x2 = bind_rows(x2a, x2b, x2c, x2d) %>% arrange(id)
names(x2) = c('Species', 'Metric', paste0("Ecor",unique(z$ecoregion)),"id")
x2 = mutate(x2, id=NULL)
write_csv(x2, paste0('output/tables/surrogate_importance_ecoregions.csv'))

# Summarize variable importance - second try
x3 = select(z, ecoregion, species, cmi_t, gpp_t, led_t, lcc_t) %>%
    mutate(cmi=as.integer(0),gpp=as.integer(0),led=as.integer(0),lcc=as.integer(0))
for (i in 1:nrow(x3)) {
    if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"cmi_t"]) {
        x3[i,"cmi"]=1
    } else if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"gpp_t"]) {
        x3[i,"gpp"]=1
    } else if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"led_t"]) {
        x3[i,"led"]=1
    } else if (max(x3[i,"cmi_t"],x3[i,"gpp_t"],x3[i,"led_t"],x3[i,"lcc_t"])==x3[i,"lcc_t"]) {
        x3[i,"lcc"]=1
    }
}
write_csv(x3, paste0('output/tables/surrogate_importance_ecoregions_counts.csv'))
