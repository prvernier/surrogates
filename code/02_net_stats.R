# Generate networks statistics
# 2019-08-21
# P Vernier

library(sf)
library(tidyverse)

netDir = 'C:/Users/PIVER37/Dropbox (BEACONs)/BEACONs Share/surrogates/data/networks/clusters/'

pba = read_sf("data/vector/pba_ecoregions.shp") %>%
    select(ecozone, ecoprov, ecoreg) %>%
    arrange(ecoreg)

z = tibble(ecozone=as.character(), ecoprovince=as.numeric(), ecoregion=as.integer(),
           all_nets=as.integer(), rep_nets=as.integer(), nonrep_nets=as.integer(), 
           rep_gd10km=as.integer(), nonrep_gd10km=as.integer(), rep1_nets=as.integer(), nonrep1_nets=as.integer())

i = 1
for (eco in pba$ecoreg) {
    cat("Ecoregion", eco, "...\n"); flush.console()
    
    v_nets = paste0(netDir,"eco_",eco,"_networks")
    if (file.exists(paste0(v_nets,".shp"))) {
        v = read_sf(paste0(v_nets,".shp"))
        v = mutate(v, rep1=if_else(ks_cmi<=0.2 & ks_gpp<=0.2 & ks_led<=0.2 & bc_lcc<=0.2 & sumGap90==0, 1, 0))
        v = mutate(v, nonrep1=if_else(ks_cmi>0.2 & ks_gpp>0.2 & ks_led>0.2 & bc_lcc>0.2, 1, 0))

        z[i,"ecozone"] = pba$ecozone[i]
        z[i,"ecoprovince"] = pba$ecoprov[i]
        z[i,"ecoregion"] = eco
        z[i,"all_nets"] = nrow(v)
        z[i,"rep1_nets"] = sum(v$rep1)
        z[i,"nonrep1_nets"] = sum(v$nonrep1)
        if (file.exists(paste0(v_nets,"_rep.shp"))) {
            r = read_sf(paste0(v_nets,"_rep.shp"))
            z[i,"rep_nets"] = nrow(r)
            if ("top10km" %in% names(r)) {
                z[i,"rep_gd10km"] = as.integer(sum(r$top10km))
            } else {
                z[i,"rep_gd10km"] = as.integer(nrow(r))
            }    
        } else {
            z[i,"rep_nets"] = 0
            z[i,"rep_gd10km"] = 0
        }
        if (file.exists(paste0(v_nets,"_nonrep.shp"))) {
            nr = read_sf(paste0(v_nets,"_nonrep.shp"))
            z[i,"nonrep_nets"] = nrow(nr)
            if ("top10km" %in% names(nr)) {
                z[i,"nonrep_gd10km"] = as.integer(sum(nr$top10km))
            } else {
                z[i,"nonrep_gd10km"] = as.integer(nrow(nr))
            }    
        } else {
            z[i,"nonrep_nets"] = 0
            z[i,"nonrep_gd10km"] = 0
        }
    } else {
        cat("...skipping, no networks\n")
    }   
    i = i + 1
}
z = mutate(z, ecoreg=NULL)
z$diff_rep = z$rep1_nets - z$rep_nets
z$diff_nonrep = z$nonrep1_nets - z$nonrep_nets
write_csv(z[!is.na(z$ecoregion),], "code/input/net_summary_stats_new.csv")

# Join to shapefile
pba1 = mutate(pba, ecozone=NULL, ecoprov=NULL)
z = mutate(z, ecoreg=ecoregion)
pba1 = left_join(pba1, z)
pba1 = drop_na(pba1)
pba1 = mutate(pba1, ecoreg=NULL)
pba1 = mutate(pba1, ecoprov=ecoprovince, ecoreg=ecoregion, nets=all_nets, 
             rep=rep_nets, nonrep=nonrep_nets, rep10=rep_gd10km, nonrep10=nonrep_gd10km,
             ecoprovince=NULL, ecoregion=NULL, all_nets=NULL, rep_nets=NULL, nonrep_nets=NULL,
             rep_gd10km=NULL, nonrep_gd10km=NULL)
write_sf(pba1, "output/net_summary_stats_new.shp")
