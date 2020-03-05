library(sf)
library(tmap)
library(tidyverse)

sppList = c("all_birds","for_birds","caribou","boch","brcr","btnw","cawa","cmwa","pigr","rubl","swth","wwcr")
sppList = "wwcr"
ecor = st_read("data/pba_ecor.shp")
ecop = st_read("data/pba_ecop.shp")
ecoz = st_read("data/pba_ecoz.shp")

for (spp in sppList) {
    cat("\n\n",toupper(spp),"\n\n")
    flush.console()
    
    # Ecozones
    nullz = mutate(ecoz, NoData="")
    ecoz2 = filter(ecoz, species==spp)
    m1a = tm_shape(nullz) + tm_polygons("NoData", palette="#f0f0f0") +
        tm_shape(ecoz2) + tm_polygons("m1_r2", breaks=c(0,0.2,0.4,0.6,0.8,1), title="Model R2") + # palette="RdYlBu"
        tm_layout(title = paste("Ecozones -",toupper(spp)), title.position = c("center", "top"), legend.position = c("right", "top"))
    #tmap_save(m1a, filename=paste0("png/ecoz_",spp,"_r2.png"), height=1260, width=2100)    
    m1b = tm_shape(nullz) + tm_polygons("NoData", palette="#f0f0f0") +
        tm_shape(ecoz2) + tm_polygons("m1_rse", breaks=c(0,0.04,0.08,0.12,0.16,0.2), title="Model RSE") + # palette="RdYlBu"
        tm_layout(title = paste("Ecozones -",toupper(spp)), title.position = c("center", "top"), legend.position = c("right", "top"))
    #tmap_save(m1b, filename=paste0("png/ecoz_",spp,"_rse.png"), height=1260, width=2100)    
    
    # Ecoprovinces
    nullp = mutate(ecop, NoData="")
    ecop2 = filter(ecop, species==spp)
    if (nrow(ecop2) > 0) {
        m2a = tm_shape(nullp) + tm_polygons("NoData", palette="#f0f0f0") +
            tm_shape(ecop2) + tm_polygons("m1_r2", breaks=c(0,0.2,0.4,0.6,0.8,1), title="Model R2") + # palette="RdYlBu"
            tm_layout(title = paste("Ecoprovinces -",toupper(spp)), title.position = c("center", "top"), legend.position = c("right", "top"))
        #tmap_save(m2a, filename=paste0("png/ecop_",spp,"_r2.png"), height=1260, width=2100)    
        m2b = tm_shape(nullp) + tm_polygons("NoData", palette="#f0f0f0") +
            tm_shape(ecop2) + tm_polygons("m1_rse", breaks=c(0,0.04,0.08,0.12,0.16,0.2), title="Model RSE") + # palette="RdYlBu"
            tm_layout(title = paste("Ecoprovinces -",toupper(spp)), title.position = c("center", "top"), legend.position = c("right", "top"))
        #tmap_save(m2b, filename=paste0("png/ecop_",spp,"_rse.png"), height=1260, width=2100)    
    }

    # Ecoregions
    nullr = mutate(ecor, NoData="")
    ecor2 = filter(ecor, species==spp)
    if (nrow(ecor2) > 0) {
        m3a = tm_shape(nullr) + tm_polygons("NoData", palette="#f0f0f0") +
            tm_shape(ecor2) + tm_polygons("m1_r2", breaks=c(0,0.2,0.4,0.6,0.8,1), title="Model R2") + # palette="RdYlBu"
            tm_layout(title = paste("Ecoregions -",toupper(spp)), title.position = c("center", "top"), legend.position = c("right", "top"))
        #tmap_save(m3a, filename=paste0("png/ecor_",spp,"_r2.png"), height=1260, width=2100)    
        m3b = tm_shape(nullr) + tm_polygons("NoData", palette="#f0f0f0") +
            tm_shape(ecor2) + tm_polygons("m1_rse", breaks=c(0,0.04,0.08,0.12,0.16,0.2), title="Model RSE") + # palette="RdYlBu"
            tm_layout(title = paste("Ecoregions -",toupper(spp)), title.position = c("center", "top"), legend.position = c("right", "top"))
        #tmap_save(m3b, filename=paste0("png/ecor_",spp,"_rse.png"), height=1260, width=2100)    
    }
    if (spp=="wwcr") {
        tm = tmap_arrange(m1a, m1b, ncol=2)
        tmap_save(tm, filename=paste0("png/",spp,".png"), height=12, width=12)
    } else {
        tm = tmap_arrange(m1a, m1b, m2a, m2b, m3a, m3b, ncol=2)
        tmap_save(tm, filename=paste0("png/",spp,".png"), height=12, width=12)
    }
}
