calcKS <- function(spp, eco, net) {
    # spp must be a rasterLayer; eco and net can be rasterLayers or sf objects
    if (!class(spp)=="RasterLayer") {
        stop('Parameter 1: Class must be "RasterLayer"!')
    }
    if (class(eco)=="RasterLayer" & class(net)=="RasterLayer") {
        v1 = values(spp * eco)
        v2 = values(spp * net)
    } else if (class(fda)=="sf" & class(net)=="sf") {
        v1 = unlist(raster::extract(rast, eco_bnd))
        v2 = unlist(raster::extract(rast, net_bnd))
    } else {
        stop('Parameter 2 & 3: Class must be "RasterLayer" or "sf"!')
    }
    ks = round(as.numeric(ks.test(v1, v2, na.rm=T)$statistic),3)
}
