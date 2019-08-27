calcRI <- function(spp, fda, net) {
    # spp must be a rasterLayer; # fda and net can be rasterLayers or sf objects
    if (!class(spp)=="RasterLayer") {
        stop('Parameter 1: Class must be "RasterLayer"!')
    }
    if (class(fda)=="RasterLayer" & class(net)=="RasterLayer") {
        # calculate denominator
        net_area = cellStats(net, sum)
        fda_area = cellStats(fda, sum)
        # calculate numerator
        spp_net = spp * net
        net_spp_sum = cellStats(spp_net, sum)
        fda_spp_sum = cellStats(spp, sum)
    } else if (class(fda)=="sf" & class(net)=="sf") {
        # calculate denominator
        net_area = as.numeric(round(st_area(net)/1000000,1))
        fda_area = as.numeric(round(st_area(fda)/1000000,1))
        # calculate numerator
        net_spp = unlist(raster::extract(spp, net))
        net_spp_sum = sum(net_spp, na.rm=T)
        fda_spp = unlist(raster::extract(spp, fda))
        fda_spp_sum = sum(fda_spp, na.rm=T)
    } else {
        stop('Parameter 2 & 3: Class must be "RasterLayer" or "sf"!')
    }
    # representation index = numerator / denominator
    ri = round((net_spp_sum/fda_spp_sum) / (net_area/fda_area),3)
}
