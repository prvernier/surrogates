calcBC <- function(spp, eco, net) {
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
	t1 = as_tibble(table(v1))
	colnames(t1) = c("class","region")
	t2 = as_tibble(table(v2))
	colnames(t2) = c("class","network")
	x = left_join(t1, t2)
    x$network[is.na(x$network)] = 0
	x$region <- x$region/sum(x$region)
	x$network <- x$network/sum(x$network)
	bc <- round(sum(abs(x$region-x$network))/(sum(x$region)+sum(x$network)),3)
}
