#' Calculate RI statistic
#'
#' Calculates the RI statistic between a reserve area and a reference area in \code{\link{brocolors}}
#'
#' @param spp species raster
#' @param eco boundary raster; e.g., ecoregion
#' @param net network raster; raster comprising one or more polygons
#'
#' @return A numeric value
#'
#' @author Pierre R Vernier, \email{pierre.vernier@@gmail.com}

#' @examples
#' calcRI(species, ecoregion, network)
#'
#' @export
calcRI <- function(rstack, ref, nets) {
    # rstack must be a rasterLayer or rasterStack; ref and nets must be sf objects
    # one reference area and one or more networks
    require(exactextractr)
    if (!class(rstack) %in% c("RasterLayer","RasterStack")) {
        stop('Parameter 1: Class must be "RasterStack"!')
    } else if (!class(ref)[1]=="sf" & !class(nets)[1]=="sf") {
        stop('Parameter 2 & 3: Class must be "sf"!')
    } else {
        n = dim(nets)[1]
        x1 = exact_extract(rstack, ref)
        x2 = exact_extract(rstack, nets)
        z = tibble(networks=nets$network)
        if (class(rstack)=="RasterLayer") {
            inames = names(rstack)
        } else {
            inames = names(x1[[1]])[1:length(names(x1[[1]]))-1]
        }
        for (k in 1:length(inames)) {
            z = mutate(z, "{inames[k]}_ri" := 0)
        }
        ref_area = as.numeric(round(sum(st_area(ref))/1000000,1))
        nets = mutate(nets, id=1:nrow(nets))
        for (j in 1:length(nets$network)) {
            #cat('Network',j,'of', n, '\n'); flush.console()
            net = filter(nets, id==j)
            net_area = as.numeric(round(st_area(net)/1000000,1))
            for (i in 1:length(inames)) {
                ref_spp = unlist(x1[[1]][i])
                ref_spp_sum = sum(ref_spp, na.rm=T)
                net_spp = unlist(x2[[j]][i])
                net_spp_sum = sum(net_spp, na.rm=T)
                z[j,i+1] = round((net_spp_sum/ref_spp_sum)/(net_area/ref_area),3)
            }
        }
    }
    return(z)
}
