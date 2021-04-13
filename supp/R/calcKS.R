#' Calculate KS statistic
#'
#' Calculates the KS statistic between a reserve area and a reference area in \code{\link{brocolors}}
#'
#' @param spp species raster
#' @param eco boundary raster; e.g., ecoregion
#' @param net network raster; raster comprising one or more polygons
#'
#' @return A numeric value
#'
#' @author Pierre R Vernier, \email{pierre.vernier@@gmail.com}

#' @examples
#' calcKS(species, ecoregion, network)
#'
#' @export

calcKS <- function(rstack, ref, nets) {
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
            z = mutate(z, "{inames[k]}_ks" := 0)
        }
        for (j in 1:length(nets$network)) {
            #cat('Network',j,'of', n, '\n'); flush.console()
            for (i in 1:length(inames)) {
                z[j,i+1] = round(as.numeric(ks.test(unlist(x1[[1]][i]), unlist(x2[[j]][i]), na.rm=T)$statistic),3)
            }
        }
    }
    return(z)
}
