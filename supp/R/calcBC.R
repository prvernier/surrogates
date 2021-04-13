#' Calculate Bray-Curtis dissimilarity
#'
#' Calculates the BC statistic between a reserve area and a reference area in \code{\link{brocolors}}
#' Calculate relative Bray-Curtis dissimilarity for categorical raster maps
#' Convert to proportions prior to calculating - ranges from 0 (similar) to 1 (dissimilar)
#' Requires two vectors of categorical data (not tabulated)
#'
#' @param spp A vector of values from a raster object of class RasterLayer representing species of interest.
#' @param eco A vector of values from a raster object of class RasterLayer representing reference area.
#' @param net A vector of values from a raster object of class RasterLayer representing network to be evaluated.
#'
#' @return Numeric, Bray-Curtis dissimilarity metric.
#'
#' @author Pierre R Vernier, \email{pierre.vernier@@gmail.com}
#' @author Pierre R. Vernier, University of Alberta, <pierre.vernier@@gmail.com>

#' @examples
#' calcBC(species, ecoregion, network)
#'
#' @export
calcBC <- function(rstack, ref, nets) {
    # rstack must be a rasterStack; ref and nets must be sf objects
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
            z = mutate(z, "{inames[k]}_bc" := 0)
        }

        for (j in 1:length(nets$network)) {
            #cat('Network',j,'of', n, '\n'); flush.console()
            for (i in 1:length(inames)) {
                v1 = unlist(x1[[1]][i])
                t1 = as_tibble(table(v1))
                colnames(t1) = c("class","region")
                v2 = unlist(x2[[j]][i])
                t2 = as_tibble(table(v2))
                colnames(t2) = c("class","network")
                x = left_join(t1, t2)
                x$network[is.na(x$network)] = 0
                x$region <- x$region/sum(x$region)
                x$network <- x$network/sum(x$network)
                z[j,i+1] <- round(sum(abs(x$region-x$network))/(sum(x$region)+sum(x$network)),3)
            }
        }
    }
    return(z)
}
