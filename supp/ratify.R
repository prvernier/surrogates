library(raster)
library(tmap)

lcc = raster("supp/eco89_data/lcc.tif")
lcc <- ratify(lcc)
rat = levels(lcc)[[1]]
rat$VALUE <- c('Conifer temperate', 'Conifer taiga', 'Broadleaf','Mixed forest', 'Shrubland', 'Grassland','Shrubland-lichen-moss', 'Grassland-lichen-moss', 'Wetland','Barren land', 'Water')
levels(lcc) <- rat
lcc_pal = c(rgb(0,61,0, max=255), rgb(148,156,112, max=255), rgb(20,140,61, max=255), rgb(92,117,43, max=255), rgb(179,138,51, max=255), rgb(224,207,138, max=255), rgb(156,117,84, max=255), rgb(186,212,143, max=255), rgb(107,163,138, max=255), rgb(168,171,173, max=255), rgb(77,112,163, max=255))

tm_shape(lcc) + tm_raster(n=11, pal=lcc_pal, style='cat', title='Class') +
    tm_layout(main.title="Landcover", legend.position = c("right", "bottom"))
