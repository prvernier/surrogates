# https://uc-r.github.io/kmeans_clustering
# Pierre Vernier
# 2019-08-21

library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#birds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
birds = read_csv("code/input/bam_density_80_species.csv") %>% pull(Code) %>% tolower()
reco = raster("data/raster/ecoregions.tif")
pba = read_csv("code/input/pan_eco_mdr_v4.csv") %>% arrange(ecoregion)
ecoregions = pba$ecoregion
net = read_csv("output/tables/ecozone_models_1000_ecoregions.csv")
net_eco = unique(net$ecoregion)

x = read_csv("code/input/species_stats80.csv") %>%
    select(ecoregion, spp, mean, std_dev, p80, p90, p95) #%>%
x = filter(x, ecoregion %in% net_eco) #%>%
    #filter(std_dev<3)

# Create data matrix
y =  select(x, ecoregion, spp, p80) %>% spread(ecoregion, p80) %>% data.frame()
names(y) = c("spp",paste0("eco",net_eco))
#y = as_tibble(y)
#y2 =  select(x, ecoregion, spp, std_dev) %>% spread(ecoregion, std_dev) %>% data.frame()
#names(y2) = c("spp",paste0("sd",net_eco))
#y2 = as_tibble(y2)
#y = left_join(y, y2)

rownames(y) = y$spp
y$spp = NULL
y = na.omit(y)
df = scale(y)

# Determine number of clusters
windows()
fviz_nbclust(df, kmeans, method = "wss")

windows()
k = kmeans(df, centers=5, nstart=100)
fviz_cluster(k, data = df)

print(k)

windows()
k2 <- kmeans(df, centers = 2, nstart = 100)
k3 <- kmeans(df, centers = 3, nstart = 100)
k4 <- kmeans(df, centers = 4, nstart = 100)
k5 <- kmeans(df, centers = 5, nstart = 100)
k6 <- kmeans(df, centers = 6, nstart = 100)
k7 <- kmeans(df, centers = 7, nstart = 100)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = df) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = df) + ggtitle("k = 7")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
