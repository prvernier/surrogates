library(lattice)
library(tidyverse)

x = read_csv(paste0('output/tables/ecozone_models_1000.csv')) %>% filter(species=="ALLBIRDS" & ecozone %in% c("4","5","6A"))
print(x)

data(VADeaths)
deathrate <- as.vector(VADeaths)
age <- rep(rownames(VADeaths), ncol(VADeaths))
type <- as.ordered(c(rep("Rural Male", 5),
          rep("Rural Female", 5),
          rep("Urban Male", 5),
          rep("Urban Female", 5)))
dotplot(age ~ deathrate | type, layout=c(1,4))

dotplot(type ~ deathrate | age, layout=c(1,5), ylab=NULL)

data(barley)
dotplot(variety~yield|year*site, data=barley)
