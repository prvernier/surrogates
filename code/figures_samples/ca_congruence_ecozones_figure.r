# Generate ecozones disagreement plot
# PV 2017-04-11

library(tidyverse)

x <- read_csv('manuscript/tables/supp_ca_congruence_eco4.csv')
x <- filter(x, Parameter %in% c("Type 1 disagreement","Type 2 disagreement"))
x$eco <- 4
for (eco in c(5,6,9,11,12,15)) {
	y <- read_csv(paste0('manuscript/tables/supp_ca_congruence_eco',eco,'.csv'))
	y <- filter(y, Parameter %in% c("Type 1 disagreement","Type 2 disagreement"))
	y$eco <- eco
	x <- rbind(x, y)
}

x$Disagreement <- rep(c("CIFL_present","CIFL_absent"),7)

x <- arrange(x, desc(Disagreement), eco)
x$Ecozone <- rep(c("Taiga Plain","Taiga Shield", "Boreal Shield", "Boreal Plain", "Taiga Cordillera", "Boreal Cordillera", "Hudson Plain"),2)
x <- x[,c("Disagreement","Ecozone","CIFL2000","GIFL2013","GIFL2000","HF2009","HF1993","FF","WILD_V2")]
colnames(x) <- c("Disagreement","Ecozone","CIFL2013xCIFL2000","CIFL2013xGIFL2013","CIFL2013xGIFL2000","CIFL2013xHF2009","CIFL2013xHF1993","CIFL2013xFF","CIFL2013xWILD")

present <- as.matrix(subset(x, Disagreement=="CIFL_present")[,3:9])
absent <- subset(x, Disagreement=="CIFL_absent")[,3:9]

rownames(present) <- rownames(absent) <- c("Taiga Plain","Taiga Shield", "Boreal Shield", "Boreal Plain", "Taiga Cordillera", "Boreal Cordillera", "Hudson Plain")

write_csv(x, "manuscript/tables/ca_congruence_ecozones.csv")

#win.metafile("manuscript/figures/fig4.emf", width=10, height=10)
png("manuscript/figures/fig4.png", width=800, height=1050)
op <- par(mfrow=c(1,2))
dotchart(t(present), xlim = c(0,0.60), main="CIFL2013 present and other map absent\n(type 1 disagreement)", pt.cex=1, pch=19, cex=0.85, xlab="Percent area of ecozone")
dotchart(t(absent), xlim = c(0,0.60), main="CIFL2013 absent and other map present\n(type 2 disagreement)", pt.cex=1, pch=19, cex=0.85, xlab="Percent area of ecozone")
par(op)
dev.off()
