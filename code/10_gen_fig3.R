# Create figures for results section of manuscript
# PV 2020-02-10

library(tidyverse)

# Figure 1 - map of the number of songbirds models that are decent (adj-R2 >= 0.2)
songbirds = c('blbw','boch','brcr','btnw','cawa','cmwa','osfl','pigr','rubl','swth','wwcr')
spp_names = c("Blackburnian Warbler","Boreal Chickadee","Brown Creeper","Black-throated Green Warbler",
    "Canada Warbler","Cape May Warbler","Olive-sided Flycatcher","Pine Grosbeak","Rusty Blackbird",
    "Swainson's Thrush","White-winged Crossbill")

z = read_csv(paste0('output/tables/models_r2_by_ecoregion_rnr.csv')) %>%
    select(ecozone, ecoregion, songbirds) %>%
    mutate(ecoregion=as.integer(ecoregion), ecozone = factor(ecozone, levels=c('4','5','6A','6B','9','11','12','14','15')),
        ecozone=recode(ecozone, 
            `4`="Taiga Plains",
            `5`="Taiga Shield",
            `6A`="Boreal Shield A",
            `6B`="Boreal Shield B",
            `9`="Boreal Plains",
            `11`="Taiga Cordillera",
            `12`="Boreal Cordillera",
            `14`="Montane Cordillera",
            `15`="Hudson Plains"),)

zz = gather(z, species, adjR, blbw:wwcr, factor_key=TRUE)
pba = filter(zz, species %in% c('blbw','btnw','cawa','cmwa','osfl'))
nwb = filter(zz, species %in% c('boch','brcr','pigr','swth','wwcr'))
spp_colours = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
scaleFUN <- function(x) sprintf("%.0f", x)
ggplot(data=pba, aes(x=ecoregion, y=adjR, color=species)) +
    geom_point() + geom_line() + geom_hline(yintercept=0.2, linetype="dashed", colour="blue", size=0.5) +
    facet_wrap(~ ecozone, ncol=3, scales="free_x") +  ylim(0.0,1.0) +
    ylab("Adjusted-R2") + xlab("Ecoregion") + theme(legend.position="right") +
    scale_colour_manual(values=spp_colours) + scale_x_continuous(labels=scaleFUN)
ggsave(paste0("code/figs/pba_birds_lines.png"))
ggplot(data=nwb, aes(x=ecoregion, y=adjR, color=species)) +
    geom_point() + geom_line() + geom_hline(yintercept=0.2, linetype="dashed", colour="blue", size=0.5) +
    facet_wrap(~ ecozone, ncol=3, scales="free_x") +  ylim(0.0,1.0) +
    ylab("Adjusted-R2") + xlab("Ecoregion") + theme(legend.position="right") +
    scale_colour_manual(values=spp_colours) + scale_x_continuous(labels=scaleFUN)
ggsave(paste0("code/figs/nwb_birds_lines.png"))
