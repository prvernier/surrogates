# Create table 3 for results section
# PV 2020-08-26

library(tidyverse)

# Bird assemblages by BCR
x = read_csv('output/obj1&2_bcr_birds.csv') %>%
    mutate(w.est = paste0(w.est," (",w.lci,",",w.uci,") "),
        d = effect_size_median)

x1 = select(x, species, bcr, rep_median) %>%
    spread(bcr, rep_median) %>%
    rename(R_BCR4=BCR4,R_BCR6=BCR6,R_BCR7=BCR7,R_BCR8=BCR8)

x2 = select(x, species, bcr, nonrep_median) %>%
    spread(bcr, nonrep_median) %>%
    rename(NR_BCR4=BCR4,NR_BCR6=BCR6,NR_BCR7=BCR7,NR_BCR8=BCR8)
y = full_join(x1, x2)

x3 = select(x, species, bcr, w.est) %>%
    spread(bcr, w.est) %>%
    rename(W_BCR4=BCR4,W_BCR6=BCR6,W_BCR7=BCR7,W_BCR8=BCR8)
y = left_join(y, x3)

x4 = select(x, species, bcr, d) %>%
    spread(bcr, d) %>%
    rename(D_BCR4=BCR4,D_BCR6=BCR6,D_BCR7=BCR7,D_BCR8=BCR8)
y = full_join(y, x4)

z = y %>%
    mutate(y, id = c(1,16,13,5,6,3,2,8,14,4,7,9,11,15,12,10)) %>%
    arrange(id) %>%
    mutate(id=NULL, group=c('Conservation','','','','Habitat','','','','Migration','','','','Waterfowl','','',''))

z = z[,c('group','species','R_BCR4','NR_BCR4','W_BCR4','D_BCR4','R_BCR6','NR_BCR6','W_BCR6','D_BCR6',
    'R_BCR7','NR_BCR7','W_BCR7','D_BCR7','R_BCR8','NR_BCR8','W_BCR8','D_BCR8')]

write_csv(z, 'output/results_table3.csv')


# Caribou by BCR
x = read_csv('output/obj1&2_bcr_caribou.csv') %>%
    mutate(w.est = paste0(w.est," (",w.lci,",",w.uci,") "),
        d = effect_size_median)

x1 = select(x, species, bcr, rep_median) %>%
    spread(bcr, rep_median) %>%
    rename(R_BCR6=BCR6,R_BCR7=BCR7,R_BCR8=BCR8)

x2 = select(x, species, bcr, nonrep_median) %>%
    spread(bcr, nonrep_median) %>%
    rename(NR_BCR6=BCR6,NR_BCR7=BCR7,NR_BCR8=BCR8)
y = full_join(x1, x2)

x3 = select(x, species, bcr, w.est) %>%
    spread(bcr, w.est) %>%
    rename(W_BCR6=BCR6,W_BCR7=BCR7,W_BCR8=BCR8)
y = left_join(y, x3)

x4 = select(x, species, bcr, d) %>%
    spread(bcr, d) %>%
    rename(D_BCR6=BCR6,D_BCR7=BCR7,D_BCR8=BCR8)
y = full_join(y, x4)

y = y[,c('species','R_BCR6','NR_BCR6','W_BCR6','D_BCR6','R_BCR7','NR_BCR7','W_BCR7','D_BCR7','R_BCR8','NR_BCR8','W_BCR8','D_BCR8')]

write_csv(y, 'output/results_table3_caribou.csv')
