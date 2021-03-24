# Bootstrap 95% CI for difference in medians
library(infer)
library(tidyverse)

z = read_csv('code/input/bcr4_deciduousbirds.csv')
z2 = mutate(z, rep2=if_else(rep==1, "rep", "nrep"))

d_hat <- z2 %>% 
  specify(dissim ~ rep2) %>% 
  calculate(stat = "diff in medians", order = c("nrep", "rep"))
d_hat

null_distn <- z2 %>% 
  specify(dissim ~ rep2) %>% 
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat="diff in medians", order=c('nrep','rep'))
null_distn

visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")

my_ci <- null_distn %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
my_ci
visualize(null_distn) +
  shade_confidence_interval(endpoints = my_ci, color = "hotpink", fill = "khaki")
