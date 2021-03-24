library(tidyverse)
library(tidymodels)

set.seed(9093)
z = read_csv('code/input/bcr4_deciduousbirds.csv')
preds <- c("ks_cmi", "ks_gpp", "ks_led", "bc_lcc")
data_split <- initial_split(z, strata = "rep", p = 0.75)
spp_train <- training(data_split)
spp_test  <- testing(data_split)

# random forest
rf_defaults <- rand_forest(mode = "regression")
rf_defaults

rf_xy_fit <- 
  rf_defaults %>%
  set_engine("ranger") %>%
  fit_xy(
    x = spp_train[, preds],
    y = spp_train$dissim
  )
tidy(rf_xy_fit)

rf_test_results <- 
  spp_test %>%
  select(dissim) %>%
  bind_cols(
    predict(rf_xy_fit, new_data = spp_test[, preds])
  )
rf_test_results %>% slice(1:5)
rf_test_results %>% metrics(truth = dissim, estimate = .pred) 

# linear model
lm_fit <- 
  linear_reg() %>% 
  set_engine("lm") %>%
  fit_xy(x=spp_train[,preds], y=spp_train$dissim)
tidy(lm_fit)

lm_test_results <-
  spp_test %>%
  select(dissim) %>%
  bind_cols(
    predict(lm_fit, new_data=spp_test[,preds])
  )

lm_test_results %>% slice(1:5)
lm_test_results %>% metrics(truth = dissim, estimate = .pred) 

