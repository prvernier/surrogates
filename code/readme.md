Surrogates analysis
PV 2020-08-25

This folder contains code for the statistical analysis.

# 01_gen_data.R
    * Generate caribou and bird raster maps for analysis with common projection etc.
    * Sum rasters by bird assemblages

# 02_calc_ks.R
    * Calculate KS statistic for representative and non-representative networks for all ecoregions and species groups
    * Originally used velox package, which is no longer maintained; changed to exact_extracr package

# 03_merge_data.R
    * Prepare data for statistical analysis
    * Merge ecozone-level data containing dissimilarity metrics for species and surrogates
    * For each ecozone, randomly select up to 500 representative and non-representative networks

# 03_test_obj1&2.R
    * Obj1: Are networks representative of surrogates also representative of non-target species?
        - Use representative networks only
    * Obj2: Are representative networks more effective than non-representative networks for target species?
        - Use representative and non-representative networks
        - Use t-test or wilcox.test (t-test should be fine due to large sample sizes)
        - Calculate effect size = (mean of DMrep) – (mean of DMnonrep) / std dev of DM
    
# 04_test_obj3.R
    * Obj3: Does the relative importance of environmental surrogates vary by groups of species?
        - Use representative networks only
        - Model species DM as a function of CMI + GPP + LED + LCC
        - Count number of times each surrogate is the most important variable

# 05_test_obj4.R
    * Obj4: Is the relationship between surrogates and non-target species influenced by other variables?
        - Test ecoregion membership, intactness, MDR size, density, and density CV
        - Calculate AIC for model with and without covariate
        - Use representative networks only
