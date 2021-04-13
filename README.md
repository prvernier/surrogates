# Surrogates Paper

Revised: 2021-04-13


Evaluating the Effectiveness of Biodiversity Surrogates


## Manuscript

Beacons et al. <i>in prep</i>. Evaluating the Effectiveness of Biodiversity Surrogates for Conservation Planning in the Boreal Region of Canada.


## Supporting information

* [Appendix S1 and S2. Construction and filtering of benchmark networks.](https://github.com/prvernier/surrogates/blob/master/supp/appendix_s1s2.md)
* [Table S1. Bird species common and latin names](https://github.com/prvernier/surrogates/blob/master/supp/table_s1.md)
* [Table S2. Summary of multiple linear regression models relating surrogates KS values to species KS values for all ecoregions with representative and non-representative networks in the boreal region of Canada.](https://github.com/prvernier/surrogates/blob/master/supp/table_s2.md)
* [Case study: Evaluating the effectiveness of surrogates (ecoregion 89) ([Rmarkdown](supp/case_study_89.Rmd) or [HTML](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp/case_study_89.html))
* [Case study: Evaluating the effectiveness of surrogates (ecoregion 181) ([Rmarkdown](supp/case_study_181.Rmd) or [HTML](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp/case_study_181.html))


## Code

The following scripts can be used to replicate the analysis. Required datasets are too large to store and Github and can be obtained from the author. Check the top of each script to determine which datasets are needed. If you have any questions, please contact: pierre.vernier@gmail.com

### Prepare data

  * 00_check_spp.R
    - Check to make sure we have all the proper scientific names and make modifications where needed
    - Requires BOTW.gdb from http://datazone.birdlife.org/species/requestdis
  
  * 01_gen_data.R
    - Generate caribou and bird raster maps for analysis with common projection etc.
    - Sum rasters by bird assemblages

### Calculate dissimilarity

  * 02_calc_ks.R
    - Calculate KS statistic for representative and non-representative networks for all ecoregions and species groups
    - Originally used velox package, which is no longer maintained; changed to exact_extracr package

  * 03_merge_ecor.R
    - Prepare data for statistical analysis
    - Merge ecoregion-level data containing dissimilarity metrics for species and surrogates

  * 04_merge_ecoz.R
    - Prepare data for statistical analysis
    - Merge ecozone-level data containing dissimilarity metrics for species and surrogates
    - For each ecozone, randomly select up to 500 representative and non-representative networks

### Test hypotheses

  * 05_test_obj1&2.R
    - Obj1: Are networks representative of surrogates also representative of non-target species?
       - Use representative networks only
    - Obj2: Are representative networks more effective than non-representative networks for target species?
      - Use representative and non-representative networks
      - Use t-test or wilcox.test (t-test should be fine due to large sample sizes)
      - Calculate effect size = (mean of DMrep) – (mean of DMnonrep) / std dev of DM
    
  * 06_test_obj3.R
    - Obj3: Does the relative importance of environmental surrogates vary by groups of species?
      - Use representative networks only
      - Model species DM as a function of CMI + GPP + LED + LCC
      - Count number of times each surrogate is the most important variable

  *  07_test_obj4.R
    - Obj4: Is the relationship between surrogates and non-target species influenced by other variables?
      - Test ecoregion membership, intactness, MDR size, density, and density CV
      - Calculate AIC for model with and without covariate
      - Use representative networks only
