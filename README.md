# Surrogates Paper

Revised: 2021-02-10


Evaluating the Effectiveness of Biodiversity Surrogates


## Manuscript

Beacons et al. <i>in prep</i>. Evaluating the Effectiveness of Biodiversity Surrogates for Conservation Planning in the Boreal Region of Canada.


## Shiny app

In this study we develop 662 multiple linear regression models between test species and environmental surrogates. Due to the large amount of resultant model output, we developed a Shiny app to enable readers and conservation planners in the boreal region to explore the results of the analysis and identify species and ecoregion combinations that are adequately or inadequately represented by benchmark networks selected using surrogates of large-scale environmental variation. Here we provide instructions on how to run the app from a local machine and also provides summary tables with all input data used in the analyses and the app.

  1. Install R
  2. Install the following additional packages:

    >install.packages(c("sf","DT","caret","shiny","leaflet","tidyverse"))
  3. Start the Shiny app:

    >library(shiny)
    >runGitHub( "prvernier/surrogates", subdir="shiny")


## Supporting information

* [Appendix S1 and S2. Construction and filtering of benchmark networks.](https://github.com/prvernier/surrogates/blob/master/supp_info/appendix_s1s2.md)
* [Table S1. Bird species common and latin names](https://github.com/prvernier/surrogates/blob/master/supp_info/table_s1.md)
* [Table S2. Summary of multiple linear regression models relating surrogates KS values to species KS values for all ecoregions with representative and non-representative networks in the boreal region of Canada.](https://github.com/prvernier/surrogates/blob/master/supp_info/table_s2.md)
* [Case study: Using R to evaluating the effectiveness of surrogates in ecoregion ([Rmarkdown](supp_info/case_study.Rmd) or [HTML](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp_info/case_study.html))


## Code

The following scripts can be used to replicate the analysis. Links to download the required datasets can be found in the [s1_datasets.md](https://github.com/beacons/intactness/blob/master/s1_datasets.md). Check the top of each script to determine which datasets are needed. If you have any questions, please contact: pierre.vernier@gmail.com

### Prepare data

  * 00_check_spp.R
    - Check to make sure we have all the proper scientific names and make modifications where needed
  
  * 01_gen_data.R
    - Generate caribou and bird raster maps for analysis with common projection etc.
    - Sum rasters by bird assemblages

### Calculate dissimilarity

  * 02_calc_ks.R
    - Calculate KS statistic for representative and non-representative networks for all ecoregions and species groups
    - Originally used velox package, which is no longer maintained; changed to exact_extracr package

  * 03_merge_data.R
    - Prepare data for statistical analysis
    - Merge ecozone-level data containing dissimilarity metrics for species and surrogates
    - For each ecozone, randomly select up to 500 representative and non-representative networks

### Test hypotheses

  * 03_test_obj1&2.R
    - Obj1: Are networks representative of surrogates also representative of non-target species?
       - Use representative networks only
    - Obj2: Are representative networks more effective than non-representative networks for target species?
      - Use representative and non-representative networks
      - Use t-test or wilcox.test (t-test should be fine due to large sample sizes)
      - Calculate effect size = (mean of DMrep) – (mean of DMnonrep) / std dev of DM
    
  * 04_test_obj3.R
    - Obj3: Does the relative importance of environmental surrogates vary by groups of species?
      - Use representative networks only
      - Model species DM as a function of CMI + GPP + LED + LCC
      - Count number of times each surrogate is the most important variable

  *  05_test_obj4.R
    - Obj4: Is the relationship between surrogates and non-target species influenced by other variables?
      - Test ecoregion membership, intactness, MDR size, density, and density CV
      - Calculate AIC for model with and without covariate
      - Use representative networks only
