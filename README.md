# Surrogates Paper

Revised: 2021-12-08


Evaluating the Effectiveness of Biodiversity Surrogates


## Manuscript

Beacons et al. <i>in prep</i>. Evaluating the Effectiveness of Biodiversity Surrogates for Conservation Planning in the Boreal Region of Canada.

## Supporting information

  - [Appendix S1](supp/S1_species_and_assemblages.csv). Table listing common and scientific names of for test species along with their assemblage membership(s).
  - [Appendix S2](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp/S2_building_and_filtering.html). Construction and filtering of benchmark networks.
  - [Appendix S3](supp/S3_networks_and_ecoregions.csv). Table summarizing the number of networks by ecoregion.
  - [Appendix S4a](supp/S4a_rep_sumDensity.csv). Table of results for objectives 1 and 2 using the sum of density approach.
  - [Appendix S4a](supp/S4b_rep_meanDissim.csv). Table of results for objectives 1 and 2 using the mean dissimilarity approach.

## Case Study

  - [Case study 1](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp/case_study_1.html). Case study comparing two methods used to evaluate representation.
  - [Case study 2](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp/case_study_2.html). Case study illustrating the difference between evaluating representativeness and representation.

## Shiny app

We developed a R Shiny app to enable readers and conservation planners in the boreal region to explore the results of the analysis and to identify species/assemblages and bird conservation regions (BCRs) combinations that are adequately or inadequately represented by benchmark networks selected using environmental surrogates. To run the app from a local machine:

  1. Install R (download from r-project.org and follow instructions)
  2. Install the following additional packages:

    >install.packages(c("sf","tmap","ggplot2","tidyverse","shinydashboard"))

  3. Start the Shiny app:

    >library(shiny)
    >runGitHub( "prvernier/surrogates", subdir="shiny")


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
