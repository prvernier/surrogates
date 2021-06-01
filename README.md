# Surrogates Paper

Revised: 2021-06-01


Evaluating the Effectiveness of Biodiversity Surrogates


## Manuscript

Beacons et al. <i>in prep</i>. Evaluating the Effectiveness of Biodiversity Surrogates for Conservation Planning in the Boreal Region of Canada.

## Supporting information

  - Appendix A1. Building and filtering networks
  - Appendix A2. Number of networks by ecoregion. Representative and non-representative benchmark networks in each ecoregion used in the evaluation of surrogates within the ecozones of the boreal region of Canada. Criteria used to differentiate representative and non-representative networks are described in the methods section. Networks were also filtered to reduce spatial overlap. The number of ecoregions used in the analysis varied by species based on the extent of their ranges.
  - Appendix A3. Species names and assemblages. Common and scientific names and assemblage membership of test features (species) used in the evaluation of surrogates. Many of the species were also combined into habitat or nesting assemblages (Barker et al. 2014). The forest habitat assemblage includes 53 species that are primarily associated with forested habitats (Stralberg et al. 2015).
  - TableS3a_rep_meanDissim.csv
  - TableS3b_rep_sumDensity.csv

## Case Study

  - [Ecoregion 89 case study: Evaluating the effectiveness of surrogates](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp/case_study_89.html)

## Shiny app

We developed a R Shiny app to enable readers and conservation planners in the boreal region to explore the results of the analysis and to identify species/assemblages and bird conservation regions (BCRs) combinations that are adequately or inadequately represented by benchmark networks selected using surrogates of large-scale environmental variation. To run the app from a local machine:

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
      - Calculate effect size = (mean of DMrep) � (mean of DMnonrep) / std dev of DM
    
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
