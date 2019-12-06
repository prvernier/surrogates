# Surrogates Paper

Revised: 2019-12-06

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

## Supporting Information

* [Table S1. Bird species common and latin names](https://github.com/prvernier/surrogates/blob/master/supp_info/table_s1.md)
* [Table S2. Summary of multiple linear regression models relating surrogates KS values to species KS values for all ecoregions with representative and non-representative networks in the boreal region of Canada.](https://github.com/prvernier/surrogates/blob/master/supp_info/table_s2.md)
* [Appendix S2. Using R to evaluating the effectiveness of surrogates in ecoregion 89 (Rmarkdown)](demo_evaluating_surrogates.Rmd)
* [Appendix S2. Using R to evaluating the effectiveness of surrogates in ecoregion 89 (html)](https://htmlpreview.github.io/?https://github.com/prvernier/surrogates/blob/master/supp_info/demo_evaluating_surrogates.html)



