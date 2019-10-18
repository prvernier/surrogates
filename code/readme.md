# R scripts and input tables for running surrogates analysis
# PV 2019-10-18

# CODE

Code for preparing data and running surrogates analysis on rep and non-rep networks only.

Scripts:
  - 00_check_spp.R - Check to make sure we have all the proper scientific names and make modifications where needed
  - 01_gen_data.R - Generate GIS database with common extent, projection & resolution
  - 02_net_stats.R - Generate networks statistics
  - 03_spp_stats.R - Calculate stats for each species in each ecoregion; density clipped to range maps
  - 03_spp_stats11.R - Calculate stats for each species in each ecoregion; density not clipped to range maps
  - 03_spp_stats80.R - Calculate stats for each species in each ecoregion
  - 04_ks_precalc.R - Prepare rep and nonrep shapefiles prior to calculating KS statistics
  - 05_ks_calc.R - Calculate KS for test species in rep and nonrep networks
  - 06_ks_merge.R - Merge KS statistics for rep and nonrep networks
  - 07_gen_models.R - Generate regression models for test species
  - 08_gen_results.R - Create results output tables
  - 09_shiny_data.R - Create shiny input tables

# CODE/INPUT

Input files used by some of the scripts; not all of them are used.

# CODE/BACKUP

Contains scripts that were used when all networks were analysed; also contains additional (e.g., cluster) or backup scripts
Scripts:
  - 04_zcluster.R - kmeans_clustering
  - 05_calc_rep.R - calculate KS for test species in all networks
  - 06_merge_rep.R - merge ecoregion-level ks stats
  - 07_gen_models.R - develop models for all networks
  - 10_gen_dotplot.R
  - 10_post_analysis1.R
  - 10_post_analysis2.R
  - 10_post_analysis3.R
