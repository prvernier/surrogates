### Surrogates Workplan



**Manuscript**

- The latest version of the manuscript and the shiny app can be found on Dropbox ("Dropbox (BEACONs)\BEACONs Share\surrogates"). Code and supp info can be found on Github ("https://github.com/prvernier/surrogates")
- I'm currently revising the methods but waiting for feedback from this meeting before finishing methods sections and working on results section.



**Clustering on species density**

- If time, try clusters (2, 4, 6); thoughts about individuals vs clusters
  - Do we pursue clustering or not? I haven't had time to try clusters based on species densities and cv of densities yet. If we were to pursue it, my preference would be to use the underlying point count data rather than the prediction surface. 



**Statistical analyses**

- Completed the rep/non representation analysis and added this functionality to Shiny.
- Shiny can now also run analyses on the fly - select one test species and regression analysis is conducted for all ecoregions where that species occurs. The results can be viewed as a table and the R-squared values as a map. The table can also be downloaded to a csv file.
- Another change relates to the selection of ecoregions for the 11 songbirds. The songbird range is no longer used to clip the density map but to select ecoregions that intersect it. This means that we no longer have to deal with networks and ecoregions that are missing up to half of their density data. It also recognizes that that range boundaries are soft and not hard as suggested by the maps.
- We now have two sets of results:
  - those based on a random sample of all networks (full variation sampled)
  - those based on all representative (KS/BC <= 0.2) and all non-representative (KS/BC > 0.2) networks.
- For the latter, we have 2 sub approaches:
  - Each network is characterized by four continuous surrogate values i.e., KS and BC
  - Each network is characterized by one binary indicator (rep=1 or rep=0)
- Indicator approach, where we use "rep" to indicate if a network is representative (rep=1) or not (rep=1) often has too few cases where rep=1. In cases with few rep networks, results have little power and are not reliable. We're better off using underlying KS/BC values.
- We would have more representative networks if we dropped the sumGap90 criteria. In one sense, using this criteria seems odd since it is also based on the KS and BC metrics which are used to the other criteria i.e., KS and BC <= 0.2 for a representative network.
- Using the underlying KS and BC values works better but only represents the two extremes of a continuous multivariate distribution of KS and BC values.
- Currently, using the regression approach we sample 1000 networks randomly, and among those, there are representative and non-representative networks and a bunch in between.
- We could include all rep and non-rep and then sample only the in-between cases where at least one surrogate is <= 0.2 and one is > 0.2.
- We could add up to 20 more ecoregions to the analysis, which would fill some of the holes in the map. To do this means:
  - Marc would need to identify non-overlapping networks by calculating the fields grid10km, top10km, and ratio10km for those ecoregions.
  - Those ecoregions would not include representative networks but would include networks with between 1-3 surrogates with KS/BC values <=0.2 and possibly some with 4 surrogates meeting those targets but whose sumGap90 > 0.