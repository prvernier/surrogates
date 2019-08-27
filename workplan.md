**Surrogates workplan**

Priority tasks for 28-Aug-2019:

- Revise manuscript (focus on methods and results tables) and circulate (see Shiny app too)
- Implement rep vs nonrep analysis and add tab to Shiny app [DONE]
- If time, try clusters (2, 4, 6); thoughts about individuals vs clusters [NO TIME]



Results - discussion points

- We now have two sets of results:
  - those based on a random sample of all networks (full variation sampled)
  - those based on all representative (KS/BC <= 0.2) and all non-representative (KS/BC > 0.2) networks.
- For the latter, we have 2 sub approaches:
  - Each network is characterized by four continuous surrogate values i.e., KS and BC
  - Each network is characterized by one binary indicator (rep=1 or rep=0)
- Indicator approach, where we use "rep" to indicate if a network is representative (rep=1) or not (rep=1) often has too few cases where rep=1. This complicates interpretation.
- We would have more representative networks if we dropped the sumGap90 criteria. In one sense, using this criteria seems odd since it is also based on the KS and BC metrics which are used to the other criteria i.e., KS and BC <= 0.2 for a representative network.
- Using the underlying KS and BC values works better but only represents the two extremes of a continuous multivariate distribution of KS and BC values.
- Currently, using the regression approach we sample 1000 networks randomly, and among those, there are representative and non-representative networks and a bunch in between.
- We could include all rep and non-rep and then sample only the in-between cases where at least one surrogate is <= 0.2 and one is > 0.2.
- Do we pursue clustering or not? I haven't had time to try clusters based on species densities and cv of densities yet. If we were to pursue it, my preference would be to use the underlying point count data rather than the prediction surface. 
- We could add up to 20 more ecoregions to the analysis, which would fill some of the holes in the map. To do this means:
  - Marc would need to identify non-overlapping networks by calculating the fields grid10km, top10km, and ratio10km for those ecoregions.
  - Those ecoregions would not include representative networks but would include networks with between 1-3 surrogates with KS/BC values <=0.2 and possibly some with 4 surrogates meeting those targets but whose sumGap90 > 0.