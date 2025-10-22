# prcpFC: Functions and data for monthly precipitation and precipitation anomaly forecasting

Markus Fritsch

## Contents

The package reproduces all results reported in
Fritsch, Haupt, Flock, Schnurbus, and Sibbertsen (2025): The memory puzzle in precipitation: Uncertainty in memory
parameter estimation and implications for forecasting practice. _Working Paper_.
Based on the methodology described in the paper, the package analyzes the memory
properties of precipitation series and provides two sets of monthly precipitation
and precipitation anomaly forecasts for the locations of the monitoring stations
of the Global Historical Climatology Network daily (GHCNd):
For the evaluation period from January 2020 until December 2024 and
out-of-sample forecasts for January 2025 until December 2025.

For details on the GHCNd, see
Menne et al. (2012) https://doi.org/10.1175/JTECH-D-11-00103.1,
Durre et al. (2010) https://doi.org/10.1175/2010JAMC2375.1, and
Durre, Menne, and Vose (2008) https://doi.org/10.1175/2007JAMC1706.1.


To install the package please use:
```{r}
library(devtools)
install_github("markusfritsch/prcpFC")
```



## Computational details for reproducing the paper results

There are three R scripts generating the results based on the datasets included in the package.
- `10_longmemo_withPlots_2025-10-22.R`: estimates the memory parameters
- `20_arfimaModeling_2025-10-22.R`: generates and evaluates all forecasts and produces tables of results
- `30_exploratoryDataAnalysis_2025-10-22.R`: generates all further tables and figures

Please adjust the working directory in the codes to generate the figures.

All results were generated in R 4.5.1 using the add-on packages
arfima (1.8-2),
dplyr (1.1.4),
forecast (8.24.0),
fracdiff (1.5-3),
ggplot2 (4.0.0),
ggmap (4.0.2),
gridExtra (2.3),
longmemo (1.1-4),
mvtnorm (1.3-3),
partitions (1.10-9),
raster (3.6-32),
sf(1.0-21),
sp (2.2-0),
strucchange (1.5-4),
terra (1.8-70),
RcppArmadillo (15.0.2-2),
rworldmap (1.3-8),
snowfall (1.84-6.3),
usmap (1.0.0),
viridis (0.6.5),
xtable(1.8-4)






