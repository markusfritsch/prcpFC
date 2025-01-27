# prcpFC: Functions and data for monthly precipitation and precipitation anomaly forecasting

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

