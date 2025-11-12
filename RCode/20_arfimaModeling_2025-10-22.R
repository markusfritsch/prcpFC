
################################################################
###	R-Basics and data
################################################################

###
###	Basics
###


###	Load libraries

#	install.packages("dplyr")
	library(dplyr)
#	install.packages("forecast")
	library(forecast)
#	install.packages("xtable")
	library(xtable)
#	install.packages("devtools")
	library(devtools)
	install_github("markusfritsch/prcpFC")
	library(prcpFC)







###	Clear Workspace

rm(list=ls())








###
###	Load rainfall data and compile anomaly data
###

###	Compile anomaly data based on imputed time series

data("raindata6024")
dat.tmp			<- raindata6024[,-1]
mean(colMeans(dat.tmp))
median(colMeans(dat.tmp))

monthly_refMeans	<- function(x){
  refMeans	 <- rowMeans(matrix(x[1:360], nrow = 12))							# 1960-01 to 1989-12
  return(refMeans)
}

monthly_refMedians	<- function(x){
  mat.tmp		<- matrix
  refMedians	<- apply(X = matrix(x[1:360], nrow=12), FUN = median, MARGIN = 1)		# 1960-01 to 1989-12
  return(refMedians)
}

refMeans	<- sapply(dat.tmp, function(x) monthly_refMeans(unname(unlist(x))))
refMeds		<- sapply(dat.tmp, function(x) monthly_refMedians(unname(unlist(x))))

dat_demeaned	<- dat.tmp - apply(refMeans, FUN = rep, MARGIN = 2, times = 65)
dat_demeded	<- dat.tmp - apply(refMeds, FUN = rep, MARGIN = 2, times = 65)

#dat_demeaned	<- dat.tmp - apply(refMeans, FUN = rep, MARGIN = 2, times = 64)		#when using 'Tend' = 2023-12-31
#dat_demeded	<- dat.tmp - apply(refMeds, FUN = rep, MARGIN = 2, times = 64)		#when using 'Tend' = 2023-12-31

#saveRDS(object = dat_demeaned, file = "raindata_demeaned_2025-01-16.rds")
#saveRDS(object = dat_demeded, file = "raindata_demeded_2025-01-16.rds")






###
###	Load station meta data with climate zone classification and generate objects with station IDs for world regions
###


###	Read in station meta data with koppen climate zone classification

data(ghcndStations)
stat2.2 		<- cbind(ghcndStations, ghcndStations$statID %in% colnames(dat_demeded))
names(stat2.2)[12]	<- "is.in.data"



###	Create objects with station IDs according to region

statAll	<- stat2.2$statID[stat2.2$is.in.data]
statAll.id	<- substr(stat2.2$statID[stat2.2$is.in.data], start = 1, stop = 2)
#all stations that contain no more missing vales after imputation

#Australia
statAus	<- "AS"
statAus.id	<- grep(pattern = statAus, x = statAll.id)
length(statAus.id)
statID.Aus	<- stat2.2$statID[stat2.2$statID %in% statAll[statAus.id]]

#Europe
statEU	<- c("AL", "BO", "BE", "BK", "BU", "DA", "GM", "EN",
"FI", "FR", "GG", "GR", "HR", "EI", "IC", "IT", "LG", "LH", "LU",
"MT", "MD", "MJ", "NL", "MK", "NO", "AU", "PL", "PO", "RO", "SW",
"SZ", "RI", "SI", "LO", "SP", "EZ", "UP", "HU", "UK")
statEU.id	<- grep(pattern = paste(statEU, collapse = "|"), x = statAll.id)
length(statEU.id)
statID.EU	<- stat2.2$statID[stat2.2$statID %in% statAll[statEU.id]]

#United States
usStatesCont	<- c(
				"AL",
#				"AK",		# Alaska
				"AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
#				"HI",		#Hawaii
				"ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
				"MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
				"NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
				"OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
				"UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

usStatesAll	<- c(
				"AL",
				"AK",		# Alaska
				"AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
				"HI",		#Hawaii
				"ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
				"MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
				"NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
				"OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
				"UT", "VT", "VA", "WA", "WV", "WI", "WY",
				"AS", "GU", "MP", "PR", "VI"
)

statID.usCont		<- stat2.2$statID[(stat2.2$stateUS %in% usStatesCont) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
statID.usContN		<- stat2.2$statID[!(stat2.2$stateUS %in% usStatesCont) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
statID.usAll		<- stat2.2$statID[(stat2.2$stateUS %in% usStatesAll) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
statID.usAllN		<- stat2.2$statID[!(stat2.2$stateUS %in% usStatesAll) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]

statID.USall	<- statID.usAll[statID.usAll %in% statAll]
statID.UScont	<- statID.usCont[statID.usCont %in% statAll]
length(statID.USall)
length(statID.UScont)


#all stations in Australia
stat_Aus		<- stat2.2[stat2.2$statID %in% statID.Aus, ]
nrow(stat_Aus)
sum(is.na(stat_Aus$koppen2))
statID.AusWOna	<- stat_Aus[!is.na(stat_Aus$koppen2), "statID"]
length(statID.AusWOna)
length(statID.Aus)


#all stations in Europe
stat_EU		<- stat2.2[stat2.2$statID %in% statID.EU, ]
nrow(stat_EU)
sum(is.na(stat_EU$koppen2))
statID.EUWOna	<- stat_EU[!is.na(stat_EU$koppen2), "statID"]
length(statID.EUWOna)
length(statID.EU)


#all stations in US
stat_usAll		<- stat2.2[stat2.2$statID %in% statID.USall, ]
nrow(stat_usAll)
sum(is.na(stat_usAll$koppen2))
statID.USallWOna	<- stat_usAll[!is.na(stat_usAll$koppen2), "statID"]
length(statID.USallWOna)
length(statID.USall)


#all stations in contiguous US
stat_usCont		<- stat2.2[stat2.2$statID %in% statID.UScont, ]
nrow(stat_usCont)
sum(is.na(stat_usCont$koppen2))
statID.UScontWOna	<- stat_usCont[!is.na(stat_usCont$koppen2), "statID"]
length(statID.UScontWOna)
length(statID.UScont)

#save("statID.Aus", "statID.EU", "statID.USall", "statID.UScont",
#	file = "D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/statIDregions.RData")
#save("statID.AusWOna", "statID.EUWOna", "statID.USallWOna", "statID.UScontWOna",
#	file = "D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/statIDregionsWOnas.RData")

stat_usAll		<- stat_usAll[!is.na(stat_usAll$koppen2), ]
stat_usCont		<- stat_usCont[!is.na(stat_usCont$koppen2), ]

#stat_usContmT	<- usmap_transform(stat_usCont)
#stat_usAllmT	<- usmap_transform(stat_usAll)





###
###	Random subsample of time series for empirical evaluation of d estimates and rule of thumb
###


#employ koppen climate zones to sample from precipitation anomaly data
data("rainresults")
dat.tmp	<- rainresults[rainresults$statID %in% colnames(raindata6024)[-1], ]
nrow(dat.tmp[dat.tmp$ddiff != 0, ])/nrow(dat.tmp)
#fraction of stations in dataset where d estimates differ

#set seed and extract stations for random subsampling
n_perCZ		<- 3
seed			<- 42
dat.tmp_rs1		<- dat.tmp[(dat.tmp$statID %in% colnames(dat_demeded)) & dat.tmp$ddiff != 0 & dat.tmp$statID %in% statID.usCont & !is.na(dat.tmp$zone_c) & dat.tmp$bestd < -0.1, ]
dat.tmp_rs2		<- dat.tmp[(dat.tmp$statID %in% colnames(dat_demeded)) & dat.tmp$ddiff != 0 & dat.tmp$statID %in% statID.usCont & !is.na(dat.tmp$zone_c) & (dat.tmp$bestd > -0.1 & dat.tmp$bestd < 0.1), ]
dat.tmp_rs3		<- dat.tmp[(dat.tmp$statID %in% colnames(dat_demeded)) & dat.tmp$ddiff != 0 & dat.tmp$statID %in% statID.usCont & !is.na(dat.tmp$zone_c) & dat.tmp$bestd > 0.1, ]

#stations in main climate zone B
dat.tmp_rs1[dat.tmp_rs1$zone_c == "B", ]
dat.tmp_rs2[dat.tmp_rs2$zone_c == "B", ]
dat.tmp_rs3[dat.tmp_rs3$zone_c == "B", ]
set.seed(seed)
id.vec_B1	<- sample(1:nrow(dat.tmp_rs1[dat.tmp_rs1$zone_c == "B", ]))
id.vec_B2	<- sample(1:nrow(dat.tmp_rs2[dat.tmp_rs2$zone_c == "B", ]))
id.vec_B3	<- sample(1:nrow(dat.tmp_rs3[dat.tmp_rs3$zone_c == "B", ]))

#stations in main climate zone C
dat.tmp_rs1[dat.tmp_rs1$zone_c == "C", ]
dat.tmp_rs2[dat.tmp_rs2$zone_c == "C", ]
dat.tmp_rs3[dat.tmp_rs3$zone_c == "C", ]
id.vec_C1	<- sample(1:nrow(dat.tmp_rs1[dat.tmp_rs1$zone_c == "C", ]))
id.vec_C2	<- sample(1:nrow(dat.tmp_rs2[dat.tmp_rs2$zone_c == "C", ]))
id.vec_C3	<- sample(1:nrow(dat.tmp_rs3[dat.tmp_rs3$zone_c == "C", ]))

#stations in main climate zone D
dat.tmp_rs1[dat.tmp_rs1$zone_c == "D", ]
dat.tmp_rs2[dat.tmp_rs2$zone_c == "D", ]
dat.tmp_rs3[dat.tmp_rs3$zone_c == "D", ]
id.vec_D1	<- sample(1:nrow(dat.tmp_rs1[dat.tmp_rs1$zone_c == "D", ]))
id.vec_D2	<- sample(1:nrow(dat.tmp_rs2[dat.tmp_rs2$zone_c == "D", ]))
id.vec_D3	<- sample(1:nrow(dat.tmp_rs3[dat.tmp_rs3$zone_c == "D", ]))

#subsample from d estimates and combine subsamples of the three main climate zones
dat_rs	<- rbind(dat.tmp_rs1[dat.tmp_rs1$zone_c == "B", ][id.vec_B1 < n_perCZ+1, ],
			dat.tmp_rs2[dat.tmp_rs2$zone_c == "B", ][id.vec_B2 < n_perCZ+1, ],
			dat.tmp_rs3[dat.tmp_rs3$zone_c == "B", ][id.vec_B3 < n_perCZ+1, ],
			dat.tmp_rs1[dat.tmp_rs1$zone_c == "C", ][id.vec_C1 < n_perCZ+1, ],
			dat.tmp_rs2[dat.tmp_rs2$zone_c == "C", ][id.vec_C2 < n_perCZ+1, ],
			dat.tmp_rs3[dat.tmp_rs3$zone_c == "C", ][id.vec_C3 < n_perCZ+1, ],
			dat.tmp_rs1[dat.tmp_rs1$zone_c == "D", ][id.vec_D1 < n_perCZ+1, ],
			dat.tmp_rs2[dat.tmp_rs2$zone_c == "D", ][id.vec_D2 < n_perCZ+1, ],
			dat.tmp_rs3[dat.tmp_rs3$zone_c == "D", ][id.vec_D3 < n_perCZ+1, ])

#extract respective columns of 'dat_demeded' (actual time series data)
dat_demeded_rs	<- dat_demeded[, colnames(dat_demeded) %in% dat_rs$statID]

#extract respective columns of 'refMeds' (monthly Medians for computing anomalies)
refMeds_rs		<- refMeds[, colnames(refMeds) %in% dat_rs$statID]

#save dataset
#save(dat_rs, dat_demeded_rs, refMeds_rs, file = "raindata_demeded_RS_2025-01-16.RData")














###
###	Tab. 5-7: Generate forecasts for precipitation anomalies and total precipitation for all monitoring stations
###


#define length of evaluation period, steps ahead and forecast window size
n.yrs		<- 5		#years for which forecasts are generated
h		<- 12		#maximum steps ahead for which forecasts are generated
w		<- 20		#forecast window size


dat_demeded	<- dat_demeded[, colnames(dat_demeded) %in% rainresults$statID]


#objects to save 1,...,12 step ahead forecasts (one row for each time period in which a forecast is made;
# one column for each h; third dimension: one entry for each time series/location at which a forecast is made)
resArr_dLW		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
fcErrArr_dLW	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
resArr_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
fcErrArr_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
resArr_drot		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
fcErrArr_drot	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)


#object for forecast evaluation (actual precipitation anomalies structured according to forecast objects)
evalAno20to24	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)

#insert entries in evaluation object
for(i in 1:ncol(dat_demeded)){

  for(j in 1:(nrow(dat_demeded[721:780, ])+1)){

    t.start			<- 720 + j
    t.end			<- 720 + j+h-1

    evalAno20to24[j,,i]	<- dat_demeded[t.start:t.end, i]

  }

}


#generate 1,...,12 step ahead forecasts according to arfima model based on dLW, bestd and drot
dat_res	<- rainresults[rainresults$statID %in% colnames(raindata6024), ]

for(i in 1:nrow(dat_res)){

  stat.tmp		<- dat_res$statID[i]
  ts.tmp		<- dat_demeded[, i]
  dLW.tmp		<- dat_res$LW[i]
  bestd.tmp		<- dat_res$bestd[i]

  for(j in 1:(n.yrs*h+1)){

    ti		<- (60-w)*12+j
    tend		<- 60*12+j-1
    dat.tmp		<- ts.tmp[ti:tend]
    m.dLW.tmp	<- forecast::arfima(y = dat.tmp, drange = c(dLW.tmp, dLW.tmp), estim = "mle")
    m.bestd.tmp	<- forecast::arfima(y = dat.tmp, drange = c(bestd.tmp, bestd.tmp), estim = "mle")
    m.drot.tmp	<- forecast::arfima(y = dat.tmp, drange = c(0.499, 0.499), estim = "mle")         #for getting rid of error message for time series 326; results virtually identical
#    m.drot.tmp	<- forecast::arfima(y = dat.tmp, drange = c(0.499999, 0.499999), estim = "mle")   #line employed to get results reported in the paper

    # compute 1,2,...,12 step ahead forecasts of anomalies with arfima function of forecast package

    resArr_dLW[j,, i]		<- as.numeric(forecast(m.dLW.tmp, h = h)$mean)
    resArr_bestd[j,, i]		<- as.numeric(forecast(m.bestd.tmp, h = h)$mean)
    resArr_drot[j,, i]		<- as.numeric(forecast(m.drot.tmp, h = h)$mean)

  }

}
#---
# Note
#--  
#There is an error message displayed for time series 326.
#According to the documentation in the forecast package this
#is due to the parameter estimates being refined by the 
#the stats::arima function when argument
#'estim = "mle"' is employed.
#However, the error cannot be reproduced when running the
#source code of the forecast package (i.e., the source
#code of function forecast::arfima). Note that as opposed to
#the package documentation, the function 'Arima()' from the
#forecast package is employed to refine the parameter estimates.
#Also not that the error disappears when reducing the argument
#'drange' to 0.499 instead of the currently employed 0.499999,
#while the estimated AR and MA parts of the models are almost
#identical
#---
  

#compute forecast errors for evaluation period (January 2020 until December 2024)
fcErrArr_dLW	<- evalAno20to24 - resArr_dLW
fcErrArr_bestd	<- evalAno20to24 - resArr_bestd
fcErrArr_drot	<- evalAno20to24 - resArr_drot


#compute squared forecast errors for evaluation period (January 2020 until December 2024)
sqfcErrArr_dLW	<- (evalAno20to24 - resArr_dLW)^2
sqfcErrArr_bestd	<- (evalAno20to24 - resArr_bestd)^2
sqfcErrArr_drot	<- (evalAno20to24 - resArr_drot)^2

#station names (third dimension of results and forecast error arrays)
dat_res$statID


#set up results objects and compute mean square forecast error
res.mat_dLW		<- matrix(NA, nrow = nrow(resArr_dLW[-1,,]), ncol = 12)
res.mat_bestd	<- matrix(NA, nrow = nrow(resArr_dLW[-1,,]), ncol = 12)
res.mat_drot	<- matrix(NA, nrow = nrow(resArr_dLW[-1,,]), ncol = 12)

for(j in 1:ncol(resArr_dLW)){

  for(i in 1:nrow(resArr_dLW[-1,,])){

    res.mat_dLW[i, j]	<- sqrt(mean(sqfcErrArr_dLW[i,j,]))
    res.mat_bestd[i, j]	<- sqrt(mean(sqfcErrArr_bestd[i,j,]))
    res.mat_drot[i, j]	<- sqrt(mean(sqfcErrArr_drot[i,j,]))

  }

}

#h-step root mean square forecast errors for each time period (averaged over monitoring stations)

#h-step root mean square forecast errors (averaged over time periods)
colMeans(res.mat_dLW, na.rm = TRUE)
colMeans(res.mat_bestd, na.rm = TRUE)
colMeans(res.mat_drot, na.rm = TRUE)

#check: ok
#1/59 * sum(res.mat_dLW[-60,2])
#1/58 * sum(res.mat_bestd[-c(59,60),3])
#1/57 * sum(res.mat_drot[-c(58,59,60),4])

#table of root mean square forecast errors
mat	<- matrix(nrow = 3, ncol = 12)
mat[1,]	<- colMeans(res.mat_dLW, na.rm = TRUE)
mat[2,]	<- colMeans(res.mat_bestd, na.rm = TRUE)
mat[3,]	<- colMeans(res.mat_drot, na.rm = TRUE)

xtable(mat, digits = 0)
#object averages over all monitoring stations in all climate zones, memory behavior, and countries





#extract forecast errors and squared forecast errors of contiguous US only
id.vec			<- colnames(dat_demeded) %in% statID.UScontWOna

resArr_dLW_US		<- resArr_dLW[,,id.vec]
resArr_bestd_US		<- resArr_bestd[,,id.vec]
resArr_drot_US		<- resArr_drot[,,id.vec]

fcErrArr_dLW_US		<- fcErrArr_dLW[,,id.vec]
fcErrArr_bestd_US	<- fcErrArr_bestd[,,id.vec]
fcErrArr_drot_US	<- fcErrArr_drot[,,id.vec]

sqfcErrArr_dLW_US	<- sqfcErrArr_dLW[,,id.vec]
sqfcErrArr_bestd_US	<- sqfcErrArr_bestd[,,id.vec]
sqfcErrArr_drot_US	<- sqfcErrArr_drot[,,id.vec]


#set up results objects for evaluation based on climate zone and memory behavior and compute mean square forecast error
#objects for table for main climate zone B
res.mat_dLW_Bap		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_dLW_Bsm		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_dLW_Blm		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Bap	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Bsm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Blm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Bap	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Bsm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Blm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)

#objects for table for main climate zone C
res.mat_dLW_Cap		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_dLW_Csm		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_dLW_Clm		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Cap	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Csm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Clm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Cap	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Csm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Clm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)

#objects for table for main climate zone D
res.mat_dLW_Dap		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_dLW_Dsm		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_dLW_Dlm		<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Dap	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Dsm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_bestd_Dlm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Dap	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Dsm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)
res.mat_drot_Dlm	<- matrix(NA, nrow = nrow(resArr_dLW_US[-1,,]), ncol = 12)

data(rainresults)
dat_res_US		<- rainresults[rainresults$statID %in% statID.UScontWOna, ]

id.vec_Bap		<- dat_res_US$zone_c == "B" & dat_res_US$bestd < -0.1
id.vec_Bsm		<- dat_res_US$zone_c == "B" & dat_res_US$bestd >= -0.1 & dat_res_US$bestd <= 0.1
id.vec_Blm		<- dat_res_US$zone_c == "B" & dat_res_US$bestd >= 0.1
id.vec_Cap		<- dat_res_US$zone_c == "C" & dat_res_US$bestd < -0.1
id.vec_Csm		<- dat_res_US$zone_c == "C" & dat_res_US$bestd >= -0.1 & dat_res_US$bestd <= 0.1
id.vec_Clm		<- dat_res_US$zone_c == "C" & dat_res_US$bestd >= 0.1
id.vec_Dap		<- dat_res_US$zone_c == "D" & dat_res_US$bestd < -0.1
id.vec_Dsm		<- dat_res_US$zone_c == "D" & dat_res_US$bestd >= -0.1 & dat_res_US$bestd <= 0.1
id.vec_Dlm		<- dat_res_US$zone_c == "D" & dat_res_US$bestd >= 0.1

for(j in 1:ncol(resArr_dLW)){

  for(i in 1:nrow(resArr_dLW[-1,,])){

    res.mat_dLW_Bap[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Bap]))
    res.mat_dLW_Bsm[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Bsm]))
    res.mat_dLW_Blm[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Blm]))
    res.mat_bestd_Bap[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Bap]))
    res.mat_bestd_Bsm[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Bsm]))
    res.mat_bestd_Blm[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Blm]))
    res.mat_drot_Bap[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Bap]))
    res.mat_drot_Bsm[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Bsm]))
    res.mat_drot_Blm[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Blm]))

    res.mat_dLW_Cap[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Cap]))
    res.mat_dLW_Csm[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Csm]))
    res.mat_dLW_Clm[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Clm]))
    res.mat_bestd_Cap[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Cap]))
    res.mat_bestd_Csm[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Csm]))
    res.mat_bestd_Clm[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Clm]))
    res.mat_drot_Cap[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Cap]))
    res.mat_drot_Csm[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Csm]))
    res.mat_drot_Clm[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Clm]))

    res.mat_dLW_Dap[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Dap]))
    res.mat_dLW_Dsm[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Dsm]))
    res.mat_dLW_Dlm[i, j]	<- sqrt(mean(sqfcErrArr_dLW_US[i,j, id.vec_Dlm]))
    res.mat_bestd_Dap[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Dap]))
    res.mat_bestd_Dsm[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Dsm]))
    res.mat_bestd_Dlm[i, j]	<- sqrt(mean(sqfcErrArr_bestd_US[i,j, id.vec_Dlm]))
    res.mat_drot_Dap[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Dap]))
    res.mat_drot_Dsm[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Dsm]))
    res.mat_drot_Dlm[i, j]	<- sqrt(mean(sqfcErrArr_drot_US[i,j, id.vec_Dlm]))

  }

}

#h-step root mean square forecast errors for each time period (averaged over monitoring stations)



###
###	Figures on number of times series in each group provided in Tab 5-7
###

#h-step root mean square forecast errors (averaged over time periods)
sum(id.vec_Bap)
colMeans(res.mat_dLW_Bap, na.rm = TRUE)
colMeans(res.mat_bestd_Bap, na.rm = TRUE)
colMeans(res.mat_drot_Bap, na.rm = TRUE)
apply(res.mat_dLW_Bap, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Bap, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Bap, FUN = sd, MARGIN = 2, na.rm = TRUE)

sum(id.vec_Bsm)
colMeans(res.mat_dLW_Bsm, na.rm = TRUE)
colMeans(res.mat_bestd_Bsm, na.rm = TRUE)
colMeans(res.mat_drot_Bsm, na.rm = TRUE)
apply(res.mat_dLW_Bsm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Bsm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Bsm, FUN = sd, MARGIN = 2, na.rm = TRUE)

sum(id.vec_Blm)
colMeans(res.mat_dLW_Blm, na.rm = TRUE)
colMeans(res.mat_bestd_Blm, na.rm = TRUE)
colMeans(res.mat_drot_Blm, na.rm = TRUE)
apply(res.mat_dLW_Blm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Blm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Blm, FUN = sd, MARGIN = 2, na.rm = TRUE)


sum(id.vec_Cap)
colMeans(res.mat_dLW_Cap, na.rm = TRUE)
colMeans(res.mat_bestd_Cap, na.rm = TRUE)
colMeans(res.mat_drot_Cap, na.rm = TRUE)
apply(res.mat_dLW_Cap, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Cap, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Cap, FUN = sd, MARGIN = 2, na.rm = TRUE)

sum(id.vec_Csm)
colMeans(res.mat_dLW_Csm, na.rm = TRUE)
colMeans(res.mat_bestd_Csm, na.rm = TRUE)
colMeans(res.mat_drot_Csm, na.rm = TRUE)
apply(res.mat_dLW_Csm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Csm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Csm, FUN = sd, MARGIN = 2, na.rm = TRUE)

sum(id.vec_Clm)
colMeans(res.mat_dLW_Clm, na.rm = TRUE)
colMeans(res.mat_bestd_Clm, na.rm = TRUE)
colMeans(res.mat_drot_Clm, na.rm = TRUE)
apply(res.mat_dLW_Clm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Clm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Clm, FUN = sd, MARGIN = 2, na.rm = TRUE)


sum(id.vec_Dap)
colMeans(res.mat_dLW_Dap, na.rm = TRUE)
colMeans(res.mat_bestd_Dap, na.rm = TRUE)
colMeans(res.mat_drot_Dap, na.rm = TRUE)
apply(res.mat_dLW_Dap, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Dap, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Dap, FUN = sd, MARGIN = 2, na.rm = TRUE)

sum(id.vec_Dsm)
colMeans(res.mat_dLW_Dsm, na.rm = TRUE)
colMeans(res.mat_bestd_Dsm, na.rm = TRUE)
colMeans(res.mat_drot_Dsm, na.rm = TRUE)
apply(res.mat_dLW_Dsm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Dsm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Dsm, FUN = sd, MARGIN = 2, na.rm = TRUE)

sum(id.vec_Dlm)
colMeans(res.mat_dLW_Dlm, na.rm = TRUE)
colMeans(res.mat_bestd_Dlm, na.rm = TRUE)
colMeans(res.mat_drot_Dlm, na.rm = TRUE)
apply(res.mat_dLW_Dlm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_bestd_Dlm, FUN = sd, MARGIN = 2, na.rm = TRUE)
apply(res.mat_drot_Dlm, FUN = sd, MARGIN = 2, na.rm = TRUE)



###
###	Tab 5-7: Tables of root mean square forecast errors and numbers on time series in each group
###

mat1		<- matrix(nrow = 9, ncol = 12)
mat1[1,]	<- colMeans(res.mat_dLW_Bap, na.rm = TRUE)
mat1[2,]	<- colMeans(res.mat_bestd_Bap, na.rm = TRUE)
mat1[3,]	<- colMeans(res.mat_drot_Bap, na.rm = TRUE)
mat1[4,]	<- colMeans(res.mat_dLW_Bsm, na.rm = TRUE)
mat1[5,]	<- colMeans(res.mat_bestd_Bsm, na.rm = TRUE)
mat1[6,]	<- colMeans(res.mat_drot_Bsm, na.rm = TRUE)
mat1[7,]	<- colMeans(res.mat_dLW_Blm, na.rm = TRUE)
mat1[8,]	<- colMeans(res.mat_bestd_Blm, na.rm = TRUE)
mat1[9,]	<- colMeans(res.mat_drot_Blm, na.rm = TRUE)

#Tab 5
xtable(mat1, digits = 0)
sum(id.vec_Bap)
sum(id.vec_Bsm)
sum(id.vec_Blm)


mat2		<- matrix(nrow = 9, ncol = 12)
mat2[1,]	<- colMeans(res.mat_dLW_Cap, na.rm = TRUE)
mat2[2,]	<- colMeans(res.mat_bestd_Cap, na.rm = TRUE)
mat2[3,]	<- colMeans(res.mat_drot_Cap, na.rm = TRUE)
mat2[4,]	<- colMeans(res.mat_dLW_Csm, na.rm = TRUE)
mat2[5,]	<- colMeans(res.mat_bestd_Csm, na.rm = TRUE)
mat2[6,]	<- colMeans(res.mat_drot_Csm, na.rm = TRUE)
mat2[7,]	<- colMeans(res.mat_dLW_Clm, na.rm = TRUE)
mat2[8,]	<- colMeans(res.mat_bestd_Clm, na.rm = TRUE)
mat2[9,]	<- colMeans(res.mat_drot_Clm, na.rm = TRUE)

#Tab 6
xtable(mat2, digits = 0)
sum(id.vec_Cap)
sum(id.vec_Csm)
sum(id.vec_Clm)


mat3		<- matrix(nrow = 9, ncol = 12)
mat3[1,]	<- colMeans(res.mat_dLW_Dap, na.rm = TRUE)
mat3[2,]	<- colMeans(res.mat_bestd_Dap, na.rm = TRUE)
mat3[3,]	<- colMeans(res.mat_drot_Dap, na.rm = TRUE)
mat3[4,]	<- colMeans(res.mat_dLW_Dsm, na.rm = TRUE)
mat3[5,]	<- colMeans(res.mat_bestd_Dsm, na.rm = TRUE)
mat3[6,]	<- colMeans(res.mat_drot_Dsm, na.rm = TRUE)
mat3[7,]	<- colMeans(res.mat_dLW_Dlm, na.rm = TRUE)
mat3[8,]	<- colMeans(res.mat_bestd_Dlm, na.rm = TRUE)
mat3[9,]	<- colMeans(res.mat_drot_Dlm, na.rm = TRUE)

#Tab7
xtable(mat3, digits = 0)
sum(id.vec_Dap)
sum(id.vec_Dsm)
sum(id.vec_Dlm)



###
###	Numbers in the text on Tab. 5-7
###

#total number of time series
15+50+11+28+180+40+48+150+22

#time series with anti-persistent or intermediate memory behavior (abs. and rel.)
15+50+28+180+48+150
(15+50+28+180+48+150)/(15+50+11+28+180+40+48+150+22)

#time series with long memory behavior (abs. and rel.)
11+40+22
(11+40+22)/(15+50+11+28+180+40+48+150+22)

#mean long-range dependence parameter estimate
mean(rainresults[(rainresults$statID %in% statID.UScontWOna) &
			(rainresults$bestd > 0.1), "bestd"])























########################################################################################################################################
###	Generate forecasts for precipitation anomalies and total precipitation based on subsample of monitoring stations
########################################################################################################################################


#define length of evaluation period, steps ahead and forecast window size
n.yrs		<- 5		#years for which forecasts are generated
h		<- 12		#maximum steps ahead for which forecasts are generated
w		<- 20		#forecast window size


#objects to save 1,...,12 step ahead forecasts
resArr_rs_dLW		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
fcErrArr_rs_dLW		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
resArr_rs_bestd		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
fcErrArr_rs_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
resArr_rs_drot		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
fcErrArr_rs_drot	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)


#object for forecast evaluation (actual precipitation anomalies structured according to forecast objects)
evalAno20to24_rs	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)

for(i in 1:ncol(dat_demeded_rs)){

  for(j in 1:(nrow(dat_demeded_rs[721:780, ])+1)){

    t.start			<- 720 + j
    t.end			<- 720 + j+h-1

    evalAno20to24_rs[j,,i]		<- dat_demeded_rs[t.start:t.end, i]

  }

}




#generate 1,...,12 step ahead forecasts according to arfima model based on dLW, bestd and drot
for(i in 1:nrow(dat_rs)){

  stat.tmp		<- dat_rs$statID[i]
  ts.tmp		<- dat_demeded_rs[, i]
  dLW.tmp		<- dat_rs$LW[i]
  bestd.tmp		<- dat_rs$bestd[i]
  assign(paste("list.m.dLW_rs_", i, sep = ""), value = list())
  assign(paste("list.m.bestd_rs_", i, sep = ""), value = list())
  assign(paste("list.m.drot_rs_", i, sep = ""), value = list())

  for(j in 1:(n.yrs*h+1)){

    ti		<- (60-w)*12+j
    tend		<- 60*12+j-1
    dat.tmp		<- ts.tmp[ti:tend]
    m.dLW.tmp	<- forecast::arfima(y = dat.tmp, drange = c(dLW.tmp, dLW.tmp), estim = "mle")
    assign(paste("list.m.dLW_rs_", i, sep = ""), value = c(get(paste("list.m.dLW_rs_", i, sep = "")), coef(m.dLW.tmp)))
    m.bestd.tmp	<- forecast::arfima(y = dat.tmp, drange = c(bestd.tmp, bestd.tmp), estim = "mle")
    assign(paste("list.m.bestd_rs_", i, sep = ""), value = c(get(paste("list.m.bestd_rs_", i, sep = "")), coef(m.bestd.tmp)))
#    m.drot.tmp	<- forecast::arfima(y = dat.tmp, drange = c(0.499, 0.499), estim = "mle")              #for consistency with code above (gets rid of error message for time series 326: not part of forecasting set)
    m.drot.tmp	<- forecast::arfima(y = dat.tmp, drange = c(0.499999, 0.499999), estim = "mle")
    assign(paste("list.m.drot_rs_", i, sep = ""), value = c(get(paste("list.m.drot_rs_", i, sep = "")), coef(m.drot.tmp)))

    # compute 1,2,...,12 step ahead forecasts of anomalies with arfima function of forecast package

    resArr_rs_dLW[j,, i]		<- as.numeric(forecast(m.dLW.tmp, h = h)$mean)
    resArr_rs_bestd[j,, i]		<- as.numeric(forecast(m.bestd.tmp, h = h)$mean)
    resArr_rs_drot[j,, i]		<- as.numeric(forecast(m.drot.tmp, h = h)$mean)

  }

}


#compute forecast errors for evaluation period (January 2020 until December 2024)
fcErrArr_rs_dLW		<- evalAno20to24_rs - resArr_rs_dLW
fcErrArr_rs_bestd	<- evalAno20to24_rs - resArr_rs_bestd
fcErrArr_rs_drot	<- evalAno20to24_rs - resArr_rs_drot


#compute squared forecast errors for evaluation period (January 2020 until December 2024)
sqfcErrArr_rs_dLW	<- (evalAno20to24_rs - resArr_rs_dLW)^2
sqfcErrArr_rs_bestd	<- (evalAno20to24_rs - resArr_rs_bestd)^2
sqfcErrArr_rs_drot	<- (evalAno20to24_rs - resArr_rs_drot)^2

#station names (third dimension of results and forecast error arrays)
dat_rs$statID


#set up results objects and compute mean square forecast error
res.mat_rs_dLW		<- matrix(NA, nrow = nrow(resArr_rs_dLW[-1,,]), ncol = 12)
res.mat_rs_bestd	<- matrix(NA, nrow = nrow(resArr_rs_dLW[-1,,]), ncol = 12)
res.mat_rs_drot		<- matrix(NA, nrow = nrow(resArr_rs_dLW[-1,,]), ncol = 12)

for(j in 1:ncol(resArr_rs_dLW)){

  for(i in 1:nrow(resArr_rs_dLW[-1,,])){

    res.mat_rs_dLW[i, j]	<- sqrt(mean(sqfcErrArr_rs_dLW[i,j,]))
    res.mat_rs_bestd[i, j]	<- sqrt(mean(sqfcErrArr_rs_bestd[i,j,]))
    res.mat_rs_drot[i, j]	<- sqrt(mean(sqfcErrArr_rs_drot[i,j,]))

  }

}

#h-step root mean square forecast errors for each time period (averaged over monitoring stations)

#h-step root mean square forecast errors (averaged over time periods)
colMeans(res.mat_rs_dLW, na.rm = TRUE)
colMeans(res.mat_rs_bestd, na.rm = TRUE)
colMeans(res.mat_rs_drot, na.rm = TRUE)

#check: ok
#1/59 * sum(res.mat_rs_dLW[-60,2])
#1/58 * sum(res.mat_rs_bestd[-c(59,60),3])
#1/57 * sum(res.mat_rs_drot[-c(58,59,60),4])

#table of root mean square forecast errors
mat_rs		<- matrix(nrow = 3, ncol = 12)
mat_rs[1,]	<- colMeans(res.mat_rs_dLW, na.rm = TRUE)
mat_rs[2,]	<- colMeans(res.mat_rs_bestd, na.rm = TRUE)
mat_rs[3,]	<- colMeans(res.mat_rs_drot, na.rm = TRUE)

xtable(mat_rs, digits = 0)





#out-of-sample forecasts for 27 (=9*3) monitoring stations in random sample

#precipitation anomaly forecasts 1 to 12 steps ahead
fc_dLW_ano_rs			<- resArr_rs_dLW[61,,]
colnames(fc_dLW_ano_rs)		<- dat_rs$statID
fc_bestd_ano_rs			<- resArr_rs_bestd[61,,]
colnames(fc_bestd_ano_rs)	<- dat_rs$statID
fc_drot_ano_rs			<- resArr_rs_drot[61,,]
colnames(fc_drot_ano_rs)	<- dat_rs$statID

#tables with precipitation anomaly forecasts
xtable(t(fc_dLW_ano_rs[, order(dat_rs$bestd)]), digits = 0)
xtable(t(fc_bestd_ano_rs[, order(dat_rs$bestd)]), digits = 0)
xtable(t(fc_drot_ano_rs[, order(dat_rs$bestd)]), digits = 0)

#total precipitation forecasts 1 to 12 steps ahead
fc_dLW_tot_rs	<- fc_dLW_ano_rs + refMeds[, colnames(refMeds) %in% colnames(fc_dLW_ano_rs)]
fc_bestd_tot_rs	<- fc_bestd_ano_rs + refMeds[, colnames(refMeds) %in% colnames(fc_bestd_ano_rs)]
fc_drot_tot_rs	<- fc_drot_ano_rs + refMeds[, colnames(refMeds) %in% colnames(fc_drot_ano_rs)]


###
###	Tab B.12-B.14: Tables with total precipitation forecasts
###

#Tab B.12
xtable(t(fc_dLW_tot_rs[, order(dat_rs$bestd)]), digits = 0)

#Tab B.13
xtable(t(fc_bestd_tot_rs[, order(dat_rs$bestd)]), digits = 0)

#Tab B.14
xtable(t(fc_drot_tot_rs[, order(dat_rs$bestd)]), digits = 0)



















###
###	Additional materials: Compute out-of-sample forecasts for all monitoring stations
###

#precipitation anomaly forecasts 1 to 12 steps ahead
fc_dLW			<- resArr_dLW[61,,]
colnames(fc_dLW)	<- dat_res$statID
fc_bestd		<- resArr_bestd[61,,]
colnames(fc_bestd)	<- dat_res$statID
fc_drot			<- resArr_drot[61,,]
colnames(fc_drot)	<- dat_res$statID

#precipitation forecasts 1 to 12 steps ahead
fc_dLW + refMeds[, colnames(refMeds) %in% colnames(fc_dLW)]
fc_bestd + refMeds[, colnames(refMeds) %in% colnames(fc_bestd)]
fc_drot + refMeds[, colnames(refMeds) %in% colnames(fc_drot)]



#monitoring stations located in the contiguous United States
ncol(dat_demeded[, colnames(dat_demeded) %in% stat_usCont$statID])











