
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
	install.packages("devtools")
	library(devtools)
	install_github("markusfritsch/prcpFC")





###	Clear Workspace

rm(list=ls())




###	Load data

#source("00_dataPreprocessing_2025-01-28")		#note that this script needs to be run first






###
###	Generate forecasts for precipitation anomalies and total precipitation based on subsample of monitoring stations
###


#define length of evaluation period, steps ahead and forecast window size
n.yrs		<- 5		#years for which forecasts are generated
h		<- 12		#maximum steps ahead for which forecasts are generated
w		<- 20		#forecast window size


#objects to save 1,...,12 step ahead forecasts
resArr_dLW		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
fcErrArr_dLW	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
resArr_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
fcErrArr_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
resArr_drot		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)
fcErrArr_drot	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)


#object for forecast evaluation (actual precipitation anomalies structured according to forecast objects)
evalAno20to24	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded_rs)), data = NA)

for(i in 1:ncol(dat_demeded_rs)){

  for(j in 1:(nrow(dat_demeded_rs[721:780, ])+1)){

    t.start			<- 720 + j
    t.end			<- 720 + j+h-1

    evalAno20to24[j,,i]	<- dat_demeded_rs[t.start:t.end, i]

  }

}


#generate 1,...,12 step ahead forecasts according to arfima model based on dLW, bestd and drot
for(i in 1:nrow(dat_rs)){

  stat.tmp		<- dat_rs$statID[i]
  ts.tmp		<- dat_demeded_rs[, i]
  dLW.tmp		<- dat_rs$LW[i]
  bestd.tmp		<- dat_rs$bestd[i]

  for(j in 1:(n.yrs*h+1)){

    ti		<- (60-w)*12+j
    tend		<- 60*12+j-1
    dat.tmp		<- ts.tmp[ti:tend]
    m.dLW.tmp	<- forecast::arfima(y = dat.tmp, drange = c(dLW.tmp, dLW.tmp), estim = "mle")
    m.bestd.tmp	<- forecast::arfima(y = dat.tmp, drange = c(bestd.tmp, bestd.tmp), estim = "mle")
    m.drot.tmp	<- forecast::arfima(y = dat.tmp, drange = c(0.499999, 0.499999), estim = "mle")

    # compute 1,2,...,12 step ahead forecasts of anomalies with arfima function of forecast package

    resArr_dLW[j,, i]		<- as.numeric(forecast(m.dLW.tmp, h = h)$mean)
    resArr_bestd[j,, i]		<- as.numeric(forecast(m.bestd.tmp, h = h)$mean)
    resArr_drot[j,, i]		<- as.numeric(forecast(m.drot.tmp, h = h)$mean)

  }

}


#compute forecast errors for evaluation period (January 2020 until December 2024)
fcErrArr_dLW	<- evalAno20to24 - resArr_dLW
fcErrArr_bestd	<- evalAno20to24 - resArr_bestd
fcErrArr_drot	<- evalAno20to24 - resArr_drot

#station names (third dimension of results and forecast error arrays)
dat_rs$statID


#set up results objects and compute mean forecast error
res.mat_dLW		<- matrix(NA, nrow = length(dat_rs$statID), ncol = 12)
res.mat_bestd	<- matrix(NA, nrow = length(dat_rs$statID), ncol = 12)
res.mat_drot	<- matrix(NA, nrow = length(dat_rs$statID), ncol = 12)

for(i in 1:length(dat_rs$statID)){

  res.mat_dLW[i, ]	<- apply(fcErrArr_dLW[,,i], FUN = mean, MARGIN = 2, na.rm = TRUE)
  res.mat_bestd[i, ]	<- apply(fcErrArr_bestd[,,i], FUN = mean, MARGIN = 2, na.rm = TRUE)
  res.mat_drot[i, ]	<- apply(fcErrArr_drot[,,i], FUN = mean, MARGIN = 2, na.rm = TRUE)

}

#mean forecast errors
colMeans(res.mat_dLW)
colMeans(res.mat_bestd)
colMeans(res.mat_drot)

#relative forecast errors
mat		<- matrix(nrow = 3, ncol = 12)
mat[1,]	<- colMeans(res.mat_dLW)/colMeans(res.mat_bestd)
mat[2,]	<- colMeans(res.mat_bestd)/colMeans(res.mat_bestd)
mat[3,]	<- colMeans(res.mat_drot)/colMeans(res.mat_bestd)

xtable(mat)



#out-of-sample forecasts for 30 monitoring stations in random sample

#precipitation anomaly forecasts 1 to 12 steps ahead
fc_dLW_ano		<- resArr_dLW[61,,]
colnames(fc_dLW_ano)	<- dat_rs$statID
fc_bestd_ano		<- resArr_bestd[61,,]
colnames(fc_bestd_ano)	<- dat_rs$statID
fc_drot_ano		<- resArr_drot[61,,]
colnames(fc_drot_ano)	<- dat_rs$statID

xtable(t(fc_dLW_ano), digits = 0)
xtable(t(fc_bestd_ano), digits = 0)
xtable(t(fc_drot_ano), digits = 0)

#precipitation forecasts 1 to 12 steps ahead
fc_dLW_tot	<- fc_dLW_ano + refMeds[, colnames(refMeds) %in% colnames(fc_dLW)]
fc_bestd_tot	<- fc_bestd_ano + refMeds[, colnames(refMeds) %in% colnames(fc_bestd)]
fc_drot_tot	<- fc_drot_ano + refMeds[, colnames(refMeds) %in% colnames(fc_drot)]

xtable(t(fc_dLW_tot), digits = 0)
xtable(t(fc_bestd_tot), digits = 0)
xtable(t(fc_drot_tot), digits = 0)









###
###	Generate forecasts for precipitation anomalies and total precipitation for all monitoring stations
###


#define length of evaluation period, steps ahead and forecast window size
n.yrs		<- 5		#years for which forecasts are generated
h		<- 12		#maximum steps ahead for which forecasts are generated
w		<- 20		#forecast window size


#objects to save 1,...,12 step ahead forecasts
resArr_dLW		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
fcErrArr_dLW	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
resArr_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
fcErrArr_bestd	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
resArr_drot		<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)
fcErrArr_drot	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)


#object for forecast evaluation (actual precipitation anomalies structured according to forecast objects)
evalAno20to24	<- array(dim =c(n.yrs*12+1, h, ncol(dat_demeded)), data = NA)

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
    m.drot.tmp	<- forecast::arfima(y = dat.tmp, drange = c(0.499999, 0.499999), estim = "mle")

    # compute 1,2,...,12 step ahead forecasts of anomalies with arfima function of forecast package

    resArr_dLW[j,, i]		<- as.numeric(forecast(m.dLW.tmp, h = h)$mean)
    resArr_bestd[j,, i]		<- as.numeric(forecast(m.bestd.tmp, h = h)$mean)
    resArr_drot[j,, i]		<- as.numeric(forecast(m.drot.tmp, h = h)$mean)

  }

}


#compute forecast errors for evaluation period (January 2020 until December 2024)
fcErrArr_dLW	<- evalAno20to24 - resArr_dLW
fcErrArr_bestd	<- evalAno20to24 - resArr_bestd
fcErrArr_drot	<- evalAno20to24 - resArr_drot

#station names (third dimension of results and forecast error arrays)
dat_res$statID


#set up results objects and compute mean forecast error
res.mat_dLW		<- matrix(NA, nrow = length(dat_res$statID), ncol = 12)
res.mat_bestd	<- matrix(NA, nrow = length(dat_res$statID), ncol = 12)
res.mat_drot	<- matrix(NA, nrow = length(dat_res$statID), ncol = 12)

for(i in 1:length(dat_rs$statID)){

  res.mat_dLW[i, ]	<- apply(fcErrArr_dLW[,,i], FUN = mean, MARGIN = 2, na.rm = TRUE)
  res.mat_bestd[i, ]	<- apply(fcErrArr_bestd[,,i], FUN = mean, MARGIN = 2, na.rm = TRUE)
  res.mat_drot[i, ]	<- apply(fcErrArr_drot[,,i], FUN = mean, MARGIN = 2, na.rm = TRUE)

}

#mean forecast errors
colMeans(res.mat_dLW)
colMeans(res.mat_bestd)
colMeans(res.mat_drot)

#relative forecast errors
mat		<- matrix(nrow = 3, ncol = 12)
mat[1,]	<- colMeans(res.mat_dLW)/colMeans(res.mat_bestd)
mat[2,]	<- colMeans(res.mat_bestd)/colMeans(res.mat_bestd)
mat[3,]	<- colMeans(res.mat_drot)/colMeans(res.mat_bestd)







#out-of-sample forecasts for all monitoring stations

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





