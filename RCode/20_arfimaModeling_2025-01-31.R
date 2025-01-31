
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




###	Load data

#source("00_dataPreprocessing_2025-01-29")		#note that this script needs to be run first






###
###	Generate forecasts for precipitation anomalies and total precipitation based on subsample of monitoring stations
###


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

#tables with total precipitation forecasts
xtable(t(fc_dLW_tot_rs[, order(dat_rs$bestd)]), digits = 0)
xtable(t(fc_bestd_tot_rs[, order(dat_rs$bestd)]), digits = 0)
xtable(t(fc_drot_tot_rs[, order(dat_rs$bestd)]), digits = 0)













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


#compute squared forecast errors for evaluation period (January 2020 until December 2024)
sqfcErrArr_dLW		<- (evalAno20to24 - resArr_dLW)^2
sqfcErrArr_bestd	<- (evalAno20to24 - resArr_bestd)^2
sqfcErrArr_drot		<- (evalAno20to24 - resArr_drot)^2

#station names (third dimension of results and forecast error arrays)
dat_res$statID


#set up results objects and compute mean square forecast error
res.mat_dLW	<- matrix(NA, nrow = nrow(resArr_dLW[-1,,]), ncol = 12)
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


#table of root mean square forecast errors
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
xtable(mat1, digits = 0)


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
xtable(mat2, digits = 0)


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
xtable(mat3, digits = 0)















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



#monitoring stations located in the contiguous United States
ncol(dat_demeded[, colnames(dat_demeded) %in% stat_usCont$statID])











