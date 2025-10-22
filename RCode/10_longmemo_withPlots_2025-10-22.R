
#############################################################################
### R-Basics and data
#############################################################################


###
###	Basics
###

###	Load libraries

#	install.packages("devtools")
	library(devtools)
#	install.packages("forecast")
	library(forecast)
#	install.packages("dplyr")
	library(dplyr)
#	install.packages("snowfall")
	library(snowfall)
#	install.packages("arfima")
	library(arfima)
#	install.packages("longmemo")
	library(longmemo)
#	install.packages("partitions")
	library(partitions)
#	install.packages("strucchange")
	library(strucchange)
#	install.packages("fracdiff")
	library(fracdiff)
#	install.packages("mvtnorm")
	library(mvtnorm)
#	install.packages("RcppArmadillo")
	library(RcppArmadillo)
	download.file(url = "https://cran.r-project.org/src/contrib/Archive/LongMemoryTS/LongMemoryTS_0.1.0.tar.gz",
		destfile = "LongMemoryTS_0.1.0.tar.gz")
	install.packages(pkgs = "LongMemoryTS_0.1.0.tar.gz", type = "source", repos = NULL)
	library(LongMemoryTS)
	download.file(url = "https://cran.r-project.org/src/contrib/Archive/memochange/memochange_1.1.2.tar.gz",
		destfile = "memochange_1.1.2.tar.gz")
	install.packages(pkgs = "memochange_1.1.2.tar.gz", type = "source", repos = NULL)
	library(memochange)
	install_github("markusfritsch/prcpFC")
	library(prcpFC)



###	Clear Workspace

rm(list = ls())




###
###	Load rainfall data and compile anomaly data
###

###	Compile anomaly data based on imputed time series

data(raindata6019)

#monthly_demean <- function(x){
#  refmeans <- rowMeans(matrix(x[1:360], nrow=12)) # 1960-01 to 1989-12
#  anomalies <- x-rep(refmeans,times=length(x)/12)
#  return(anomalies)
#}
#results_anomalies <- sapply(raindata6019[,-1], function(x) monthly_demean(unname(unlist(x))))
#raindata6019_demeaned <- data.frame(raindata6019[,1], data.frame(results_anomalies))
#colnames(raindata6019_demeaned)[1]	<- "date"


monthly_demed <- function(x){
  refmeds <- apply(matrix(x[1:360], nrow=12), FUN = median, MARGIN = 1) # 1960-01 to 1989-12
  anomalies <- x-rep(refmeds,times=length(x)/12)
  return(anomalies)
}
results_anomalies <- sapply(raindata6019[,-1], function(x) monthly_demed(unname(unlist(x))))
raindata6019_demeded <- data.frame(raindata6019[,1], data.frame(results_anomalies))
colnames(raindata6019_demeded)[1]	<- "date"





###
###	Prewhitening
###


raintib <- filter(raindata6019_demeded, as.numeric(substr(date, 1, 4))>=1990)


#################################

my_arfimafit <- function(x, maxp, maxq, criterion="BIC"){
  pqcombis <- expand.grid(p=0:maxp, q=0:maxq)
  allfits <- mapply(function(p,q){cat(paste0("p=",p,", q=",q,"\n"));arfima::arfima(x,order=c(p,0,q),dmean=FALSE,quiet=TRUE)},
                    pqcombis$p, pqcombis$q, SIMPLIFY=FALSE)
  if(criterion=="BIC"){
    bestfitindex <- which.min(sapply(allfits, function(i) suppressWarnings(tryCatch(summary(i)$bics,error=function(cond){return(NA)}))))
  }else if(criterion=="AIC"){
    bestfitindex <- which.min(sapply(allfits, function(i) suppressWarnings(tryCatch(summary(i)$aics,error=function(cond){return(NA)}))))
  }
  bestfit <- allfits[[bestfitindex]]
  bestpq <- as.matrix(pqcombis)[bestfitindex,]
  bestphis <- bestfit$modes[[1]]$phi
  bestthetas <- bestfit$modes[[1]]$theta 
  bestd <- bestfit$modes[[1]]$dfrac
  bestorder <- round(c(bestpq[1],d=bestd,bestpq[2]),2)
  return(list(fit=bestfit, order=bestorder, pars=list(phi=bestphis, d=bestd, theta=bestthetas)))
}

my_prewhitening <- function(x){  # following [Qu 2011, Sec. 5]
  mu <- mean(x)
  bestfit <- my_arfimafit(x-mu, maxp=1, maxq=1, criterion="AIC")
  order <- c(bestfit$order[1],0,bestfit$order[3])
  fixed <- mu   # coefficients (phi, theta, mu)
  if(order[3]==1){
    fixed <- c(-bestfit$pars$theta, fixed)
    if(abs(fixed[1])>0.99){
      fixed[1] <- sign(fixed[1])*0.99
    }
  }
  if(order[1]==1){
    fixed <- c(bestfit$pars$phi, fixed)
    if(abs(fixed[1])>0.99){
      fixed[1] <- sign(fixed[1])*0.99
    }
  }
  prewhitefit <- stats::arima(x,order=order,fixed=fixed,include.mean=TRUE,transform.pars=FALSE)
  return(list(arfimafit=bestfit, armafit=prewhitefit, prewhitened=as.vector(prewhitefit$residuals)))
}


sfInit(parallel=TRUE, cpus=15)
sfExport("my_arfimafit")
sfExport("raintib")
results_prewhitening <- sfLapply(raintib[,-1], my_prewhitening)
sfStop()

raintib_prewhitened <- tibble(date=raintib$date,
                              as_tibble(lapply(results_prewhitening, function(i) i$prewhitened)))
prewhitening_phis <- lapply(results_prewhitening, function(i) i$arfimafit$fit$modes[[1]]$phi)
prewhitening_phis <- sapply(prewhitening_phis, function(i) ifelse(length(i)==0,0,i))
prewhitening_ds <- sapply(results_prewhitening, function(i) i$arfimafit$fit$modes[[1]]$dfrac)
prewhitening_thetas <- lapply(results_prewhitening, function(i) i$arfimafit$fit$modes[[1]]$theta)
prewhitening_thetas <- sapply(prewhitening_thetas, function(i) ifelse(length(i)==0,0,i))
results_prewhitening_coefs <- cbind(prewhitening_phis,prewhitening_ds,prewhitening_thetas)





###
###	Long Memory Estimation and Testing
###


raintib <- filter(raindata6019_demeded, as.numeric(substr(date, 1, 4))>=1990)


#################################

my_R.LW <- function (d, peri, m, n, l=1){
  lambda <- 2*pi/n*(l:m)
  K <- log(1/(m-l+1)*(sum(peri[l:m]*(lambda)^(2*d))))-2*d/(m-l+1)*sum(log(lambda))
  return(K)
}


my_LW <- function(data, m, int=c(-0.5,2.5), diff_param=1, l=1){
  n <- length(data)
  peri <- longmemo::per(data)[-1]
  d.hat <- optimize(f=my_R.LW, interval=int, peri=peri, m=m, n=n, l=l)$minimum
  return(d.hat) 
}


my_houLW <- function(data, m, int=c(-0.4999,0.99)){
  n <- length(data)
  peri <- longmemo::per(data)[-1]
  #peri <- longmemo::per(data)[2:(m+1)]
  out <- optim(par=c(0,0.1), fn=my_J.M, peri=peri, m=m, n=n,
               method="L-BFGS-B", lower=c(int[1],0), upper=c(int[2],10000))  # lower=c(int[1],0.0001)
  d.hat <- out$par[1]
  return(d.hat) 
  # return(out$par) # zur Kontrolle von theta (wenn 0 dann keine low frequency contaminations)
}


my_Qu <- function (data, m, epsilon = 0.05){
  n <- length(data)
  m <- min(m, floor(n/2))
  r.grid <- 1/m
  r <- seq(epsilon, 1, r.grid)
  peri <- longmemo::per(data)[-1]
  lambdaj <- 2 * pi * 1:floor(n/2)/n
  nuj <- log(lambdaj[1:m]) - mean(log(lambdaj[1:m]))
  d.hat <- my_LW(data, m = m)
  G.hat <- mean(lambdaj[1:m]^(2 * d.hat) * peri[1:m])
  mr <- floor(m * r)
  if (mr[1] < 1) {
    mr <- mr[-1]
  }
  W.aux <- 0
  for (i in mr) {
    W.aux[i] <- 1/sqrt(sum(nuj[1:m]^2)) * abs(sum(nuj[1:i] * 
                                                    (peri[1:i]/(G.hat * lambdaj[1:i]^(-2 * d.hat)) - 
                                                       1)))
  }
  W.aux <- na.omit(W.aux)
  crit <- cbind(c(1.118, 1.252, 1.374, 1.517), c(1.022, 1.155, 
                                                 1.277, 1.426))
  colnames(crit) <- c("eps=.02", "eps=.05")
  rownames(crit) <- c("alpha=.1", "alpha=.05", 
                      "alpha=.025", "alpha=.01")
  list(W.stat = max(W.aux), CriticalValues = crit)
}


my_J.M <- function (vec, peri, m, n){
  d <- vec[1]
  theta <- vec[2]
  lambda <- 2*pi/n*(1:m)
  g.k <- lambda^(-2*d)+(theta*lambda^(-2)/n)
  K <- log((1/m)*sum(peri[1:m]/g.k)) + sum(log(g.k))/m
  #K <- log((1/m)*sum(peri/g.k)) + sum(log(g.k))/m
  return(K)
}



n <- nrow(raintib)    # keep epsilon=0.05 for both sample sizes
m0.7 <- floor(n^0.7)
m0.6 <- floor(n^0.6)  # only for short-run dynamics

LManalysis <- function(x){
  Qu <- my_Qu(x, m=m0.7, epsilon=0.05)$W.stat
  LW0.6 <- my_LW(data=x,m=m0.6)
  LW <- my_LW(data=x,m=m0.7)
  houLW0.6 <- my_houLW(data=x,m=m0.6)
  houLW <- my_houLW(data=x,m=m0.7)
  results <- c("Qu"=Qu,"LW0.6"=LW0.6,"LW"=LW,"houLW0.6"=houLW0.6,"houLW"=houLW)
  return(results)
}

sfInit(parallel=TRUE, cpus=15)
sfExportAll()
LMresults <- cbind("Qu_pre"=sfSapply(raintib_prewhitened[,-1], function(x) my_Qu(x, m=m0.7, epsilon=0.05)$W.stat),
                   t(sfApply(raintib[,-1], 2, function(x) LManalysis(x=x))))
sfStop()


# prepare best LW and houLW according to short-run dynamics
armanoise <- ifelse(results_prewhitening_coefs[,1]!=0 | results_prewhitening_coefs[,3]!=0,TRUE,FALSE)
LWmixed <- ifelse(armanoise, LMresults[,"LW0.6"], LMresults[,"LW"])
houLWmixed <- ifelse(armanoise, LMresults[,"houLW0.6"], LMresults[,"houLW"])
LMresults <- cbind(LMresults, LWmixed, houLWmixed)





###
###	Testing for Mean Breaks
###


raintib	<- as_tibble(raindata6019_demeded)
raintib <- filter(raindata6019_demeded, as.numeric(substr(date, 1, 4))>=1990)


#################################

SWtests <- function(x,d){
  SNtest <- memochange::snsupwald(x,d,tau=0.15)
  FBtest <- memochange::fixbsupw(x,d,bandw=0.1,tau=0.15)
  SNdecision <- ifelse(SNtest[4]>SNtest[3],"break1",
                       ifelse(SNtest[4]>SNtest[2],"break5",
                              ifelse(SNtest[4]>SNtest[1],"break10","constant")))
  FBdecision <- ifelse(FBtest[4]>FBtest[3],"break1",
                       ifelse(FBtest[4]>FBtest[2],"break5",
                              ifelse(FBtest[4]>FBtest[1],"break10","constant")))
  return(c(SNSW=unname(SNdecision),FBSW=unname(FBdecision)))
}

sfInit(parallel=TRUE, cpus=15)
sfExportAll()
SWresults <- t(sfSapply(1:ncol(raintib[,-1]),
                        function(i) c(SWtests(x=raintib[[i+1]], d=LMresults[i,"LW"]),
                                      SWtests(x=raintib[[i+1]], d=LMresults[i,"LWmixed"]))))
sfStop()
colnames(SWresults) <- c("SNSW","FBSW","SNSWmixed","FBSWmixed")





###
###	Building Binary Variables and dstar
###

# Qu critical values 10%=1.022, 5%=1.155, 1%=1.426


allresults <- as_tibble(data.frame(LMresults, SWresults, results_prewhitening_coefs))

allresults <- allresults %>%
  mutate(armanoise = prewhitening_phis!=0 | prewhitening_thetas!=0) %>%
  mutate(meanbreak =
           (SNSWmixed=="break1" | FBSWmixed=="break1") |
           (SNSWmixed=="break5" | FBSWmixed=="break5") |
           (SNSWmixed=="break10" & FBSWmixed=="break10")) %>%
  mutate(quspurious5 = Qu_pre>=1.155) %>%  # 5% rejection
  mutate(quspurious10 = Qu_pre>=1.022) %>%  # 10% rejection
  mutate(potentially_contaminated = quspurious10 | SNSWmixed!="constant" | FBSWmixed!="constant") %>%
  mutate(bestd = ifelse(potentially_contaminated, houLWmixed, LWmixed)) %>%
  mutate(ddiff = LW-bestd)


data(ghcndStations)
rainresults <- tibble(ghcndStations[ghcndStations$statID %in% colnames(raintib)[-1], c(1:4,7,9:11)],
                  select(allresults,  # zvars and destims
                         quspurious5, quspurious10, meanbreak, armanoise, potentially_contaminated,
                         LW, LWmixed, houLW, houLWmixed, bestd, ddiff))

rainresults <- rainresults[(rainresults$zone_c == "B" |
				rainresults$zone_c == "C" |
				rainresults$zone_c == "D"), ]
#save(rainresults, file = "rainresults.RData"))












