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


my_arstationaritycheck <- function(phis){
  charpolynom <- as.function(polynom::polynomial(coef=c(1,-phis)))
  fvalues <- sapply(seq(-1,1,0.00001), charpolynom)
  stationary <- all(fvalues>0) | all(fvalues<0)
  return(stationary)
}

my_mainvertibilitycheck <- function(thetas){
  charpolynom <- as.function(polynom::polynomial(coef=c(1,-thetas)))
  fvalues <- sapply(seq(-1,1,0.00001), charpolynom)
  invertible <- all(fvalues>0) | all(fvalues<0)
  return(invertible)
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
