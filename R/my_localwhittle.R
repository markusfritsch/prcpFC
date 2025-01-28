# in LongMemoryTS Fehler: m-l-1

#' @keywords internal
#'
my_R.LW <- function (d, peri, m, n, l=1){
  lambda <- 2*pi/n*(l:m)
  K <- log(1/(m-l+1)*(sum(peri[l:m]*(lambda)^(2*d))))-2*d/(m-l+1)*sum(log(lambda))
  return(K)
}





#' @keywords internal
#'
my_LW <- function(data, m, int=c(-0.5,2.5), diff_param=1, l=1){
  n <- length(data)
  peri <- longmemo::per(data)[-1]
  d.hat <- optimize(f=my_R.LW, interval=int, peri=peri, m=m, n=n, l=l)$minimum
  return(d.hat) 
}





#' @keywords internal
#'
my_J.M <- function (vec, peri, m, n){
  d <- vec[1]
  theta <- vec[2]
  lambda <- 2*pi/n*(1:m)
  g.k <- lambda^(-2*d)+(theta*lambda^(-2)/n)
  K <- log((1/m)*sum(peri[1:m]/g.k)) + sum(log(g.k))/m
  #K <- log((1/m)*sum(peri/g.k)) + sum(log(g.k))/m
  return(K)
}





#' @keywords internal
#'
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





#' @keywords internal
#'
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


