myfunc3 <- function(x){
  return(sum(x^2))
}

pso.de.jong <- function(N, n, epsilon=c(1e-3), theta.max=0.9, 
                theta.min=0.4,interval=rbind(c(-5.12, 5.12)), 
                iter.max,f=myfunc3,c1=2,c2=2){
  
  X <- matrix(NA, N, n)
  V <- matrix(0, N, n)
  
  for (i in 1:n){
    X[,i] <- runif(N, min=interval[1], max=interval[2])
  }
  
  X.best <- X
  k <- 0
  fx<- rep(NA, length.out=N)
  for (i in 1:N){
    fx[i] <- f(X[i,])
  }
  diam <- calc.diam(X)
  fx.best <- fx
  k <- k+1
  
  while (k<iter.max && !check.diam(diam, epsilon)){
    fx <- rep(NA, length.out=N)
    for (i in 1:N){
      fx[i] <- f(X[i,])
    }
    l <- which(fx<fx.best)
    X.best[l,] <- X[l,]
    fx.best <- rep(NA, N)
    for (i in 1:N){
      fx.best[i] <- f(X.best[i,])
    }
    x.best <- X.best[which.min(fx)[1],]
    theta <- theta.max-((theta.max-theta.min)/iter.max)*k
    V.update <- matrix(NA, N, n)
    r <- runif(2)
    for (j in 1:N){
      V.update[j,] <- theta*V[j,]+c1*r[1]*(X.best[j,]-X[j,])+
        c2*r[2]*(x.best-X[j,])
    }
    V <- V.update
    X <- X+V
    diam <- calc.diam(X)
    k <- k+1
  }
  
  opt <- f(x.best)
  output.list <- list("optimal.value"=opt,"optimal.vector"=x.best,
                      "num.iterations"=k)
  return(output.list)
}

calc.diam <- function(M){
  n <- dim(M)[2]
  diam <- rep(NA, length.out=n)
  for (i in 1:n){
    diam[i] <- abs(max(M[,i])-min(M[,i]))
  }
  return(diam)
}

check.diam <- function(d, epsilon){
  return(all(d<epsilon))
}

pso.de.jong(N = 100, n = 2, iter.max = 5000)
optim(c(1, 1), fn = myfunc3)
