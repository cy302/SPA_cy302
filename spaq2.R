############
# Problem 1#
############

pso <- function(N, n=1, epsilon=1e-4, theta.max=0.9, 
                theta.min=0.4,interval=rbind(c(-2, 2)), 
                iter.max, f, c1=2, c2=2){
  
  interval.min <- interval[,1]
  interval.max <- interval[,2]
  X <- matrix(NA, N, n)
  V <- matrix(0, N, n)
  
  for (i in 1:n){
    X[,i] <- runif(N, min=interval.min[i], max=interval.max[i])
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
    l <- which(fx>fx.best)
    X.best[l,] <- X[l,]
    fx.best <- rep(NA, N)
    for (i in 1:N){
      fx.best[i] <- f(X.best[i,])
    }
    x.best <- X.best[which.max(fx.best)[1],]
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

myfunc1 <- function(x) -x^2+2*x+11 

pso(100, iter.max=500, f = myfunc1)

optim(c(0, 0), myfunc2, method = "L-BFGS-B", lower = rep(0, 0), 
      upper = c(3, 1))

#############
# Problem 2 #
#############

myfunc2 <- function(x) x[1]^2+x[2]^2-2*x[1]-4*x[2]

constraint.1 <- function(x) x[1]+4*x[2]-5

constraint.2 <- function(x) 2*x[1]+3*x[2]-6

qf <- function(x){
  return(max(0, x))
}

Thetaf <- function(x){
  if (x < 0.001){
    output <- 10
  }
  else if (0.001 <= x && x <= 0.1){
    output <- 20
  }
  else if (0.1 < x && x <= 1){
    output <- 100
  }
  else{
    output <- 300
  }
  return(output)
}

gamma <- function(x){
  if (x < 1){
    output <- 1
  }
  else{
    output <- 2
  }
  return(output)
}

pso.constrained <- function(N,n=2,epsilon=1e-3,theta.max=0.9, 
                            theta.min=0.4,cons1=constraint.1,
                            cons2=constraint.2,iter.max,f,c1=2,
                            c2=2){
  X <- matrix(NA, N, n)
  V <- matrix(0, N, n)
  
  # X[,1] <- runif(N, 0, 3)
  # X[,2] <- runif(N, 0, 2)
  
  for (i in 1:n){
    X[,i] <- runif(N)
    # initialise x1's and x2's using runif(), U[0,1], hence all
    # constraints are satisfied in this case.
  }
  
  X.best <- X
  k <- 0
  
  diam <- calc.diam(X)
  
  while (k<iter.max && !check.diam(diam, epsilon)){
    h <- sqrt(k)
    
    Fx<- rep(NA, N)
    for (i in 1:N){
      Fx[i] <- f(X[i,])+h*(Thetaf(qf(cons1(X[i,])))*
                             (qf(cons1(X[i,]))^gamma(qf(cons1(X[i,]))))+
                             Thetaf(qf(cons2(X[i,])))*
                             (qf(cons2(X[i,]))^gamma(qf(cons2(X[i,])))))
    }
    
    if (k==0){
      Fx.best <- Fx
      x.best <- X.best[which.min(Fx.best)[1],]
    }
    else{
      l <- which(Fx<Fx.best)
      X.best[l,] <- X[l,]
      Fx.val <- rep(NA, N)
      for (i in 1:N){
        Fx.val[i] <- f(X.best[i])
      }
      
      x.best <- X.best[which.min(Fx.best)[1],]
      theta <- theta.max-((theta.max-theta.min)/iter.max)*k
      V.update <- matrix(NA, N, n)
      
      for (j in 1:N){
        r1 <- runif(2)
        V.update[j,] <- theta*V[j,]+c1*r1[1]*(X.best[j,]-X[j,])+
          c2*r1[2]*(x.best-X[j,])
      }
      V <- V.update
      X <- X+V
      
      Fx.best <- rep(NA, N)
      for (i in 1:N){
        Fx.best[i] <- f(X.best[i,])+h*(Thetaf(qf(cons1(X.best[i,])))*
                                         (qf(cons1(X.best[i,]))^gamma(qf(cons1(X.best[i,]))))+
                                         Thetaf(qf(cons2(X.best[i,])))*
                                         (qf(cons2(X.best[i,]))^gamma(qf(cons2(X.best[i,])))))
      }
    }
    
    diam <- calc.diam(X)
    k <- k+1
  }
  
  opt <- f(x.best)
  output.list <- list("optimal.value"=opt, 
                      "optimal.vector"=x.best,
                      "num.iterations"=k)
  return(output.list)
}

pso.constrained(N=100, iter.max=5000, f=myfunc2)

myfunc2 <- function(x) x[1]^2+x[2]^2-2*x[1]-4*x[2]
gmyfunc2 <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(2*x1-2, 2*x2-4)
}
constrOptim(theta = c(0, 0), f = myfunc2, grad = gmyfunc2, 
            ui = rbind(c(-1, -4), c(-2, 3)), ci = c(-5, -6))

############
# Problem3 #
############

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

pso.de.jong(N = 100, n = 2, iter.max = 5000)
optim(c(1, 1), fn = myfunc3)