setwd("~/Desktop/MPhilComputationalBiology/SP/SPA3/ALL_tsp/")

circular.points <- function(M, r, centroid){
  theta.list <- seq(0, 2*pi, length.out = M)
  x <- r*cos(theta.list) + centroid[1]
  y <- r*sin(theta.list) + centroid[2]
  return(cbind(x, y))
}
# X <- circular.points(250, .1, c(3, 4))
# plot(X[,1], X[,2])

phi <- function(d, K){
  # energy function phi
  return(exp(-d^2/(2*K^2)))
}

calc.dist <- function(x, y){
  # euclidean 2-norm
  return(sqrt((x[,1]-y[,1])^2+(x[,2]-y[,2])^2))
}

X <- read.csv("~/Desktop/MPhilComputationalBiology/SP/SPA3/random_10_cities.csv")
X <- as.matrix(X)

transfer.mat <- function(filename){
  df <- readLines(filename)
  startline <- grep("1", df)[1]
  df <- df[startline:(length(df)-1)]
  X <- matrix(NA, length(df), 3)
  for (i in 1:length(df)){
    a <- as.double(unlist(strsplit(df[i]," ")))
    X[i, ] <- a[!is.na(a)]
  }
  return(X)
}

par(pty="s")
X.old <- transfer.mat("ali535.tsp.gz")[,2:3]
X <- rescale(X.old)
#X <- read.csv("~/Desktop/MPhilComputationalBiology/SP/SPA3/western_sahara.csv")
#X <- as.matrix(X)
X1 <- X[, 1]
X2 <- X[, 2]
N <- length(X1)
M <- round(2.5 * N)
centroid <- c(0.5, 0.5)
radius <- 0.1
# radius <- 0.1
# r.2 <- 2 * radius
Y <- circular.points(M, radius, centroid)

# r.2 <- 0.2

iter <- 1
num.iter <- 500

for (iter in 1:num.iter){
  K <- 0.2 - (0.05*0.2)*as.integer(iter/(0.052*num.iter))
  W <- calc.weight.mat(path_nodes = Y, city_nodes = X, K)
  s <- calc.diff(path_nodes = Y, city_nodes = X, W)
  
  del.y <- matrix(NA, M, 2)
  
  del.y[1,] <- alpha*s[1,] + beta*K*(Y[2,]-2*Y[1,]+Y[M,])
  del.y[M,] <- alpha*s[M,] + beta*K*(Y[(M-1),]-2*Y[M,]+Y[1,])
  del.y[2:(M-1),] <- alpha*s[2:(M-1),] + 
    beta*K*(Y[1:(M-2),]-2*Y[2:(M-1),]+Y[3:M,])
  
  Y <- Y + del.y
}
K <- r.2 - (0.05*r.2)*as.integer(iter/(0.055*num.iter))
W <- calc.weight.mat(path_nodes = Y, city_nodes = X, K)
s <- calc.diff(path_nodes = Y, city_nodes = X, W)

del.y <- matrix(NA, M, 2)

del.y[1,] <- alpha*s[1,] + beta*K*(Y[2,]-2*Y[1,]+Y[M,])
del.y[M,] <- alpha*s[M,] + beta*K*(Y[(M-1),]-2*Y[M,]+Y[1,])
del.y[2:(M-1),] <- alpha*s[2:(M-1),] + 
  beta*K*(Y[1:(M-2),]-2*Y[2:(M-1),]+Y[3:M,])

Y <- Y + del.y

X.old
Y <- scale.back(X.old, Y)

plot(X.old, type = "p")
lines(rbind(Y, Y[1,]), col=2)

iter <- iter + 1
