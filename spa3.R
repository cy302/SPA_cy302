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
  y <- matrix(y, dim(x)[1], 2, byrow = TRUE)
  return(sqrt(rowSums((x-y)^2)))
}

calc.dist.mat <- function(path_nodes, city_nodes){
  m <- matrix(NA, dim(city_nodes)[1], dim(path_nodes)[1])
  for (i in 1:dim(city_nodes)[1]){
    m[i,] <- calc.dist(path_nodes, city_nodes[i,])
  }
  return(m)
}

calc.weight.mat <- function(path_nodes, city_nodes, K){
  dist.mat <- calc.dist.mat(path_nodes, city_nodes)
  weight.mat <- phi(dist.mat, K)
  W <- matrix(NA, dim(city_nodes)[1], 2*dim(path_nodes)[1])
  for (i in 1:dim(city_nodes)[1]){
    sums <- sum(weight.mat[i,])
    weight.mat[i,] <- weight.mat[i,]/sums
  }
  for (j in 1:dim(path_nodes)[1]){
    W[,2*j-1] <- W[,2*j] <- weight.mat[,j]
  }
  return(W)
}

calc.diff <- function(path_nodes, city_nodes, W){
  A <- matrix(NA, dim(city_nodes)[1], 2*dim(path_nodes)[1])
  for (j in 1:dim(path_nodes)[1]){
    A[,(2*j-1):(2*j)] <- city_nodes - 
      matrix(path_nodes[j,], dim(city_nodes)[1], 2, byrow=TRUE) 
  }
  A <- W * A
  s <- colSums(A)
  s <- matrix(s, dim(path_nodes)[1], 2, byrow = TRUE)
  return(s)
}

rescale <- function(X){
  max1 <- max(X[,1])
  min1 <- min(X[,1])
  max2 <- max(X[,2])
  min2 <- min(X[,2])
  rg1 <- max1 - min1
  rg2 <- max2 - min2
  X[,1] <- (X[,1]-min1)/rg1
  X[,2] <- (X[,2]-min2)/rg2
  return(X)
}

scale.back <- function(X, Y){
  max1 <- max(X[,1])
  min1 <- min(X[,1])
  max2 <- max(X[,2])
  min2 <- min(X[,2])
  rg1 <- max1 - min1
  rg2 <- max2 - min2
  Y[,1] <- rg1*Y[,1] + min1
  Y[,2] <- rg2*Y[,2] + min2
  return(Y)
}

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

elastic.net <- function(alpha=0.2, beta=2.0, filename,num.iter){
  X <- transfer.mat(filename)[,2:3]
  X.new <- rescale(X)
  X1 <- X.new[, 1]
  X2 <- X.new[, 2]

  
  N <- length(X1)
  M <- round(2.5 * N)
  centroid <- c(0.5, 0.5)
  radius <- 0.1
  Y <- circular.points(M, radius, centroid)
  for (iter in 1:num.iter){
    K <- 0.2 - (0.05*0.2)*as.integer(iter/(0.051*num.iter))
    W <- calc.weight.mat(path_nodes = Y, city_nodes = X.new, K)
    s <- calc.diff(path_nodes = Y, city_nodes = X.new, W)
    
    del.y[1,] <- alpha*s[1,] + beta*K*(Y[2,]-2*Y[1,]+Y[M,])
    del.y[M,] <- alpha*s[M,] + beta*K*(Y[(M-1),]-2*Y[M,]+Y[1,])
    del.y[2:(M-1),] <- alpha*s[2:(M-1),] + 
      beta*K*(Y[1:(M-2),]-2*Y[2:(M-1),]+Y[3:M,])
    
    Y <- Y + del.y
  }
  Y <- scale.back(X, Y)
  dis <- 0
  for (i in 1:(dim(Y)[1]-1)){
    dis <- dis + sqrt(sum((Y[i,]-Y[(i+1),])^2))
  }
  l <- list("cities.coordinates"=X, "pathpoints.coordinates"=Y, 
            "shortest.distance"=dis)
  return(l)
}

l <- elastic.net(filename = "ali535.tsp.gz", num.iter = 500)
