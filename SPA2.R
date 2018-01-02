setwd('/Users/cy1614/Desktop/')
require(knitr)
knit2pdf('spa2_138.Rnw')

# 1.1 the plot
drawgrid <- function(sums, n, main=''){
  if (typeof(sums)!='double'||typeof(n)!='double'||typeof(main)!='character'){
    return(FALSE)
  }
  else{
    plot(c(-2,1),c(-2,1),type="n",bty='n',xaxt='n',yaxt='n',ann=FALSE,asp=1,cex=10)
    segments(-2, 1, 1, 1)
    segments(-2, -2, 1, -2)
    segments(-2, -1, 1, -1)
    segments(-2, 0, 1, 0)
    segments(-2, 1, -2, -2)
    segments(-1, 1, -1, -2)
    segments(0, 1, 0, -2)
    segments(1, 1, 1, -2)
    
    symbols(-1, 0, circles = 0.25, inches = FALSE, add = TRUE, bg = 'white')
    symbols(-1, -1, circles = 0.25, inches = FALSE, add = TRUE, bg = 'white')
    symbols(0, 0, circles = 0.25, inches = FALSE, add = TRUE, bg = 'white')
    symbols(0, -1, circles = 0.25, inches = FALSE, add = TRUE, bg = 'white')
    
    text(-1, 0, labels = sums[1])
    text(0, 0, labels = sums[2])
    text(-1, -1, labels = sums[3])
    text(0, -1, labels = sums[4])
    
    if (length(n) != 9){
      return(FALSE)
    }
    else{
      N <- matrix(n, 3, 3, byrow=TRUE)
      for (i in 1:3){
        for (j in 1:3){
          if (N[i, j] != 0){
            text(j-2.5, 1.5-i, labels = N[i, j])
          }
        }
      }
    }
    title(main=main, line = -3)
    return(plot)
  }
}
par(mfrow=c(1, 3), mar=c(2, 2, 1.5, 1))
drawgrid(c(16, 19, 17, 16), c(0, 1, 0, 0, 0, 0, 6, 4, 0), main='A')
drawgrid(c(16, 11, 21, 21), c(8, 0, 2, 0, 0, 0, 0, 0, 0), main='B')
drawgrid(c(18, 13, 18, 18), c(0, 0, 0, 0, 0, 0, 0, 0, 0), main='C')

# 1.2 function for solving sujiko problem
sujiko <- function(sums, squares){
  if (typeof(sums)!='double'||typeof(squares)!='double'||
      length(sums)!=4||length(squares)!=9){
    return(FALSE)
  }
  else{
    num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    if (identical(squares, rep(0, 9))){
      per <- permutations(9)
      for (i in seq.int(factorial(9))){
        squares.1 <- per[i,]
        bool.1 <- (squares.1[1]+squares.1[2]+squares.1[4]+squares.1[5])==sums[1]
        bool.2 <- (squares.1[2]+squares.1[3]+squares.1[5]+squares.1[6])==sums[2]
        bool.3 <- (squares.1[4]+squares.1[5]+squares.1[7]+squares.1[8])==sums[3]
        bool.4 <- (squares.1[5]+squares.1[6]+squares.1[8]+squares.1[9])==sums[4]
        bool.final <- bool.1&&bool.2&&bool.3&&bool.4
        if (bool.final){
          break
        }
      }
      return(squares.1)
    }
    else{
      nonzerosq <- squares[which(squares!=0)]
      num <- num[-nonzerosq]
      per <- permutations(length(num))
      for (i in seq.int(length(per)/length(num))){
        num.1 <- num[per[i,]]
        squares.1 <- squares
        for (j in 1:9){
          if (squares.1[j]==0){
            squares.1[j] <- num.1[1]
            num.1 <- num.1[-1]
          }
        }
        bool.1 <- (squares.1[1]+squares.1[2]+squares.1[4]+squares.1[5])==sums[1]
        bool.2 <- (squares.1[2]+squares.1[3]+squares.1[5]+squares.1[6])==sums[2]
        bool.3 <- (squares.1[4]+squares.1[5]+squares.1[7]+squares.1[8])==sums[3]
        bool.4 <- (squares.1[5]+squares.1[6]+squares.1[8]+squares.1[9])==sums[4]
        bool.final <- bool.1&&bool.2&&bool.3&&bool.4
        if (bool.final){
          break
        }
      }
      return(squares.1)
    }
  }
}


permutations <- function(n){
  if (n==1){
    return(matrix(1))
  }
  else{
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <-matrix(nrow=n*p, ncol=n)
    for (i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i, sp+(sp>=i))
    }
    return(A)
  }
}
# 1.3 solving the problenms in Figure 1
par(mfrow=c(1, 3), mar=c(2, 2, 1.5, 1))
sq.1 <- sujiko(c(16, 19, 17, 16), c(0, 1, 0, 0, 0, 0, 6, 4, 0))
sq.2 <- sujiko(c(16, 11, 21, 21), c(8, 0, 2, 0, 0, 0, 0, 0, 0))
sq.3 <- sujiko(c(18, 13, 18, 18), c(0, 0, 0, 0, 0, 0, 0, 0, 0))
drawgrid(c(16, 19, 17, 16), sq.1, main='A')
drawgrid(c(16, 11, 21, 21), sq.2, main='B')
drawgrid(c(18, 13, 18, 18), sq.3, main='C')

# question 2
sum.step <- function(v){
  if (length(v)<2){
    return(c())
  }
  return(c(v[1]+v[2], sum.step(v[-1])))
}

check <- function(s){
  s4 <- s
  while (length(s)>1){
    s2 <- sum.step(s)
    if (max(s2)>99){
      return(FALSE)
    }
    s.n <- length(s)
    s <- unique(c(s, s2))
    if (length(s)<(s.n+length(s2))){
      return(FALSE)
    }
    s <- s2
    s4 <- c(s4, s2)
  }
  if (!identical(s4, unique(s4))){
    return(FALSE)
  }
  return(TRUE)
}

A <- matrix(0, 6, 6)
for (i.C in 1:5){
  for (i.D in (i.C+1):5){
    for (i.B in seq(1:11)[-c(i.C, i.D)]){
      if (!check(c(i.B, i.C, i.D))){
        next
      }
      for (i.E in seq(1:11)[-c(i.C, i.D, i.B)]){
        if (!check(c(i.B, i.C, i.D, i.E))){
          next
        }
        for (i.A in seq(1:30)[-c(i.C, i.D, i.B, i.E)]){
          if (!check(c(i.A, i.B, i.C, i.D, i.E))){
            next
          }
          for (i.F in seq(1:30)[-c(i.C, i.D, i.B, i.E, i.A)]){
            l <- c(i.A, i.B, i.C, i.D, i.E, i.F)
            if (!check(l)){
              next
            }
            i <- 1
            while (length(l)>0){
              print(l)
              A[i, 1:length(l)] <- l
              i <- i+1
              l <- sum.step(l)
            }
          }
        }
      }
    }
  }
}

pyramid <- function(n){
  A <- matrix(0, n, n)
  for (i.C in 1:5){
    for (i.D in (i.C+1):5){
      for (i.B in seq(1:11)[-c(i.C, i.D)]){
        if (!check(c(i.B, i.C, i.D))){
          next
        }
        for (i.E in seq(1:11)[-c(i.C, i.D, i.B)]){
          if (!check(c(i.B, i.C, i.D, i.E))){
            next
          }
          for (i.A in seq(1:30)[-c(i.C, i.D, i.B, i.E)]){
            if (!check(c(i.A, i.B, i.C, i.D, i.E))){
              next
            }
            for (i.F in seq(1:30)[-c(i.C, i.D, i.B, i.E, i.A)]){
              l <- c(i.A, i.B, i.C, i.D, i.E, i.F)
              if (!check(l)){
                next
              }
              i <- 1
              while (length(l)>0){
                print(l)
                A[i, 1:length(l)] <- l
                i <- i+1
                l <- sum.step(l)
              }
            }
          }
        }
      }
    }
  }
  return(A)
}

solve.pyramid <- function(n){
  A <- pyramid(n)
  par(mfrow=c(1, 1), mar=c(2, 2, 1.5, 1))
  plot(c(0, 6),c(0, 3.5),type="n",bty='n',xaxt='n',yaxt='n',ann=FALSE,asp=1,cex=10)
  symbols(0.5, 0.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(1.5, 0.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(2.5, 0.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(3.5, 0.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(4.5, 0.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(5.5, 0.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(1.0, 0.8, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(2.0, 0.8, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(3.0, 0.8, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(4.0, 0.8, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(5.0, 0.8, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(1.5, 1.4, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(2.5, 1.4, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(3.5, 1.4, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(4.5, 1.4, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(2.0, 2.0, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(3.0, 2.0, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(4.0, 2.0, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(2.5, 2.6, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(3.5, 2.6, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  symbols(3.0, 3.2, rectangles = matrix(c(0.9, 0.5), 1, 2), add = TRUE, inches = FALSE, bg = 'white')
  for (i in 1:n){
    for (j in 1:(7-i)){
      text(0.5*i+j-1, 0.2+0.6*(i-1), A[i, j])
    }
  }
  return(plot)
}


# question 3
logistic.map <- function(alpha, x){
  x.t1 <- rep(NA, 1000)
  x.t1[1] <- x
  for (i in 1:999){
    x.t1[i+1] <- alpha * x.t1[i] * (1-x.t1[i])
  }
  return(x.t1[800:1000])
}

alpha <- seq(2.8, 3.6, length.out = 300)
logistic <- sapply(alpha, logistic.map, x = runif(1))
alpha.plot <- sort(rep(alpha, 201))
plot(alpha.plot, logistic, pch = '.', bty = 'n')


doub <- function(alpha, x){
  ind <- 1
  doubl <- c()
  for (i in 1:length(alpha)){
    x.t1 <- rep(NA, 1000)
    x.t1[1] <- x
    for (j in 1:999){
      x.t1[j+1] <- alpha[i] * x.t1[j] * (1-x.t1[j])
    }
    x.unique <- unique(round(x.t1[800:1000], 1))
    if (length(x.unique) > ind){
      ind <- length(x.unique)
      doubl <- c(doubl, alpha[i])
    }
  }
  return(doubl)
}

alpha <- seq(2.8, 3.6, by = 0.001)
doubling.list <- doub(alpha, 0.1)

alpha <- seq(2.8, 3.6, length.out = 300)
logistic <- sapply(alpha, logistic.map, x = runif(1))
alpha.plot <- sort(rep(alpha, 201))
plot(alpha.plot, logistic, pch = '.', bty = 'n')

for (i in 1:length(doubling.list)){
  abline(v = doubling.list[i], lty = 2)
}