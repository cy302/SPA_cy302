# Question 1

# 1.1
df <- readLines(
  '/home/cy302/SPA1/usr-share-dict-words.txt')
df <- toupper(df)
unique.df <- unique(df)
n.unique <- length(unique.df)
# there are 97723L (ignore the L) unique words

# 1.2
# in this part, I am going to use the grepl function 
# to check for existence of the apostrophe in words
n.apos <- 0
l.apos <- c()
for (i in seq.int(n.unique)){
  ind <- grepl('\'', unique.df[i])
  if (ind == TRUE){
    n.apos <- n.apos + 1
    l.apos <- c(l.apos, i)
  }
}
unique.df <- unique.df[-l.apos]
print(n.apos)
# there are 25751 words contain an apostrophe

# 1.3
n.non.asc <- 0
l.non.asc <- c()
for (i in seq.int(length(unique.df))){
  ind <- grepl("NOT_ASCII",
               iconv(unique.df[i],"latin1","ASCII",
                     sub="NOT_ASCII"))
  if (ind==TRUE){
    n.non.asc <- n.non.asc + 1
    l.non.asc <- c(l.non.asc, i)
  }
}
unique.df <- unique.df[-l.non.asc]
print(n.non.asc)
# define database for the following questions
database <- unique.df

# 1.4
n <- length(database)
l.og <- c()
l.ogue <- c()
for (i in seq.int(n)){
  len.a <- nchar(database[i])
  if (substr(database[i],len.a-1,len.a)=='OG'){
    l.og <- c(l.og, database[i])
  }
  else if (substr(database[i], len.a-3, len.a)=='OGUE'){
    l.ogue <- c(l.ogue, database[i])
  }
}
l <- c()
for (j in 1:length(l.og)){
  len.og.j <- nchar(l.og[j])
  f <- substr(l.og[j], 1, len.og.j-2)
  for (k in 1:length(l.ogue)){
    len.ogue.k <- nchar(l.ogue[k])
    g <- substr(l.ogue[k], 1, len.ogue.k-4)
    if (f == g){
      l <- c(l, l.og[j], l.ogue[k])
    }
  }
}

# 1.5
scrabble <- read.table('/home/cy302/SPA1/scrabble.txt', header=FALSE)
m <- matrix(0, 26, 2)
for (i in 1:26){
  m[i, 1] <- scrabble[[1]][i]
  # this step directly transforms characters to numbers in terms of the
  # alphabetical order
  m[i, 2] <- scrabble[[4]][i]
}
m <- m[order(m[, 1], decreasing=FALSE), ]
scores <- m[,2]
# this is the scores vector we are looking for

#1.6
n <- length(database)
S <- rep(0, n)
for (i in seq.int(n)){
  s <- 0
  a <- database[i]
  for (j in 1:nchar(a)){
    s <- s+scores[which(LETTERS==substr(a, j, j))]
  }
  S[i] <- s
}
plot(density(S))
l.max.ind <- which.max(S)
l.max <- database[l.max.ind]
sc.max <- S[l.max.ind]
# the highest-scoring word is PIZZAZZ, and the highest score is 45, the 49913th
# element in the database

# 1.7
reverse.complement <- function(a){
  if (typeof(a) != 'character'){
    return(FALSE)
  }
  else{
    n <- nchar(a)
    a <- strsplit(a, '')[[1]]
    a <- rev(a)
    a <- paste(a, collapse = '')
    for (i in (1:n)){
      a.i <- substr(a, i, i)
      ind.old <- which(LETTERS==a.i)
      ind.new <- 27-ind.old
      substr(a, i, i) <- LETTERS[ind.new]
    }
    return(a)
  }
}
l.rev <- c()
for (i in seq.int(n)){
  a.old <- database[i]
  a.new <- reverse.complement(a.old)
  ind <- a.new %in% database
  if (ind){
    l.rev <- c(l.rev, a.old, a.new)
  }
}
l.rev <- unique(l.rev)

#1.8 
l.9 <- c('F', 'A', 'L', 'U', 'Y', 'P', 'L', 'N', 'I')
check.once <- function(a){
  if (typeof(a)!='character'){
    return(FALSE)
  }
  else{
    ind <- c()
    char.a <- unlist(strsplit(a, ''))
    unique.char.a <- unique(char.a)
    for (i in 1:length(unique.char.a)){
      l <- length(char.a[char.a==unique.char.a[i]])
      if (l > 1 && unique.char.a[i]!='L'){
        ind <- c(ind, FALSE)
      }
      else if (l >2 && unique.char.a[i]=='L'){
        ind <- c(ind, FALSE)
      }
      else{
        ind <- c(ind, TRUE)
      }
    }
    return(all(ind))
  }
}
match.1 <- function(a, b){
  ind <- c()
  if (nchar(a) >= 4 && grepl('A', a)){
    for (i in 1:nchar(a)){
      if (substr(a, i, i) %in% b){
        ind <- c(ind, TRUE)
      }
      else{
        ind <- c(ind, FALSE)
      }
    }
    ind <- all(ind)
    return(ind)
  }
  else{
    return(FALSE)
  }
}
l.letters <- c()
for (i in seq.int(n)){
  a <- database[i]
  if (match.1(a, l.9) && check.once(a)){
    l.letters <- c(l.letters, a)
  }
}

# Question 2

setwd('/home/cy302/SPA1/grade/student_grades/')
dataFiles <- lapply(Sys.glob("student*.dat.txt"), read.table)
crib <- read.table('/home/cy302/SPA1/grade/crib.dat.txt')
grade <- read.table('/home/cy302/SPA1/grade/grade.txt')

correct <- c()
for (i in 1:length(dataFiles)){
  mark <- 0
  a <- data.matrix(dataFiles[[i]]$V1[2:31])
  b <- data.matrix(dataFiles[[i]]$V2[2:31])
  c <- data.matrix(crib$V1)
  for (j in 1:30){
    ind <- as.numeric(a[j])
    student.answer <- b[j]
    right.answer <- c[ind]
    if (student.answer==right.answer){
      mark <- mark+1
    }
  }
  correct <- c(correct,mark)
}
correct <- c(correct[4:12], correct[1:3])
grade.crit <- read.table('/home/cy302/SPA1/grade/grade.txt', header=TRUE)
min <- grade.crit$min
max <- grade.crit$max
grade <- as.vector(grade.crit$grade)
alpha.grade <- rep(0, 12)
for (i in 1:12){
  g <- correct[i]
  g.percentage <- floor((g/30)*100)
  counter <- 1
  for (j in 1:5){
    if (min[j]>g.percentage){
      counter <- counter + 1
    }
    else{
      break
    }
  }
  alpha.grade[i] <- grade[counter]
}
rank.student <- rank(-correct, ties.method = 'min')
num.students <- 12
results <- data.frame(student=1:num.students, score=correct, 
                      grade=alpha.grade, rank=rank.student)
print(results)

check.sim <- function(a, b){
  if (abs(correct[a]-correct[b])<=2){
    return(FALSE)
  }
  else{
    if (length(intersect(dataFiles[a]$V1[2:31], dataFiles[b]$V1[2:31]))>=15){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
}


# Question 3

# simulation project
# part 1
L <- 20
v <- rep(5, L)
l.lambda <- 0.9
r.lambda <- 1.2
V <- matrix(0, 50, L)
V[1,] <- v
d <- 0.1
s <- rep(0, 50)
s[1] <- 5*L
for (t in 1:49){
  # compute the geometric growth within patches
  M <- c(l.lambda*V[t, 1:10], r.lambda*V[t, 11:20])
  V[t+1, 2:19] <- (1-2*d)*M[2:19] + d*M[1:18] + d*M[3:20]
  V[t+1, 1] <- (1-d)*M[1] + d*M[2]
  V[t+1, 20] <- (1-d)*M[20] + d*M[19]
  s[t+1] <- sum(V[t,])
}

plot(1:50, log(s), type='o', xlim=c(1, 50),
     ylim=c(4, 14), xlab='days', 
     ylab='log(total population size)')
title('plot of log(total population) against time 1:50')

simulation.project <- function(L, d){
  # this function is as required in the second part of
  # the question
  v <- rep(5, L)
  l.lambda <- 0.9
  r.lambda <- 1.2
  V <- matrix(0, 50, L)
  V[1,] <- v
  s <- rep(0, 50)
  s[1] <- 5*L
  
  for (t in 1:49){
    # compute the geometric growth within patches
    M <- c(l.lambda*V[t, 1:as.integer(L/2)], r.lambda*V[t, (as.integer(L/2)+1):L])
    V[t+1, 2:L-1] <- (1-2*d)*M[2:(L-1)] + d*M[1:(L-1)] + d*M[3:L]
    V[t+1, 1] <- (1-d)*M[1] + d*M[2]
    V[t+1, 20] <- (1-d)*M[L] + d*M[L-1]
    s[t+1] <- sum(V[t+1,])
  }
  
  plot(1:50, log(s), type='o', xlab='days', 
       ylab='log(total population size)')
  return(s)
}

reflecting <- function(M, d){
  m <- length(M)
  N.1 <- (1-d)*M[1]+d*M[2]
  N.L <- (1-d)*M[m]+d*M[m-1]
  N <- (1-2*d)*M[2:(m-1)]+d*M[1:(m-2)]+d*M[3:m]
  N <- c(N.1, N, N.L)
  return(N)
}

population.growth <- function(a){
  if (typeof(a)!='double'){
    return(FALSE)
  }
  else{
    n <- length(a)
    growth <- c()
    for (i in 1:(n-1)){
      growth[i] <- (a[i+1]-a[i])/a[i]
    }
    return(growth)
  }
}
# now we need a modified version of the simulation.project() function
simulation.project.2 <- function(d, L, spatial){
  v <- rep(5, L)
  l.lambda <- 0.9
  r.lambda <- 1.2
  V <- matrix(0, 50, L)
  V[1,] <- v
  s <- rep(0, 50)
  s[1] <- 5*L
  
  for (t in 1:49){
    # compute the geometric growth within patches
    M <- c(l.lambda*V[t, spatial], r.lambda*V[t, c(1:L)[-spatial]])
    V[t+1,] <- reflecting(M, d)
    s[t+1] <- sum(V[t+1,])
  }
  return(s)
}

ma <- matrix(0, 10, 4)
ma[,1] <- c(1:10)
ma[,2] <- c(5:14)
ma[,3] <- sample(c(1:20))[1:10]
ma[,4] <- sample(c(1:20))[1:10]
par(mfrow=c(2, 2))
par(mar=c(3, 3, 1.5, 0.5), mgp=c(2.5, 1.0))
for (i in 1:4){
  plot(c(1:49), population.growth(simulation.project.2(0.1, 20, ma[,i])), 
       type='l', ylim=c(0.00, 1.00), xlab='t', ylab='growth rate', 
       title='plot of growth rates against time')
}




