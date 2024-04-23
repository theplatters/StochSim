library(pracma)

ex1.weibull_icmf <- function(x, alpha, beta){
  nthroot(- log(1-x) / alpha,beta)
}

ex1.weibull_rv <- function(alpha,beta,size=1){
  l = runif(size)
  lapply(l, \(x) weibull_icmf(x,alpha,beta))
}



ex3.pdf = function(x){
  exp(x) / (exp(1) - 1)
}

ex3.sampler <- function(size){
  i <- 1
  l <- list()
  while(i <= size){
    x = runif(1,min = 0,max = 1/(exp(1) -1))
    y = runif(1)
    
    if(y <= x){
      l <- c(l,y)
      i = i+1
    }
  }
  return(l)
}

ex4.sampler <- function(size){
  i <- 1
  l <- list()
  f <- \(x) 20 * x * (1- x)^3
  
  while(i <= size){
    y <- runif(1)
    u <- runif(1)
    if(u <= f(y) /(20 * y)){
      l <- c(l,y)
      i = i + 1
    }
  }
  return(l)
}

x5.erlang_sampler <- function(lambda,n,size = 1){
  l = zeros(size,1)
  lapply(l, function(e){
    u = runif(n)
    return(- 1/lambda * log(Reduce('*',u)))
  })
}

x6.box_mueller <- function(n){
  u <- runif(2 * n)
  l = list()
  for (i in seq(n)) {
    l <- c(l,sqrt(-2 * log(u[i])) * cos(2 * pi * u[2 * i]), sqrt(-2 * log(u[i])) * sin(2 * pi * u[2 * i]))
  }
  return(l)
}

x7.polar <- function(n){
  l <- list()
  i <- 1
  while(i <= n){
    u1 <- runif(1)[1]
    u2 <- runif(2)[1]
    
    v1 <- 2*u1 -1
    v2 <- 2*u2 -1
    rho <- v1^2 + v2^2

    if(rho <= 1){
      l <- c(l,v1 * sqrt(-2 * log(rho) /rho), v2 * sqrt(-2 * log(rho) /rho))
      i <- i + 1
    }
  }
  return(l)
}




