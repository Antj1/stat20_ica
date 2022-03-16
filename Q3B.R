rm(list=ls(all=TRUE))
dev.off()
library(pracma)

mu <- 0 
sigma <- 1
lambda <- 1


PL_LDA = function(x) {
  lambda * (1/2 - 1/2*erf((log(x)-mu)/(sigma*sqrt(2))))
  }


x_s = 0:200
plot(x_s,PL_LDA(x_s),"l")
