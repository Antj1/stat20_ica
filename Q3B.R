rm(list=ls(all=TRUE))
dev.off()
library(pracma)
library(latex2exp)

mu <- 0 
sigma <- 1
lambda <- 1





PL_LDA = function(x) {
  lambda * (1/2 - 1/2*erf((log(x)-mu)/(sigma*sqrt(2))))
  }

dev.off()
x_s = 0:100
plot(x_s,PL_LDA(x_s),main = "First order asymptotic approximation for the Poisson-Lognormal model Tail Function",xlab = "x",ylab="Tail Function","l",ylim = c(0,1))

#Lambda tests______________________________________________________
dev.off()
x_s = 0:150
sigma_lambdatest <- 1
mu_lambdatest <- 0
lambdas = c(1,5,10)
color = c("red","blue","green")

for(i in 1:3){
  PL = function(x) {
    lambdas[i] * (1/2 - 1/2*erf((log(x)-mu_lambdatest)/(sigma_lambdatest*sqrt(2))))
  }
  
  plot(x_s,PL(x_s),main = "First order asymptotic approximation for the Poisson-Lognormal model Tail Function",xlab = "x",ylab="Tail Function","l",ylim = c(0,1),col = color[i])
  par(new = TRUE)
}
legend(100,1,legend = c("lambda =1","lambda =5","lambda =10"),col = c("red","blue","green"),lty = 1)
#Mu tests______________________________________________________
dev.off()
x_s = 0:100
lambda_mutest <- 1
sigma_mutest <- 1
mus = c(0,1,5)
color = c("red","blue","green")

for(i in 1:3){
  PL = function(x) {
    lambda_mutest * (1/2 - 1/2*erf((log(x)-mus[i])/(sigma_mutest*sqrt(2))))
  }
  plot(x_s,PL(x_s),"l",main = "First order asymptotic approximation for the Poisson-Lognormal model Tail Function",xlab = "x",ylab="Tail Function",ylim = c(0,1),col = color[i])
  par(new = TRUE)
}
legend(85,1,legend = c("mu =0","mu =1","mu =5"),col = c("red","blue","green"),lty = 1)
#Sigma tests______________________________________________________
dev.off()
lambda_sigmatest <- 1
mu_sigmatest <- 0
sigmas = c(1,5,10)
color = c("red","blue","green")

for(i in 1:3){
  PL = function(x) {
    lambda_sigmatest * (1/2 - 1/2*erf((log(x)-mu_sigmatest)/(sigmas[i]*sqrt(2))))
  }
  plot(x_s,PL(x_s),main = "First order asymptotic approximation for the Poisson-Lognormal model Tail Function",xlab = "x",ylab="Tail Function",ylim = c(0,1),"l",col = color[i])
  par(new = TRUE)
}
legend(80,1,legend = c("Sigma =1","Sigma =5","Sigma =10"),col = c("red","blue","green"),lty = 1)
