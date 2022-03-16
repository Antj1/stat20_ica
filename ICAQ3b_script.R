rm(list=ls(all=TRUE))
dev.off()
library(pracma)
library(latex2exp)

#Defining variables used in First order asymptotic approximation for the Poisson-Lognormal model tail function
mu <- 0 
sigma <- 1
lambda <- 1


PL_LDA = function(x) {
  lambda * (1/2 - 1/2*erf((log(x)-mu)/(sigma*sqrt(2))))#This is the equation found from part a)
}

x_s = 0:100
plot(x_s,PL_LDA(x_s),main = "1st Order Asymptotic Approximation for Poisson-Lognormal model Tail Function", cex.main=1.05 ,xlab = "x",ylab=TeX(r'(Tail Function  $\bar{F}_z(x)$ approximation)'),cex.lab=0.9,"l",ylim = c(0,1)) #We plot the tail function from x values going from 1-100.

legend(80,1,legend = c(TeX(r'($\lambda = 0)'),TeX(r'($\sigma = 0)'),TeX(r'($\mu = 0)')))

#Lambda tests______________________________________________________
dev.off()
x_s = 0:150
sigma_lambdatest <- 1 #We are going to plot the model for lambda = 1, 5 and 10
mu_lambdatest <- 0
lambdas = c(1,5,10)
color = c("red","blue","green")

for(i in 1:3){
  PL = function(x) {
    lambdas[i] * (1/2 - 1/2*erf((log(x)-mu_lambdatest)/(sigma_lambdatest*sqrt(2))))
  }
  
  plot(x_s,PL(x_s),main = "1st Order Asymptotic Approximation for Poisson-Lognormal model Tail Function", cex.main=1.05 ,xlab = "x",ylab=TeX(r'(Tail Function  $\bar{F}_z(x)$ approximation)'),cex.lab=0.9,"l",ylim = c(0,1),col = color[i])
  par(new = TRUE)
}
legend(100,1,legend = c(TeX(r'($\lambda = 1)'),TeX(r'($\lambda = 5)'),TeX(r'($\lambda = 10)')),col = c("red","blue","green"),lty = 1)
legend(100,0.7, legend = c(TeX(r'($\mu = 0)'),TeX(r'($\sigma = 1)')))

#Mu tests______________________________________________________
dev.off()
x_s = 0:100
lambda_mutest <- 1
sigma_mutest <- 1
mus = c(0,1,5) #We are going to plot the model for mu = 0, 1, and 5
color = c("red","blue","green")

for(i in 1:3){
  PL = function(x) {
    lambda_mutest * (1/2 - 1/2*erf((log(x)-mus[i])/(sigma_mutest*sqrt(2))))
  }
  plot(x_s,PL(x_s),"l",main = "1st Order Asymptotic Approximation for Poisson-Lognormal model Tail Function", cex.main=1.05 ,xlab = "x",ylab=TeX(r'(Tail Function  $\bar{F}_z(x)$ approximation)'),cex.lab=0.9,ylim = c(0,1),col = color[i])
  par(new = TRUE)
}
legend(85,1.02,legend = c(TeX(r'($\mu = 0)'),TeX(r'($\mu = 1)'),TeX(r'($\mu = 5)')),col = c("red","blue","green"),lty = 1)
legend(85,0.64, legend = c(TeX(r'($\lambda = 1)'),TeX(r'($\sigma = 1)')))

#Sigma tests______________________________________________________
dev.off()
lambda_sigmatest <- 1
mu_sigmatest <- 0
sigmas = c(1,5,10) #We are going to plot the model for sigma = 1, 5 and 10
color = c("red","blue","green")

for(i in 1:3){
  PL = function(x) {
    lambda_sigmatest * (1/2 - 1/2*erf((log(x)-mu_sigmatest)/(sigmas[i]*sqrt(2))))
  }
  plot(x_s,PL(x_s),main = "1st Order Asymptotic Approximation for Poisson-Lognormal model Tail Function", cex.main=1.05 ,xlab = "x",ylab=TeX(r'(Tail Function  $\bar{F}_z(x)$ approximation)'),cex.lab=0.9,ylim = c(0,1),"l",col = color[i])
  par(new = TRUE)
}
legend(83,1,legend = c(TeX(r'($\sigma = 0)'),TeX(r'($\sigma = 1)'),TeX(r'($\sigma = 5)')),col = c("red","blue","green"),lty = 1)
legend(83,0.7, legend = c(TeX(r'($\mu = 1)'),TeX(r'($\lambda = 1)')))

