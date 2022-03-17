rm(list=ls(all=TRUE))
dev.off()

library(pracma)
library(stats)
library(rmutil)
lambda <- 10 #rate of Poisson frequency model
gamm <- 0.01 #scale of Levy severity model
delt <- 0 #Location parameter of Levy severity model

delt_tilde = function(n,delt,gamm) {n*delt + (n^2 *abs(gamm) - n*gamm)} #Under convolution new delta

#Density of Poisson-Levy LDA Model-----------
dens <- function(z,n,lambda,delta,gamma) {partialsum = 0 
for(k in 1:n){
  if(delt_tilde(k,delta,gamma)<z){
    partialsum = partialsum + dpois(k,lambda)*dlevy(z,delt_tilde(k,delta,gamma),s=(k^2)*gamma)}
}
return(partialsum)
}

#Plotting density of Poisson-Levy LDA Model
zs1 = seq(0, 200, by = 0.5)

dens_vals_list <- c(1:length(zs1))

for(i in 1:length(zs1)){
  dens_vals_list[i] <- dens(zs1[i],n=20,lambda = 10, delta = 0, gamma= 0.01)}

plot(zs1,dens_vals_list,'l', xlab = 'z', ylab= 'f_ZN(z)')


#Distribution of Poisson-Levy LDA Model-----------
cdf <- function(z,n,lambda,delta,gamma) {partialsum = exp(-lambda)
for(k in 1:n){
  if(delt_tilde(k,delta,gamma)<z){
    partialsum = partialsum + dpois(k,lambda)*plevy(z,delt_tilde(k,delta,gamma),s=(k^2)*gamma)}
}
return(partialsum)
}

#Plotting distribution of Poisson-Levy LDA Model
zs2 = seq(0, 2000, by = 1)

dens_vals_list <- c(1:length(zs1))
cdf_vals_list <- c(1:length(zs2))

for(i in 1:length(zs2)){
  cdf_vals_list[i] <- cdf(zs2[i],n=20,lambda = 10, delta = 0, gamma= 0.01)}

plot(zs2,cdf_vals_list,'l',ylim=c(0,1), xlab = 'z', ylab= 'F_ZN(z)')

#Testing different values of lambda, alpha and beta

#Lambda tests____________________________________________________
dev.off()
zs1 = seq(0, 100, by = 0.5)
zs2 = seq(0, 200, by = 1)
dens_vals_list <- c(1:length(zs1))
cdf_vals_list <- c(1:length(zs2))

delta_lambdatest <- 0 #Setting our delta and gamma parameters
gamma_lambdatest <- 0.01

lambdas = c(8,10,12)#We are going to plot the model for lambda = 8, 10 and 12

color = c("red","blue","green")#Defining different colours to see the difference in plots

for(i in 1:3){ #Testing each parameter value for the density function
  
  for(j in 1:length(zs1)){
    dens_vals_list[j] <- dens(zs1[j],n=20,lambda = lambdas[i], delta = delta_lambdatest, gamma= gamma_lambdatest)}
  
  plot(zs1,dens_vals_list,main = "Density of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_ZN(z)",xlim = c(0,30),ylim = c(0,0.35),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.20,legend = c("lambda = 8","lambda = 10","lambda = 12"),col = c("red","blue","green"),lty = 1)
legend(20,0.1, legend = c("delta = 0","gamma = 0.01"))

dev.off()
for(i in 1:3){ #Testing each parameter value for the distribution function
  
  for(j in 1:length(zs2)){
    cdf_vals_list[j] <- cdf(zs2[j],n=20,lambda = lambdas[i], delta = delta_lambdatest, gamma= gamma_lambdatest)}
  
  plot(zs2,cdf_vals_list,main = "Distribution of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Distribution : F_ZN(z)",xlim = c(0,200),ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(150,0.60,legend = c("lambda = 8","lambda = 10","lambda = 12"),col = c("red","blue","green"),lty = 1)
legend(150,0.35, legend = c("delta = 0","gamma = 0.01"))


#Delta tests____________________________________________________
dev.off()
zs1 = seq(0, 100, by = 0.05)
zs2 = seq(0, 200, by = 0.05)

lambda_deltatest <- 10 #Setting our gamma and lambda parameters
gamma_deltatest <- 0.01

deltas = c(-0.1,0,0.1)#We are going to plot the model for delta = -0.1, 0 and 0.1
color = c("red","blue","green")

for(i in 1:3){ #Testing each parameter value for the density function
  
  for(j in 1:length(zs1)){
    dens_vals_list[j] <- dens(zs1[j],n=20,lambda = lambda_deltatest, delta = deltas[i], gamma= gamma_deltatest)}
  
  plot(zs1,dens_vals_list,main = "Density of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_ZN(z)",xlim = c(0,30),ylim = c(0,0.40),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.20,legend = c("delta = -0.1","delta = 0","delta = 0.1"),col = c("red","blue","green"),lty = 1)
legend(20,0.1, legend = c("lambda = 10","gamma = 0.01"))

dev.off()
for(i in 1:3){ #Testing each parameter value for the distribution function
  
  for(j in 1:length(zs2)){
    cdf_vals_list[j] <- cdf(zs2[j],n=20,lambda = lambda_deltatest, delta = deltas[i], gamma= gamma_deltatest)}
  
  plot(zs2,cdf_vals_list,main = "Distrbution of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : F_ZN(z)",xlim = c(0,50),ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.50,legend = c("delta = -0.1","delta = 0","delta = 0.1"),col = c("red","blue","green"),lty = 1)
legend(20,0.20, legend = c("lambda = 10","gamma = 0.01"))

#Gamma tests____________________________________________________
dev.off()
zs1 = seq(0, 100, by = 0.05)
zs2 = seq(0, 200, by = 0.05)

lambda_gammatest <- 10 #Setting our delta and lambda parameters
delta_gammatest <- 0.01
gammas = c(0.005,0.01,0.02)#We are going to plot the model for gamma = 0.005,0.01 and 0.02
color = c("red","blue","green")

for(i in 1:3){ #Testing each parameter value for the density function
  
  for(j in 1:length(zs1)){
    dens_vals_list[j] <- dens(zs1[j],n=20,lambda = lambda_gammatest, delta = delta_gammatest, gamma = gammas[i])}
  
  plot(zs1,dens_vals_list,main = "Density of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_ZN(z)",xlim = c(0,30),ylim = c(0,0.4),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.20,legend = c("gamma = -0.005","gamma = 0.01","gamma = 0.02"),col = c("red","blue","green"),lty = 1)
legend(20,0.1, legend = c("lambda = 10","delta = 0.01"))

dev.off()
for(i in 1:3){ #Testing each parameter value for the distribution function
  
  for(j in 1:length(zs2)){
    cdf_vals_list[j] <- cdf(zs2[j],n=20,lambda = lambda_gammatest, delta = delta_gammatest, gamma = gammas[i])}
  
  plot(zs2,cdf_vals_list,main = "Density of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_ZN(z)",xlim = c(0,30),ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.50,legend = c("gamma = -0.005","gamma = 0.01","gamma = 0.02"),col = c("red","blue","green"),lty = 1)
legend(20,0.2, legend = c("lambda = 10","delta = 0.01"))

#Monte Carlo Simulation_______________________________________________________________
dev.off()
poisson_levy_montecarlo <- function(lambda,gamma,delta,nsim){
  
  annual_loss <- rep(NA,nsim)
  
  for (i in 1:nsim){ #We will run nsim = 10,000 simulations
    
    no_of_losses <- rpois(1,lambda) #N drawn from poisson distribution
    
    if (no_of_losses == 0){
      
      annual_loss[i] <- 0
      
    } else{
      
      individual_losses = rlevy(no_of_losses,gamma, delta)#severity X drawn from levy distribution
      
      agg_losses = sum(individual_losses)
      annual_loss[i] <- agg_losses
      
    }
    
  }
  
  
  return(annual_loss)
  
}
levy = poisson_levy_montecarlo(lambda = lambda,gamma = gamm, delta = delt, nsim = 10000)
#Histogram for simulations is plot
hist(levy, main = "Monte Carlo Simulation of Poisson Levy LDA Model" ,xaxt='n' , xlab = "Loss Amount ")


#Monte-carlo Simulations for parameter testing-----------------------
zs1 = seq(0, 100, by = 0.05)
zs2 = seq(0, 200, by = 0.05)

#Setting parameters for gamma, delta and lambda for each test
delta_lambdatest <- 0 
gamma_lambdatest <- 0.01

lambda_gammatest <- 10
delta_gammatest <- 0

lambdas = c(8,10,12)
deltas = c(0,0.05,0.1)
gammas = c(0.005,0.01,0.02)

color = c("red","blue","green")

#Lambda tests____________________________________________________

dev.off()
par(mfrow=c(2,3))

for(i in 1:3){#Testing the Monte carlo distribution for values of lambda
  monty_test = poisson_levy_montecarlo(lambda = lambdas[i],gamma = gamma_lambdatest, delta = delta_lambdatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test,col = color[i],xlab = "Loss Amount ",main = " Poisson-Levy LDA Monte Carlo Sim.",xaxt='n',cex.main = 0.94,ylim = c(0,2500))
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],xlab = "Loss Amount ",main = "Cumulative Annual Loss Distribution",xaxt='n',cex.main = 0.94)
}
legend(15,6000,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(15,3000, legend = c("alpha = 3","beta = 2"),cex = 0.75)

#Gamma tests____________________________________________________
dev.off()
par(mfrow=c(2,3))
for(i in 1:3){#Testing the Monte carlo distribution for values of gamma
  monty_test = poisson_levy_montecarlo(lambda = lambda_gammatest,gamma = gammas[i], delta = delta_gammatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test,col = color[i],xaxt='n',xlab = "Loss Amount ",main = " Poisson-Levy LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,4000))
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],xlab = "Loss Amount ", main = "Cumulative Annual Loss Distribution",xaxt='n',cex.main = 0.94)
}
legend(6,6000,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(6,3000, legend = c("lambda = 5","alpha = 3"),cex = 0.75)


