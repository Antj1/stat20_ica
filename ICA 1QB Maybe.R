rm(list=ls(all=TRUE))
dev.off()

library(pracma)
library(stats)
library(rmutil)
lambda <- 10 #8,10,12
gamm <- 0.01 #0.01,0.05,0.1
delt <- 0 #-0.05,0,0.05


bet_tilde <- 1

gamm_tilde_power = function(n) {n * abs(gamm)^0.5}

delt_tilde = function(n) {n*delt + (n^2 *abs(gamm) - n*gamm)}

#Density-----------
dens <- function(z,n) {partialsum = 0 
for(k in 1:n){
  if(delt_tilde(k)<z){
    partialsum = partialsum + dpois(k,lambda)*dlevy(z,delt_tilde(k),s=(k^2)*gamm)}
}
return(partialsum)
}

zs1 = seq(0, 200, by = 0.5)
#plot(zs1,dens(zs1,10),'l')
dens_vals_list <- c(1:length(zs1))

for(i in 1:length(zs1)){
  dens_vals_list[i] <- dens(zs1[i],20)}

plot(zs1,dens_vals_list,'l')

#Distribution------------
cdf <- function(z,n) {partialsum = 0 
for(k in 1:n){
  if(delt_tilde(k)<z){
    partialsum = partialsum + dpois(k,lambda)*plevy(z,delt_tilde(k),s=(k^2)*gamm)}
}
return(partialsum)
}

zs2 = seq(0, 2000, by = 1)
#plot(zs1,dens(zs1,10),'l')
cdf_vals_list <- c(1:length(zs2))

for(i in 1:length(zs2)){
  cdf_vals_list[i] <- cdf(zs2[i],20)}

plot(zs2,cdf_vals_list,'l',ylim=c(0,1))

#Monte Carlo Simulation_______________________________________________________________

poisson_levy_montecarlo <- function(lambda,gamma,delta,nsim){
  
  annual_loss <- rep(NA,nsim)
  
  for (i in 1:nsim){
    
    no_of_losses <- rpois(1,lambda)
    
    if (no_of_losses == 0){
      
      annual_loss[i] <- 0
      
    } else{
      
      individual_losses = rlevy(no_of_losses,delta, gamma)
      
      agg_losses = sum(individual_losses)
      annual_loss[i] <- agg_losses
      
    }
    
  }
  
  
  return(annual_loss)
  
}
levy = poisson_levy_montecarlo(lambda = lambda,gamma = gamm, delta = delt, nsim = 10000)

hist(levy)

####################
poisson_levy_montecarlo <- function(l,g,d,nsim){
  
  annual_loss <- rep(NA,nsim)
  
  for (i in 1:nsim){
    
    number_of_losses <- rpois(1,l)
    
    if (number_of_losses == 0){
      
      annual_loss[i] <- 0
      
    } else{
      
      severities <- rlevy(number_of_losses,d,g)

      annual_loss[i] <- sum(severities)
      
    }
    return(annual_loss)
    
  }}
hist(poisson_levy_montecarlo(l = lambda,g = gamm, d=delt,nsim=10000))

  



