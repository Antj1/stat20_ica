rm(list=ls(all=TRUE))
dev.off()

library(pracma)

library(rmutil)
lambda <- 10
gamm <- 0.01
delt <- 0

bet_tilde <- 1

gamm_tilde_power = function(n) {n * abs(gamm)^0.5}

delt_tilde = function(n) {n*delt + (n^2 *abs(gamm) - n*gamm)}


dens <- function(z,n) {partialsum = 0 
for(k in 1:n){
  if(delt_tilde(k)<z){
    partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) *  gamm_tilde_power(k)/sqrt(2*pi) * (z-delt_tilde(k))^(-3/2) * exp(-gamm_tilde_power(k)^2/(2*(z-delt_tilde(k))))}
}
return(partialsum)
}


zs1 = seq(0, 49, by = 1)
dens_vals_list <- c(length(zs1))

for(i in 1:length(zs1)){
  dens_vals_list[i] <- dens(zs1[i],2000)}

plot(zs1,dens_vals_list,"l")

#________________________________________________________________________________



cdf <- function(z,n) {partialsum = exp(-lambda) 
for(k in 1:n){
  if(delt_tilde(k)<z){
    partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) *  erfc(gamm_tilde_power(k)/sqrt((2*(z-delt_tilde(k)))))
  }
  return(partialsum)
}
}


zs2 = seq(0, 49, by = 1)
cdf_vals_list <- vector(mode = "numeric",length= length(zs2))

length(zs2)
length(cdf_vals_list)

for(i in 1:length(zs2)){
  cdf_vals_list[i] <- cdf(zs2[i],100)}

length(zs2)
length(cdf_vals_list)
plot(zs2,cdf_vals_list,ylim=c(0,0.02),"l")


#Monte Carlo Simulation_______________________________________________________________

poisson_levy_montecarlo <- function(lambda,gamma,delta,nsim){
  
  annual_loss <- rep(NA,nsim)
  
  for (i in 1:nsim){
    
    no_of_losses <- rpois(1,lambda)
    
    if (no_of_losses == 0){
      
      annual_loss[i] <- 0
      
    } else{
      
      individual_losses = rlevy(no_of_losses,gamma, delta)
      
      agg_losses = sum(individual_losses)
      annual_loss[i] <- agg_losses
      
    }
    
  }
  
  
  return(annual_loss)
  
}
levy = poisson_levy_montecarlo(lambda = lambda,gamma = gamm, delta = delt, nsim = 10000)

hist(levy)

