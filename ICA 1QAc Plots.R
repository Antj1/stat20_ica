rm(list=ls(all=TRUE))

dev.off()
lambda <- 2
bet <- 2
alph <- 3

###PDF

dens <- function(z,n) {partialsum = 0

for(k in 1:n){
  partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) * bet^(alph*k)*z*(alph * k -1) * exp(-bet*z)/gamma(alph*k)}

return(partialsum)
}

zs1 = seq(0, 10, by = 0.0005)

plot(zs1,dens(zs1,200),"l")




####CDF

gammadist <- function(u,i) {bet^(alph*i)*u*(alph * i -1) * exp(-bet*u)/gamma(alph*i)}


cdf <- function(z,n) {partialsum = lambda * exp(-lambda)
if(n>0){
  
  for(k in 1:n){
    partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) * integrate(gammadist,0,z,i=k)$value
  } 
} 

return(partialsum)
}


zs2 = seq(0, 10, by = 0.01)
cdf_vals_list <- c(length(zs2))

for(i in 1:length(zs2)){
  cdf_vals_list[i] <- cdf(zs2[i],20)}


plot(zs2,cdf_vals_list,"l")

#Monte Carlo Simulation_______________________________________________________________

poisson_gamma_montecarlo <- function(lambda,alpha,beta,nsim){
  
  annual_loss <- rep(NA,nsim)
  
  for (i in 1:nsim){
    
    no_of_losses <- rpois(1,lambda)
    
    if (no_of_losses == 0){
      
      annual_loss[i] <- 0
      
    } else{
      
      individual_losses = rgamma(no_of_losses,alpha, beta)
      
      agg_losses = sum(individual_losses)
      annual_loss[i] <- agg_losses
      
    }
    
  }

  
  return(annual_loss)

}

monty = poisson_gamma_montecarlo(lambda = lambda,alpha = alph,beta = bet, nsim = 10000)

hist(monty)

