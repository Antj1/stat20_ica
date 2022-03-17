rm(list=ls(all=TRUE))
library(pracma)
library(sads)
dev.off()
lambda <- 2
bet <- 2
alph <- 3

###PDF


PGdens <- function(z,n,alph,bet,lambda) {partialsum = 0

for(k in 1:n){
  partialsum = partialsum + dpois(k,lambda)*dgamma(z,alph*k,bet)
  
  return(partialsum)
}}

zs1 = seq(0, 10, by = 0.05)

plot(zs1,PGdens(zs1,200,alph,bet,lambda),main = "Density of LDA Model, Poisson Frequency-Gamma Severity",cex.main=1.05 ,xlab = "z",ylab="Density : f_Zn(z)",cex.lab=0.9,"l")


####CDF
zs = seq(0, 20, by = 0.05)

PGcdf <- function(z,n,alph,bet,lambda) {partialsum = exp(-lambda)
if(n>0){
  
  for(k in 1:n){
    partialsum = partialsum +  dpois(k,lambda)* pgamma(z,alph*k,bet)
  } 
} 

return(partialsum)
}


plot(zs,PGcdf(zs,50,alph,bet,lambda),main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity",cex.main=1.05 ,xlab = "z",ylab="Distribution : F_Zn(z)",cex.lab=0.9,"l")



#Testing different values of lambda, alpha and beta

#Lambda tests____________________________________________________
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)
alpha_lambdatest <- 3 #We are going to plot the model for lambda = 1, 5 and 10
beta_lambdatest <- 2
lambdas = c(1,3,5)
color = c("red","blue","green")

for(i in 1:3){
  pgdens = PGdens(zs1,200,alpha_lambdatest,beta_lambdatest,lambda = lambdas[i])
  
  plot(zs1,pgdens,main = "Density of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Density : f_Zn(z)",ylim = c(0,0.25),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(8,0.20,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(8,0.13, legend = c("alpha = 3","beta = 2"))

dev.off()
for(i in 1:3){
  pgcdf = PGcdf(zs2,20,alpha_lambdatest,beta_lambdatest,lambda = lambdas[i])
  
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(16,0.42, legend = c("alpha = 3","beta = 2"))

#Alpha tests____________________________________________________
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)
beta_alphatest <- 2 #We are going to plot the model for lambda = 1, 5 and 10
lambda_alphatest <- 1
alphas = c(1,3,5)
color = c("red","blue","green")

for(i in 1:3){
  pgdens = PGdens(zs1,200,alph = alphas[i],beta_alphatest,lambda_alphatest)
  
  plot(zs1,pgdens,main = "Density of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Density : f_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(8,0.80,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1)
legend(8,0.53, legend = c("lambda = 1","beta = 2"))

dev.off()
for(i in 1:3){
  pgcdf = PGcdf(zs2,20,alph = alphas[i],beta_alphatest,lambda_alphatest)
  
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("alpha = 1","alpha = 5","alpha = 10"),col = c("red","blue","green"),lty = 1)
legend(16,0.42, legend = c("lambda = 1","beta = 2"))

#Beta tests____________________________________________________
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)
alpha_betatest <- 2 #We are going to plot the model for lambda = 1, 5 and 10
lambda_betatest <- 1
betas = c(1,2,3)
color = c("red","blue","green")

for(i in 1:3){
  pgdens = PGdens(zs1,200,alpha_betatest,bet = betas[i],lambda_alphatest)
  
  plot(zs1,pgdens,main = "Density of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Density : f_Zn(z)",ylim = c(0,0.5),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(8,0.40,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1)
legend(8,0.23, legend = c("lambda = 1","alpha = 2"))

dev.off()
for(i in 1:3){
  pgcdf = PGcdf(zs2,20,alpha_betatest,bet = betas[i],lambda_alphatest)
  
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1)
legend(16,0.42, legend = c("lambda = 1","alpha = 2"))

#Monte Carlo Simulation_______________________________________________________________
dev.off()
lambda <- 2
bet <- 2
alph <- 3

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

hist(monty,ylim = c(0,2500))
my_hist <- hist(monty,ylim = c(0,2500))                   # Store histogram info
my_hist$counts <- cumsum(my_hist$counts)    # Change histogram counts
plot(my_hist)  

#Montecarlo Simulations
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)
alpha_lambdatest <- 3 #We are going to plot the model for lambda = 1, 5 and 10
beta_lambdatest <- 2
alpha_betatest <- 2 #We are going to plot the model for lambda = 1, 5 and 10
lambda_betatest <- 5
beta_alphatest <- 2 #We are going to plot the model for lambda = 1, 5 and 10
lambda_alphatest <- 5
alphas = c(1,3,5)
betas = c(1,2,3)
lambdas = c(2,4,6)
color = c("red","blue","green")

#Lambda tests____________________________________________________

#Density---
dev.off()
par(mfrow=c(1,3))
for(i in 1:3){
  monty_test = poisson_gamma_montecarlo(lambda = lambdas[i],alpha = alpha_lambdatest,beta = beta_lambdatest, nsim = 10000)
  par(new=FALSE)
  hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,2500))
}
legend(13,1500,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(15,1200, legend = c("alpha = 3","beta = 2"))

#Cumulative Distribution---
dev.off()
par(mfrow=c(1,3))
for(i in 1:3){
  monty_test = poisson_gamma_montecarlo(lambda = lambdas[i],alpha = alpha_lambdatest,beta = beta_lambdatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test);
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i])
  
}
legend(13,1500,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(15,1200, legend = c("alpha = 3","beta = 2"))


#Alpha tests____________________________________________________
dev.off()
par(mfrow=c(1,3))
for(i in 1:3){
  monty_test = poisson_gamma_montecarlo(lambda_alphatest,alph = alphas[i],beta_alphatest, nsim = 10000)
  par(new=FALSE)
  hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,3000))
}

legend(10,3000,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1)
legend(10,2400, legend = c("lambda = 5","beta = 2"))

#Beta tests____________________________________________________
dev.off()
par(mfrow=c(1,3))
for(i in 1:3){
  monty_test = poisson_gamma_montecarlo(lambda_betatest,alpha_betatest,beta = betas[i], nsim = 10000)
  par(new=FALSE)
  hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,xlab = "Loss amount",ylab = "",ylim = c(0,3000))
}
legend(4,3000,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1)
legend(4,2400, legend = c("lambda = 5","alpha = 3"))

#Cumulative ____________________________________________________

