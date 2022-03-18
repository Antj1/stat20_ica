rm(list=ls(all=TRUE))
library(pracma)
library(sads)
dev.off()
#Setting the parameters
lambda <- 2
bet <- 2
alph <- 3

###PDF

#Defining our density function for Poisson-Gamma distribution
PGdens <- function(z,n,alph,bet,lambda) {partialsum = 0

for(k in 1:n){  #We want a sum, hence we use a for loop
  partialsum = partialsum + dpois(k,lambda)*dgamma(z,alph*k,bet) #Defining the sum using the equation we defined
  
  return(partialsum)
}}

zs1 = seq(0, 10, by = 0.05) #Our range of z values

#Plotting the density function for our parameter values
plot(zs1,PGdens(zs1,200,alph,bet,lambda),main = "Density of LDA Model, Poisson Frequency-Gamma Severity",cex.main=1.05 ,xlab = "z",ylab="Density : f_Zn(z)",cex.lab=0.9,"l")


####CDF
zs = seq(0, 20, by = 0.05)#Our range of z values

#Defining our distribution function for Poisson-Gamma distribution
PGcdf <- function(z,n,alph,bet,lambda) {partialsum = exp(-lambda)
if(n>0){
  
  for(k in 1:n){
    partialsum = partialsum +  dpois(k,lambda)* pgamma(z,alph*k,bet)#Defining the sum using the equation we defined
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

alpha_lambdatest <- 3 #Setting our alpha and beta parameters
beta_lambdatest <- 2

lambdas = c(1,3,5)#We are going to plot the model for lambda = 1, 3 and 5

color = c("red","blue","green")#Defining different colours to see the difference in plots

for(i in 1:3){ #Testing each parameter value for the density function
  pgdens = PGdens(zs1,200,alpha_lambdatest,beta_lambdatest,lambda = lambdas[i])
  
  #Plotting the density
  plot(zs1,pgdens,main = "Density of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_Zn(z)",ylim = c(0,0.25),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(8,0.20,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(8,0.13, legend = c("alpha = 3","beta = 2"))

dev.off()
for(i in 1:3){#Testing each parameter value for the distribution function
  pgcdf = PGcdf(zs2,20,alpha_lambdatest,beta_lambdatest,lambda = lambdas[i])
  
  #Plotting the distribution
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(16,0.42, legend = c("alpha = 3","beta = 2"))

#Alpha tests____________________________________________________
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)

beta_alphatest <- 2 #Setting our beta and lambda parameters
lambda_alphatest <- 1

alphas = c(1,3,5)#We are going to plot the model for alphas = 1, 3 and 5
color = c("red","blue","green")

for(i in 1:3){#Testing each parameter value for the density function
  pgdens = PGdens(zs1,200,alph = alphas[i],beta_alphatest,lambda_alphatest)
  
  #Plotting the density
  plot(zs1,pgdens,main = "Density of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(8,0.80,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1)
legend(8,0.53, legend = c("lambda = 1","beta = 2"))


dev.off()
for(i in 1:3){#Testing each parameter value for the density function
  pgcdf = PGcdf(zs2,20,alph = alphas[i],beta_alphatest,lambda_alphatest)
  
  #Plotting the distribution
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("alpha = 1","alpha = 5","alpha = 10"),col = c("red","blue","green"),lty = 1)
legend(16,0.42, legend = c("lambda = 1","beta = 2"))

#Beta tests____________________________________________________
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)
alpha_betatest <- 2 #Setting our alpha and lambda parameters
lambda_betatest <- 1
betas = c(1,2,3)#We are going to plot the model for betas = 1,2 and 3
color = c("red","blue","green")

for(i in 1:3){#Testing each parameter value for the density function
  pgdens = PGdens(zs1,200,alpha_betatest,bet = betas[i],lambda_alphatest)
  
  #Plotting the density
  plot(zs1,pgdens,main = "Density of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_Zn(z)",ylim = c(0,0.5),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(8,0.40,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1)
legend(8,0.23, legend = c("lambda = 1","alpha = 2"))

dev.off()
for(i in 1:3){#Testing each parameter value for the distribution function
  pgcdf = PGcdf(zs2,20,alpha_betatest,bet = betas[i],lambda_alphatest)
  
  #Plotting the distribution
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1)
legend(16,0.42, legend = c("lambda = 1","alpha = 2"))

#Monte Carlo Simulation_______________________________________________________________
dev.off()
lambda <- 2 # Deining intial parameters
bet <- 2
alph <- 3

poisson_gamma_montecarlo <- function(lambda,alpha,beta,nsim){
  
  annual_loss <- rep(NA,nsim)
  
  for (i in 1:nsim){ #We want to simulate for n yea so we use a for loop
    
    no_of_losses <- rpois(1,lambda) #Smpling the number of losses from the poisson distribution
    
    if (no_of_losses == 0){ #If the number of losses in that year is 0 then we want a zero value 
      
      annual_loss[i] <- 0
      
    } else{
      
      individual_losses = rgamma(no_of_losses,alpha, beta) #Sampling the individual losses that year from the Gamma distribution
      
      agg_losses = sum(individual_losses) #Aggregating the losses
      annual_loss[i] <- agg_losses #Assigning the loss for that year as the aggregate loss
      
    }
    
  }
  
  
  return(annual_loss)
  
}

monty = poisson_gamma_montecarlo(lambda = lambda,alpha = alph,beta = bet, nsim = 10000)

hist(monty,ylim = c(0,2500),xlab='Losses', main='Poisson-Gamma LDA Monte-Carlo') #Plotting the histogram
my_hist <- hist(monty,ylim = c(0,2500))       # Store histogram info
my_hist$counts <- cumsum(my_hist$counts)    # Change histogram counts
plot(my_hist,xlab='Losses',main='Cumulative Poisson-Gamma LDA Monte-Carlo')  

#Montecarlo Simulations
dev.off()
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)

alpha_lambdatest <- 3 #Setting consistent parameters for alpha , beta and lambda for each tes
beta_lambdatest <- 2

alpha_betatest <- 2 
lambda_betatest <- 5

beta_alphatest <- 2 
lambda_alphatest <- 5

alphas = c(1,3,5)#We are going to plot the model for alpha = 1, 3 and 5
betas = c(1,2,3)#We are going to plot the model for lambda = 1, 2 and 3
lambdas = c(2,4,6)#We are going to plot the model for lambda = 2, 4 and 6

color = c("red","blue","green")

#Lambda tests____________________________________________________

dev.off()
par(mfrow=c(2,3))

for(i in 1:3){#Testing the Monte carlo distribution for values of lambda
  monty_test = poisson_gamma_montecarlo(lambda = lambdas[i],alpha = alpha_lambdatest,beta = beta_lambdatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,2500))
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],main = "Histogram of Annual Loss Distribution",cex.main = 0.94)
}
legend(15,6000,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(15,3000, legend = c("alpha = 3","beta = 2"),cex = 0.75)


#Alpha tests____________________________________________________
dev.off()
par(mfrow=c(2,3))
for(i in 1:3){#Testing the Monte carlo distribution for values of alphas
  monty_test = poisson_gamma_montecarlo(lambda_alphatest,alph = alphas[i],beta_alphatest, nsim = 10000)
  par(new=FALSE)
  
  cuml_hist <- hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,3000))
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],main = "Histogram of Annual Loss Distribution",cex.main = 0.94)
}

legend(20,6000,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(20,3000, legend = c("lambda = 5","beta = 2"),cex = 0.75)

#Beta tests____________________________________________________
dev.off()
par(mfrow=c(2,3))
for(i in 1:3){#Testing the Monte carlo distribution for values of beta
  monty_test = poisson_gamma_montecarlo(lambda_betatest,alpha_betatest,beta = betas[i], nsim = 10000)
  par(new=FALSE)
  
  cuml_hist <- hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,xlab = "Loss amount",ylab = "",ylim = c(0,3000))
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],main = "Histogram of Annual Loss Distribution",cex.main = 0.94)
}
legend(6,6000,legend = c("beta = 1","beta = 3","beta = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(6,3000, legend = c("lambda = 5","alpha = 3"),cex = 0.75)

