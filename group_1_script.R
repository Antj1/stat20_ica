rm(list=ls(all=TRUE))
library(pracma)
library(stats)



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
legend(7.5,0.20,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(7.5,0.13, legend = c("alpha = 3","beta = 2"))

#We see that the peak height reduces as lambda increases.

dev.off()
for(i in 1:3){#Testing each parameter value for the distribution function
  pgcdf = PGcdf(zs2,20,alpha_lambdatest,beta_lambdatest,lambda = lambdas[i])
  
  #Plotting the distribution
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(15,0.66,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1)
legend(15,0.35, legend = c("alpha = 3","beta = 2"))



#Alpha tests____________________________________________________
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
legend(7.8,0.80,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1)
legend(7.8,0.5, legend = c("lambda = 1","beta = 2"))

#We see that that the peak position shifts to the right as alpha increases.


for(i in 1:3){#Testing each parameter value for the density function
  pgcdf = PGcdf(zs2,20,alph = alphas[i],beta_alphatest,lambda_alphatest)
  
  #Plotting the distribution
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "z",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(15,0.66,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1)
legend(15,0.36, legend = c("lambda = 1","beta = 2"))



#Beta tests____________________________________________________
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

legend(8,0.4,legend = c("beta = 1","beta = 2","beta = 3"),col = c("red","blue","green"),lty = 1)
legend(8,0.23, legend = c("lambda = 1","alpha = 2"))


#We can see heavier tails as beta gets smaller.

for(i in 1:3){#Testing each parameter value for the distribution function
  pgcdf = PGcdf(zs2,20,alpha_betatest,bet = betas[i],lambda_alphatest)
  
  #Plotting the distribution
  plot(zs2,pgcdf,main = "Distribution of LDA Model, Poisson Frequency-Gamma Severity", cex.main=1.05 ,xlab = "x",ylab="Distribution : F_Zn(z)",ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  par(new = TRUE)
}
legend(16,0.66,legend = c("beta = 1","beta = 2","beta = 3"),col = c("red","blue","green"),lty = 1)
legend(16,0.38, legend = c("lambda = 1","alpha = 2"))

#Q1A Part e)

#Q1A PART E-------------------------
#Monte Carlo Simulation_______________________________________________________________
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



my_hist <- hist(monty,ylim = c(0,2500),xlab='Losses', main='Poisson-Gamma LDA Monte-Carlo') #Plotting the histogram and Storing histogram info
my_hist$counts <- cumsum(my_hist$counts)    # Change histogram counts
plot(my_hist,xlab='Losses',main='Cumulative Poisson-Gamma LDA Monte-Carlo')



#Montecarlo Simulations with Testing
zs1 = seq(0, 10, by = 0.05)
zs2 = seq(0, 20, by = 0.05)

alpha_lambdatest <- 3 #Setting consistent parameters for alpha , beta and lambda for each tes
beta_lambdatest <- 2

alpha_betatest <- 2 
lambda_betatest <- 5

beta_alphatest <- 2 
lambda_alphatest <- 5

alphas = c(1,3,5)#We are going to plot the model for alpha = 1, 3 and 5
betas = c(1,2,3)#We are going to plot the model for beta = 1, 2 and 3
lambdas = c(2,4,6)#We are going to plot the model for lambda = 2, 4 and 6

color = c("red","blue","green")



#Lambda tests____________________________________________________

par(mfrow=c(2,3))

for(i in 1:3){#Testing the Monte carlo distribution for values of lambda
  monty_test = poisson_gamma_montecarlo(lambda = lambdas[i],alpha = alpha_lambdatest,beta = beta_lambdatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,2500),xlab='Losses')
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],main = "Histogram of Annual Loss Distribution",cex.main = 0.94, xlab='Losses')
}
legend(15,6000,legend = c("lambda = 1","lambda = 3","lambda = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(15,2700, legend = c("alpha = 3","beta = 2"),cex = 0.75)




#Alpha tests____________________________________________________
par(mfrow=c(2,3))
for(i in 1:3){#Testing the Monte carlo distribution for values of alphas
  monty_test = poisson_gamma_montecarlo(lambda_alphatest,alph = alphas[i],beta_alphatest, nsim = 10000)
  par(new=FALSE)
  
  cuml_hist <- hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,3000), xlab='Losses')
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],main = "Histogram of Annual Loss Distribution",cex.main = 0.94, xlab='Losses')
}

legend(20,6000,legend = c("alpha = 1","alpha = 3","alpha = 5"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(20,2700, legend = c("lambda = 5","beta = 2"),cex = 0.75)



#Beta tests____________________________________________________
par(mfrow=c(2,3))
for(i in 1:3){#Testing the Monte carlo distribution for values of beta
  monty_test = poisson_gamma_montecarlo(lambda_betatest,alpha_betatest,beta = betas[i], nsim = 10000)
  par(new=FALSE)
  
  cuml_hist <- hist(monty_test,col = color[i],main = "Histogram of Poisson-Gamma LDA Monte Carlo Sim.",cex.main = 0.94,xlab = "Loss amount",ylab = "Frequency",ylim = c(0,3000))
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],main = "Histogram of Annual Loss Distribution",cex.main = 0.94, xlab='Losses')
}
legend(6,6000,legend = c("beta = 1","beta = 2","beta = 3"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(6,2700, legend = c("lambda = 5","alpha = 3"),cex = 0.75)

#We can see that the Monte-Carlo simulations produce very similar graph shapes to their respective density and distribution graphs.


#Q1B Part C)
dev.off()
rm(list=ls(all=TRUE))
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

#We can see as we increase lambda the peak height decreases.

for(i in 1:3){ #Testing each parameter value for the distribution function
  
  for(j in 1:length(zs2)){
    cdf_vals_list[j] <- cdf(zs2[j],n=20,lambda = lambdas[i], delta = delta_lambdatest, gamma= gamma_lambdatest)}
  
  plot(zs2,cdf_vals_list,main = "Distribution of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Distribution : F_ZN(z)",xlim = c(0,200),ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(150,0.65,legend = c("lambda = 8","lambda = 10","lambda = 12"),col = c("red","blue","green"),lty = 1)
legend(150,0.35, legend = c("delta = 0","gamma = 0.01"))



#Delta tests____________________________________________________
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
legend(20,0.3,legend = c("delta = -0.1","delta = 0","delta = 0.1"),col = c("red","blue","green"),lty = 1)
legend(20,0.18, legend = c("lambda = 10","gamma = 0.01"))

#We can see as we increase delta the peak location shifts to the right.

for(i in 1:3){ #Testing each parameter value for the distribution function
  
  for(j in 1:length(zs2)){
    cdf_vals_list[j] <- cdf(zs2[j],n=20,lambda = lambda_deltatest, delta = deltas[i], gamma= gamma_deltatest)}
  
  plot(zs2,cdf_vals_list,main = "Distrbution of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : F_ZN(z)",xlim = c(0,50),ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.50,legend = c("delta = -0.1","delta = 0","delta = 0.1"),col = c("red","blue","green"),lty = 1)
legend(20,0.20, legend = c("lambda = 10","gamma = 0.01"))



#Gamma tests____________________________________________________
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
legend(20,0.30,legend = c("gamma = -0.005","gamma = 0.01","gamma = 0.02"),col = c("red","blue","green"),lty = 1)
legend(20,0.18, legend = c("lambda = 10","delta = 0.01"))


#We can see as we increase gamma the distributions tail gets heavier.

for(i in 1:3){ #Testing each parameter value for the distribution function
  
  for(j in 1:length(zs2)){
    cdf_vals_list[j] <- cdf(zs2[j],n=20,lambda = lambda_gammatest, delta = delta_gammatest, gamma = gammas[i])}
  
  plot(zs2,cdf_vals_list,main = "Density of LDA Model, Poisson Frequency-Levy Severity", cex.main=1.05 ,xlab = "z",ylab="Density : f_ZN(z)",xlim = c(0,30),ylim = c(0,1),cex.lab=0.9,"l",col = color[i])
  
  par(new = TRUE)
}
legend(20,0.50,legend = c("gamma = -0.005","gamma = 0.01","gamma = 0.02"),col = c("red","blue","green"),lty = 1)
legend(20,0.2, legend = c("lambda = 10","delta = 0.01"))


#Q1B Part e)
lambda <- 10 #rate of Poisson frequency model
gamm <- 0.01 #scale of Levy severity model
delt <- 0 #Location parameter of Levy severity model
library(pracma)
library(stats)
library(rmutil)


#Monte Carlo Simulation_______________________________________________________________
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
par(mfrow=c(2,3))

for(i in 1:3){#Testing the Monte carlo distribution for values of lambda
  monty_test = poisson_levy_montecarlo(lambda = lambdas[i],gamma = gamma_lambdatest, delta = delta_lambdatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test,col = color[i],xlab = "Loss Amount ",main = " Poisson-Levy LDA Monte Carlo Sim.",xaxt='n',cex.main = 0.94,ylim = c(0,2500))
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],xlab = "Loss Amount ",main = "Cumulative Annual Loss Distribution",xaxt='n',cex.main = 0.94)
}
legend(0.15,6200,legend = c("lambda = 8","lambda = 10","lambda = 12"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(0.15,3000,legend = c("delta = 0","gamma = 0.01"),cex = 0.75)



#Gamma tests____________________________________________________
par(mfrow=c(2,3))
for(i in 1:3){#Testing the Monte-Carlo distribution for values of gamma
  monty_test = poisson_levy_montecarlo(lambda = lambda_gammatest,gamma = gammas[i], delta = delta_gammatest, nsim = 10000)
  par(new=FALSE)
  cuml_hist <- hist(monty_test,col = color[i],xaxt='n',xlab = "Loss Amount ",main = " Poisson-Levy LDA Monte Carlo Sim.",cex.main = 0.94,ylim = c(0,4000))
  
  
  cuml_hist$counts <- cumsum(cuml_hist$counts)
  plot(cuml_hist,col=color[i],xlab = "Loss Amount ", main = "Cumulative Annual Loss Distribution",xaxt='n',cex.main = 0.94)
}
legend(0.3,6200,legend = c("gamma = 0.005","gamma = 0.01","gamma = 0.02"),col = c("red","blue","green"),lty = 1,cex = 0.75)
legend(0.3,3000, legend = c("lambda = 10","delta = 0"),cex = 0.75)

#We can see the densities and cumulative distribution follow a similar shape to the histograms.

#Q3B
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

#We can see for different values of $\lambda$ that the tail does not vary much, which suggests that the tail behaviour is more dependent on the severity.


#Mu tests______________________________________________________
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

#As suggested earlier, we can see tail behaviour does vary when we change $\mu$ from the Lognormal severity distribution, as for larger values of $\mu$, we get heavier tails than smaller values. This makes sense as the mean ($e^{\mu}$) and median ($e^{\mu + \frac{\sigma^2}{2}}$) increase as $\mu$ increases.


#Sigma tests______________________________________________________
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
legend(83,1,legend = c(TeX(r'($\sigma = 1)'),TeX(r'($\sigma = 5)'),TeX(r'($\sigma = 10)')),col = c("red","blue","green"),lty = 1)
legend(83,0.7, legend = c(TeX(r'($\mu = 1)'),TeX(r'($\lambda = 1)')))


#We can also see we get heavier tails for larger values of $\sigma$ than smaller values, which makes sense given that the median of the Lognormal severity ($e^{\mu + \frac{\sigma^2}{2}}$) increases as $\sigma$ increases.

