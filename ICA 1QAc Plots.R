lambda <- 2
bet <- 2
alph <- 3


dens <- function(z,n) {partialsum = 0

for(k in 1:n){
partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) * bet^(alph*k)*z*(alph * k -1) * exp(-bet*z)/gamma(alph*k)}

return(partialsum)
}

zs1 = seq(0, 10, by = 0.0005)

plot(zs1,dens(zs1,200),"l")








gammadist <- function(u,i) {bet^(alph*i)*u*(alph * i -1) * exp(-bet*u)/gamma(alph*i)}


cdf <- function(z,n) {partialsum = exp(-lambda)
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

