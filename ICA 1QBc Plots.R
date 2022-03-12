lambda <- 2
gamm <- 3
delt <- 3

bet_tilde <- 1

gamm_tilde_half = function(n) {n * abs(gamm)^0.5}

delt_tilde = function(n) {n*delt + tan(pi/4) * (n^2 *abs(gamm) - n*gamm)}


dens <- function(z,n) {partialsum = 0
  for(k in 1:n){
    if(delt_tilde(k)<z){
      partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) *  gamm_tilde_half(k)/(2*pi) * (z-delt_tilde(k))^(-1.5) * exp(-gamm_tilde_half(k)/(2*(z-delt_tilde(k))))}
}
  return(partialsum)
}

zs1 = seq(0, 50, by = 0.01)
dens_vals_list <- c(length(zs1))

for(i in 1:length(zs1)){
  dens_vals_list[i] <- dens(zs1[i],20)}

plot(zs1,dens_vals_list,"l")

#integrate(dens,0,1000,n=50)

cdf <- function(z,n) {partialsum = exp(-lambda) 
  for(k in 1:n){
    if(delt_tilde(k)<z){
      partialsum = partialsum + (lambda^k * exp(-lambda)/factorial(k)) *  erfc(gamm_tilde_half(k)/sqrt((2*(z-delt_tilde(k)))))
  }
  return(partialsum)
  }
  }

zs2 = seq(0, 500, by = 0.1)
cdf_vals_list <- c(1:5001)
length(zs2)
length(cdf_vals_list)


for(i in 1:length(zs2)){
  cdf_vals_list[i] <- cdf(zs1[i],100)}

length(zs2)
length(cdf_vals_list)
plot(zs2,cdf_vals_list,"l")





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
  cdf_vals_list[i] <- cdf(zs2[i],30)}


plot(zs2,cdf_vals_list,"l")
