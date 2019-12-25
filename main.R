
#Mean and Variance of Gamma Distribution
mean_gamma = function(k,λ){
  af = function(x){(x)*((x^(k-1))*((exp(1))^((-x*λ))))/((factorial(k-1))*(1/λ)^k)}
  mean<- (integrate(f = af, upper = Inf, lower = 0))$value
  return(mean)
}
print(mean_gamma(4,0.5)) #Test


var_gamma = function(k,λ){
  af = function(x){(x^2)*((x^(k-1))*((exp(1))^((-x*λ))))/((factorial(k-1))*(1/λ)^k)}
  var<- (integrate(f = af, upper = Inf, lower = 0))$value - (mean_gamma(k,λ))^2
  return(var)
}
print(var_gamma(5,0.5)) #Test

#Mean and Variance of Normal Distribution
mean_normal = function(μ,σ){
  af = function(x){(x)*((exp(1))^((-1/2)*((x-μ)/σ)^2))/(σ*sqrt(2*pi))}
  mean<- (integrate(f = af, upper = Inf, lower = 0))$value
  return(mean)
}
print(mean_normal(98,10)) #takes mean and standard deviation as a parameter

var_normal = function(μ,σ){
  af = function(x){(x^2)*((exp(1))^((-1/2)*((x-μ)/σ)^2))/(σ*sqrt(2*pi))}
  var<- (integrate(f = af, upper = Inf, lower = 0))$value- (mean_normal(μ,σ))^2
  return(var)
}
print(var_normal(98,10)) #Test

#Mean and Variance of Chi-Squared Distribution
mean_chi = function(k){
  af = function(x){(x)*(((x^((k/2)-1))*((exp(1))^((-x)/2)))/((2^(k/2))*factorial(k/2-1)))}
  mean<- (integrate(f = af, upper = Inf, lower = 0))$value
  return(mean)
}
print(mean_chi(33)) #Test

var_chi = function(k){
  af = function(x){(x^2)*(((x^((k/2)-1))*((exp(1))^((-x)/2)))/((2^(k/2))*factorial(k/2-1)))}
  var<- (integrate(f = af, upper = Inf, lower = 0))$value - (mean_chi(k))^2
  return(var)
}
print(var_chi(33)) #Test

#Mean and Variance of Exponential Distribution
mean_exponential = function(λ){
  af = function(x){(x)*(λ*(exp(1))^(-λ*x))}
  mean<- (integrate(f = af, upper = Inf, lower = 0))$value
  return(mean)
}
print(mean_exponential(40)) #Test

var_exponential = function(λ){
  af = function(x){(x^2)*(λ*(exp(1))^(-λ*x))}
  var<- (integrate(f = af, upper = Inf, lower = 0))$value - (mean_exponential(λ))^2
  return(var)
}
print(var_exponential(40)) #Test 
