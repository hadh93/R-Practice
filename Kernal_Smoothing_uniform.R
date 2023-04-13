library(readr)
library(knitr)
library(tidyverse)

data <- read_csv("C:/Users/hoond/Downloads/HW10_data.csv")

x = data$x
y = data$y

h <- 0.5
n <- 100

w <- function(i,h,x_vector, x_value) {
  ret = 0
  numerator = ifelse(abs(x_vector[i]-x_value) < h, 1, 0)
  denominator = 0
  for (ii in 1:n) {
    denominator = denominator + ifelse(abs(x_vector[ii]-x_value) < h, 1, 0)
  }
  numerator/denominator
}

m_hat <- function(x_value){
  numerator = 0
  denominator = 0
  for (i in 1:n){
    numerator = numerator + y[i]*w(i,h,x,x_value)
    denominator = denominator + w(i,h,x,x_value)
  }
  numerator/ denominator
}

m_hat_arr = rep(0,n)
for (i in 1:n){
  m_hat_arr[i] = m_hat_arr[i]+ m_hat(x[i])
}

# 1-(1)

print(m_hat(0))

plot(x, y, pch = 16, col = "blue", xlab = "x", ylab = "y")
par(new=TRUE)
plot(x, m_hat_arr, col = "red", type = "p")

# 1-(2)

bias_numer = 0
bias_denom = 0

for (i in 1:n){
  bias_numer = bias_numer + ifelse(abs(0-x[i]) < h, x[i]-0, 0)
  bias_denom = bias_denom + ifelse(abs(0-x[i]) < h, 1, 0)
}
bias = bias_numer/bias_denom

print(bias)

# 1-(3)

var_0 = 0

for (i in 1:n){
  var_0 = var_0 + w(i,h,x,0)^2
}

print(var_0)


# 1-(4)
var_estimate = 0
for (i in 1:n){
  var_estimate = var_estimate + (y[i]-m_hat(x[i]))^2
}
var_estimate = var_estimate/(n-1)
print(var_estimate)

# 1-(5)

print(m_hat(0) + 1.96*sqrt(var_estimate*var_0))
print(m_hat(0) - 1.96*sqrt(var_estimate*var_0))


