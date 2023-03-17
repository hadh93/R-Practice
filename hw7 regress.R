library(readr)
library(knitr)
library(tidyverse)
data <- read_csv("C:/Users/hoond/Downloads/HW7_data.csv")

x <- data$x
y <- data$y



theta = 1


for (j in 1:10){
  l=0
  ll = 0
  lll = 0
  
  for(i in  1:100){
    l = l + 1/2*(y[i]-exp(theta * x[i]))^2
    ll = ll + (y[i]-exp(theta * x[i])) * x[i] * exp(theta * x[i])
    lll = lll - x[i]^2 * exp(theta*x[i])^2
  }
  
  theta = theta - (ll/lll) # update theta for next iteration
  print(theta)
}

ggplot(data, aes(x,y)) + geom_point()+
stat_function(fun = function(x) exp(theta*x))+
ylim(-1, 12)

var = sum(data$y-exp())


theta_star <- c()
B = 1000
for (i in 1:B){
  
}




