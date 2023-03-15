library(readr)
library(knitr)
library(tidyverse)
data <- read_csv("C:/Users/hoond/Downloads/HW7_data.csv")

x <- data[,"x"]
y <- data[,"y"]

ggplot(data, aes(x,y)) + geom_point()

theta = 0

l_theta = 0

for(i in  1:100){
  l_theta = l_theta + (y[i] -log(theta + x[i]))^2
}
l_theta = 1/2 * l_theta