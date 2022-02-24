library(shellpipes)
library(dplyr)

N <- 1e2
xbar = 2
ybar = 0
rho = 0.8

## x, y and z
b0 <- 5
bx <- -0.4
by <- 1.5
bw <- 1

seed <- 992

saveEnvironment()
