library(shellpipes)

## Parameters
N <- 20
beta0 <- 10
## Age
betaA <- 0.1
mean_age <- 3
age_max <- 5
age_sd <- 0.5
## Wealth
betaW <- 0.8
## Income
betaI <- 1.5
mean_income <- 2
income_sd <- 0.8
income_max <- 4

## Interaction
betaIW <- 0.2

## HHsize
hhsize_sd <- 1

saveEnvironment()
