library(shellpipes)

## Parameters
N <- 20
beta0 <- 5
## Age
betaA <- 0.1
mean_age <- 25
age_max <- 50
age_min <- 18
age_sd <- 1
## Wealth
betaW <- 1
## Income
betaI <- 2
mean_income <- 2
income_sd <- 0.8
income_max <- 4

## Interaction
betaIW <- 5

## HHsize
hhsize_sd <- 1

saveEnvironment()
