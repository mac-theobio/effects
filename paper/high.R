library(shellpipes)

N <- 250
seed <- 31

mass_mean <- 2
nitro_mean <- 1
nitro_spread <- 0.8
beta_pm <- 1.5
phos_spread <- 3
pot_spread <- 0.9

## Bicko only
nitro_max <- 4

## JD only
pot_mean <- 1
phos_mean <- 1
nitro_spread <- 0.4
mass_spread <- 0.4

# nitrogen, phosphorous, kpotassium
beta_np <- 0.5
beta_nm <- 0.5
beta_km <- 0.2
beta_nk <- 0
beta_pk <- 0.4
beta_pkm <- 0.1
beta_nnm <- -0.623

# Extra params for binomial outcome
binSize <- 20
beta_r <- 2
beta_mr <- 0.006

saveEnvironment()
