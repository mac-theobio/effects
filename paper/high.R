library(shellpipes)

N <- 500
seed <- 30
pot_mean <- 8
pot_spread <- 0.4
phos_mean <- 22
phos_spread <- 0.4
nitro_mean <- 33
nitro_spread <- 0.4
mass_mean <- 500
mass_spread <- 0.4

# nitrogen, phosphorous, kpotassium
beta_np <- 0.5
beta_pm <- 1.5
beta_nm <- 0.9
beta_km <- 0.2
beta_nk <- 0
beta_pk <- 0.4
beta_pkm <- 2
beta_nnm <- -0.625
beta_nnm <- 0

# Extra params for binomial outcome
binSize <- 1
beta_r <- 1.4
beta_mr <- 2

saveEnvironment()
