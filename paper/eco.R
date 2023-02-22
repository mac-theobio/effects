library(shellpipes)

N <- 25 
seed <- 31
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
beta_pm <- 0.8
beta_nm <- 0.9
beta_km <- 0.2
beta_nk <- 0
beta_pk <- 0.4
beta_pkm <- 0.1
beta_nnm <- -0.623

saveEnvironment()
