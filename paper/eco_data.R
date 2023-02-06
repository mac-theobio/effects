library(dplyr)
library(tibble)

library(shellpipes)

N <- 25 
seed <- 31
pot_mean <- 8
pot_spread <- 0.4
phos_mean <- 22
phos_spread <- 0.4
nitro_mean <- 33
nitro_spread <- 0.4
mass_mean <- 100
mass_spread <- 0.4

# nitrogen, phosphorous, kpotassium
beta_np <- 0.5
beta_pm <- 0.8
beta_nm <- 0.7
beta_km <- 0.2
beta_nk <- 0
beta_pk <- 0.4
beta_pkm <- 0.1

set.seed(seed)

## meanlog = log(0) works as desired in rlnorm!
dat <- (data.frame(nitro = rlnorm(N, meanlog=log(nitro_mean), sdlog=nitro_spread))
	%>% mutate(NULL
		, phos_pred = phos_mean+(nitro-nitro_mean)*beta_np
		, phos = rlnorm(N, meanlog=log(phos_pred), sdlog=phos_spread)
		, pot_pred = pot_mean
			+ (nitro-nitro_mean)*beta_nk
			+ (phos-phos_mean)*beta_pk
		, pot = rlnorm(N, meanlog=log(pot_pred), sdlog=pot_spread)
		, mass_pred=mass_mean
			+ (nitro-nitro_mean)*beta_nm
			+ (phos-phos_mean)*beta_pm
			+ (pot-pot_mean)*beta_km
			+ (pot-pot_mean)*(phos-phos_mean)*beta_pkm
		, mass = rlnorm(N, meanlog=log(mass_pred), sdlog=mass_spread)
	)
)

summary(dat)

rdsSave(dat)

