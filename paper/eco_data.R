library(dplyr)
library(tibble)

library(shellpipes)

N <- 25 
seed <- 31
phos_mean <- 22
phos_spread <- 0.4
nitro_mean <- 33
nitro_spread <- 0.4
mass_mean <- 100
mass_spread <- 0.4
beta_np <- 0.8
beta_pm <- 0.8
beta_nm <- 0.4
beta_pnm <- 0.1

set.seed(seed)

## meanlog = log(0) works as desired in rlnorm!
dat <- (data.frame(nitro = rlnorm(N, meanlog=log(nitro_mean), sdlog=nitro_spread))
	%>% mutate(NULL
		, phos_pred = phos_mean+(nitro-nitro_mean)*beta_np
		, phos = rlnorm(N, meanlog=log(phos_pred), sdlog=phos_spread)
		, mass_pred=mass_mean
			+ (nitro-nitro_mean)*beta_nm
			+ (phos-phos_mean)*beta_pm
			+ (nitro-nitro_mean)*(phos-phos_mean)*beta_pnm
		, mass = rlnorm(N, meanlog=log(mass_pred), sdlog=mass_spread)
	)
)

rdsSave(dat)

