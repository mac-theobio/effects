library(dplyr)
library(tibble)

library(shellpipes)

loadEnvironments()

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
			+ (nitro-nitro_mean)^2*beta_nnm
			+ (phos-phos_mean)*beta_pm
			+ (pot-pot_mean)*beta_km
			+ (pot-pot_mean)*(phos-phos_mean)*beta_pkm
		, mass = rlnorm(N, meanlog=log(mass_pred), sdlog=mass_spread)
		, robust = rbinom(N
			, size=binSize
			, prob=plogis(beta_r + beta_mr*(mass_pred-mass_mean))
		)
		, numTest=binSize
		, robustProp = robust/binSize
	)
)

summary(dat)

rdsSave(dat)

