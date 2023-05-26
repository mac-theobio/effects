
library(shellpipes)
library(dplyr)

loadEnvironments()

set.seed(991)
## Simulate data
dat <- (data.frame(nitro=rlnorm(N,meanlog=log(nitro_mean), sdlog=nitro_spread))
	%>% mutate(nitro=pmin(nitro, nitro_max)
		, nitro = nitro - min(nitro)
		, phos=rnorm(N, mean=1, sd=phos_spread)
		, pot = rnorm(N)
		, eta=mass_mean + beta_nm*nitro + beta_pm*phos + pot_spread*pot
		, robust=rbinom(N, binSize, plogis(eta))
		, robustProp=robust/binSize
		, numTest=binSize
	)
)

saveVars(dat)

