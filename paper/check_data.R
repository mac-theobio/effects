library(shellpipes)
library(dplyr)
library(tibble)

loadEnvironments()

set.seed(991)

# N <- 100
# b0 <- 1
# bn <- 0.1
# bp <- 0.8
# bk <- 0.7
# 
# ## Mean and spread
# nitro_mean <- 2
# nitro_spread <- 5
# phos_mean <- 3
# phos_spread <- 3
# pot_mean <- 4
# pot_spread <- 2
# 
# # logit(y) ~ b0 + bn * nitro + bp * phos + bk * pot
# 
# dat <- (data.frame(nitro = rlnorm(N, meanlog=log(nitro_mean), sdlog=log(nitro_spread)))
# 	%>% mutate(NULL
# 		, pot = rlnorm(N, meanlog=log(pot_mean), sdlog=log(pot_spread))
# 		, phos = rlnorm(N, meanlog=log(phos_mean), sdlog=log(phos_spread))
# 		, pred = b0 + bn*(nitro - nitro_mean) + bp*(phos - phos_mean) + bk*(pot - pot_mean)
# 		, mass = rbinom(N, size=10, plogis(pred))
# 	)
# 	%>% data.frame()
# )
# 
# 
# ## Fit model
# table(dat$mass)
# 
# mod <- glm(mass ~ nitro + pot + phos, dat, family="binomial")
# summary(mod)
# 
# saveVars(dat, mod)
# 
# 
# library(shellpipes)

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
beta_pkm <- 2
beta_nnm <- -0.625
beta_nnm <- 0

# Extra params for binomial outcome
binSize <- 100
beta_r <- 4
beta_mr <- 0.004



## Take the log of values that are large compared to scale; smoothly return a finite value for negative numbers
safelog <- function (x, scale=1){
	return(log(scale*log(1+exp(x/scale))))
}

## meanlog = log(0) works as desired in rlnorm!
dat <- (data.frame(nitro = rlnorm(N, meanlog=log(nitro_mean), sdlog=nitro_spread))
	%>% mutate(NULL
		, phos_pred = phos_mean+(nitro-nitro_mean)*beta_np
		, phos = rlnorm(N, meanlog=log(phos_pred), sdlog=phos_spread)
		, pot_pred = pot_mean
			+ (nitro-nitro_mean)*beta_nk
			+ (phos-phos_mean)*beta_pk
		, pot = rlnorm(N, meanlog=safelog(pot_pred), sdlog=pot_spread)
		, mass_pred=mass_mean
			+ (nitro-nitro_mean)*beta_nm
			+ (nitro-nitro_mean)^2*beta_nnm
			+ (phos-phos_mean)*beta_pm
			+ (pot-pot_mean)*beta_km
			+ (pot-pot_mean)*(phos-phos_mean)*beta_pkm
		, mass = rlnorm(N, meanlog=safelog(mass_pred), sdlog=mass_spread)
		, robust = rbinom(N
			, size=binSize
			, prob=plogis(beta_r + beta_mr*(mass_pred-mass_mean))
		)
		, numTest=binSize
		, robustProp = robust/binSize
		, robust2 = rbinom(N
			, size=1
			, prob=plogis(beta_r + beta_mr*(mass_pred-mass_mean))
		)
	)
)

dat



