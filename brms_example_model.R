library(shellpipes)
library(brms)

set.seed(911)

loadEnvironments()
startGraphics()
 
#### Model
brms_mod <- brm(y ~ 1+x1+x2+x3+(1|hhid)
	, family=binomial(link="logit")
	, data=rstan_sim_df
	, chains=1
	, cores=2
)
brms_mod

saveEnvironment()
