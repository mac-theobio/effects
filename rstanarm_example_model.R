library(shellpipes)
library(rstanarm)

loadEnvironments()
startGraphics()
 
#### Model
rstan_mod <- stan_glmer(y ~ 1+x1+x2+x3+(1|hhid)
	, family=binomial(link="logit")
	, data=rstan_sim_df
	, chains=1
	, cores=2
)
rstan_mod

saveEnvironment()
