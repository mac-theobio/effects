library(shellpipes)
library(dplyr)
library(effects)

loadEnvironments()

effects_iv <- (Effect("age", fit
		, type="response"
		, xlevels=list(age=unique(focal))
	)
	|> data.frame()
	|> rename(
		, lwr=lower
		, upr=upper
	)
	|> mutate(model="Effect")
)
head(effects_iv)

saveVars(effects_iv)
