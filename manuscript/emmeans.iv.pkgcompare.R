library(shellpipes)
library(dplyr)
library(emmeans)

loadEnvironments()

emmeans_iv <- (emmeans(fit, specs=~age
		, type="response"
		, at=list(age=focal)
		, weights="proportional"
		, nesting=NULL
	)
	|> data.frame()
	|> rename(
		fit=emmean
		, se=SE
		, lwr=lower.CL
		, upr=upper.CL
	)
	|> mutate(model="emmeans")
	|> select(-df)
)
head(emmeans_iv)

saveVars(emmeans_iv)
