library(shellpipes)
library(lme4)

d <- rdsRead()

summary(d)

poism <- glmer(inc ~ group*wave + (1 | date) + offset(log(susc))
	, data = d, family = poisson(link = "log")
)

summary(poism)

saveVars(poism)

