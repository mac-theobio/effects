library(shellpipes)
library(glmmTMB)

loadEnvironments()
startGraphics()
 
#### Model
glme_mod <- glmmTMB(status~age+(1|hhid)
	, data=glme_sim_df
	, family = binomial()
)
summary(glme_mod)

saveEnvironment()
