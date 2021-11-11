library(shellpipes)
library(glmmTMB)

commandEnvironments()
makeGraphics()
 
#### Model
glme_mod <- glmmTMB(status~age+(1|hhid)
	, data=glme_sim_df
	, family = binomial()
)
summary(glme_mod)

saveEnvironment()
