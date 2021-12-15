library(shellpipes)
library(glmmTMB)

loadEnvironments()
startGraphics()
 
#### Model
glm_mod <- glm(status~age
	, data=glm_sim_df
	, family = binomial
)
summary(glm_mod)

saveEnvironment()
