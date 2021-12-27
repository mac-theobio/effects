library(shellpipes)
library(glmmTMB)

loadEnvironments()
startGraphics()
 
#### Model
glm_mod <- glm(status~age#+x2
	, data=glm_sim_df
	, family = binomial
)
summary(glm_mod)

saveEnvironment()
