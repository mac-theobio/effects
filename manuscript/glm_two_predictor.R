library(shellpipes)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(991)

############################################################################################
# Global variables
nHH_obs <- 1e3
perHH <- 1

############################################################################################
# generalized linear model
############################################################################################

### No interaction model

#### Simulation
glm_sim <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, addnoiseGLM=FALSE
	, noiseSD=1 # ignored if addnoiseGLM=FALSE
	, form=~1+x1+x2
	, betas = c(2, 0.5, 1.5)
	, pgausian=list(p=2,fun=rnorm, mean=0, sd=1)
	, pcat=list(p=0)
	, link_scale=FALSE
	, vnames=c("age", "wealthindex", "status")
)
glm_sim_betas <- glm_sim$betas
print(glm_sim_betas)
glm_sim_df <- glm_sim$data
head(glm_sim_df)
true_prop_df <- (glm_sim_df
	%>% summarize_all(mean)
	%>% mutate(model="Observed mean")
)
true_prop_df

saveEnvironment()

