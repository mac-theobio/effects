library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
nHH_obs <- 5000
perHH <- 1

############################################################################################
# generalized linear model
############################################################################################

### No interaction model

#### Simulation
glm_sim_df <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, addnoiseGLM=FALSE
	, noiseSD=1 # ignored if addnoiseGLM=FALSE
	, form=~1+x1
	, betas = c(2, 1)
	, pgausian=list(p=1,fun=rnorm, mean=0, sd=1)
	, pcat=list(p=0)
	, link_scale=FALSE
	, vnames=c("age", "status")
)$data
head(glm_sim_df)
true_prop_df <- (glm_sim_df
	%>% summarize_all(mean)
)
true_prop_df

saveEnvironment()
