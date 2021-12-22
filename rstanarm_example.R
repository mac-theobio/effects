library(shellpipes)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
nHH_obs <- 100
perHH <- 10

############################################################################################
# generalized linear mixed effect model
############################################################################################

## Varying intercept: one grouping factor

### No interaction model

#### Simulation
rstan_sim_df <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, pgausian=list(p=2)
	, pcat=list(p=1,nlevels=4, replace=TRUE)
	, link_scale=FALSE
)$data
head(rstan_sim_df)
true_prop_df <- (rstan_sim_df
	%>% group_by(x3)
	%>% summarize_all(mean)
)
true_prop_df

saveEnvironment()
