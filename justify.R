library(shellpipes)
library(vareffects); varefftheme()
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
nHH_obs <- 100
perHH <- 1

############################################################################################
# Simulate data for justifying proposed approach
############################################################################################

#### Simulation

## No interaction
justify_sim_df <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, form=~1+x1+x2+x3
	, betas=c(5, -0.2, 1, 2)
	, pgausian=list(p=3)
	, pcat=list(p=0)
	, link_scale=TRUE
)$data
head(justify_sim_df)
true_prop_df <- (justify_sim_df
	%>% summarize_all(mean)
)
true_prop_df

## With interaction
justify_inter_sim_df <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, form=~1+x1+x2*x3
	, betas=c(5, -0.2, 1, 2, 5)
	, pgausian=list(p=3)
	, pcat=list(p=0)
	, link_scale=TRUE
)$data
head(justify_inter_sim_df)
true_prop_inter_df <- (justify_inter_sim_df
	%>% summarize_all(mean)
)
true_prop_inter_df

saveEnvironment()
