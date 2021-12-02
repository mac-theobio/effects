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
nHH_obs <- 500
perHH <- 10

############################################################################################
# generalized linear mixed effect model
############################################################################################

## Varying intercept: one grouping factor

### No interaction model

#### Simulation
glme_sim_df <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, form=~1+x1
	, betas = c(5, 0.8)
	, hhSD = 5
	, pgausian=list(p=1,fun=rnorm, mean=0, sd=3)
	, pcat=list(p=0)
	, link_scale=FALSE
	, vnames=c("age", "status")
)$data
head(glme_sim_df)
true_prop_df <- (glme_sim_df
	%>% summarize_all(mean)
)
true_prop_df

saveEnvironment()
