library(shellpipes)
library(tidyr)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
nHH_obs <- 50
perHH <- 10

############################################################################################
# Linear mixed effect model
############################################################################################

## Varying intercept: one grouping factor

### Two outcomes
### No interaction model
### Two predictors plus one latent

#### Simulation
sim_df_cont_joint <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, form=~1+x1+x2+x3
	, betas = c(1.5, 0.1, 2, 1)
	, hhSD = c(2, 3)
	, pgausian=list(p=3,fun=rnorm, mean=0, sd=1)
	, pcat=list(p=0)
	, noutcomes=2
	, separatelatent=TRUE
	, link_scale=TRUE
	, blatent=c(1, -5)
	, vnames=c("age", "wealthindex", "latent", "hhsize", "rent")
)$data
head(sim_df_cont_joint)


############################################################################################
# Long format for joint model
############################################################################################

sim_df_cont_joint_long <- (sim_df_cont_joint
	%>% mutate(iid=1:n())
	%>% gather(services, values, c("hhsize", "rent"))
	%>% data.frame()
)
head(sim_df_cont_joint_long)

saveVars(sim_df_cont_joint_long, comparevarpred)
