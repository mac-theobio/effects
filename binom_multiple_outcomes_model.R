library(shellpipes)
library(dplyr)
library(glmmTMB)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
## Joint model
############################################################################################

## Observed averages
### Outcomes
true_prop_df <- (sim_df_cont_joint_long
	%>% group_by(services)
	%>% summarize(fit=mean(values))
)
print(true_prop_df)

## Models
mod_cont_joint <- glmmTMB(values~-1+services+services:(age+wealthindex)+(services-1|hhid)
	, data=sim_df_cont_joint_long
	, family=binomial()
)
summary(mod_cont_joint)

saveEnvironment()
