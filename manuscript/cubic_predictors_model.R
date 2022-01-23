library(shellpipes)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
## Joint model
############################################################################################

## Observed averages
### Outcomes
true_prop_df <- (sim_df_cubic
	%>% summarize_all(mean)
	%>% rename(fit=hhsize)
	%>% mutate(model="Observed mean")
)
print(true_prop_df)

## Models
mod_cubic <- lm(hhsize ~ poly(age, degree=3, raw=TRUE)+wealthindex
	, data=sim_df_cubic
)
summary(mod_cubic)

saveEnvironment()

