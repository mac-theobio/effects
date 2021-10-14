library(shellpipes)
library(dplyr)
library(splines)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

############################################################################################
## Joint model
############################################################################################

## Observed averages
### Outcomes
true_prop_df <- (sim_df_cubic
	%>% summarize_all(mean)
)
print(true_prop_df)

## Models
mod_cubic <- lm(y ~ poly(x1, degree=3, raw=TRUE)+x2
	, data=sim_df_cubic
)
summary(mod_cubic)

saveEnvironment()
