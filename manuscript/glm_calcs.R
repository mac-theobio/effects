library(shellpipes)
library(vareffects)
library(dplyr)

loadEnvironments()

### Not corrected
pred_age_none <- as.data.frame(varpred(glm_mod
	, "age"
	, bias.adjust="none"
	, modelname="Mean-based"
))

summary(pred_age_none)

### Bias corrected
pred_age_pop <- as.data.frame(varpred(glm_mod
	, "age"
	, bias.adjust="population"
	, modelname="Population-based"
))

### Binned obs
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)

saveEnvironment()
