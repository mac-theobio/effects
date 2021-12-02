source("makestuff/makeRfuns.R")
loadEnvironments()
sourceFiles()

## Fit lm model
mod_unscaled <- lm(y1 ~ x1 + x2, data = sim_df)
summary(mod_unscaled)

## Fit unscaled lm model
mod_scaled <- lm(y1 ~ x1std + x2std, data = sim_df)
summary(mod_scaled)

saveVars(mod_unscaled
	, mod_scaled
	, sim_df
)
