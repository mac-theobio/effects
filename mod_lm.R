source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

## Fit lm model
mod_unscaled <- lm(pred1 ~ x1 + x2, data = sim_df)
summary(mod_unscaled)

## Fit unscaled lm model
mod_scaled <- lm(pred1 ~ x1s + x2s, data = sim_df)
summary(mod_scaled)

saveVars(mod_unscaled
	, mod_scaled
	, sim_df
)
