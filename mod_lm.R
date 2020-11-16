source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

## Fit lm model
lm_mod_unscaled <- lm(pred1 ~ x1 + x2, data = sim_df)
summary(lm_mod_unscaled)

## Fit unscaled lm model
lm_mod_scaled <- lm(pred1 ~ drop(scale(x1)) + drop(scale(x2)), data = sim_df)
summary(lm_mod_scaled)

saveVars(lm_mod_unscaled
	, lm_mod_unscaled
	, sim_df
)
