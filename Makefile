## This is mac-theobio/effects

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R)

automatic_makeR = defined

######################################################################

## effect functions: clean zero_cov and add extract_assign for variable-name-wise zero out
effectsfuns.Rout: effectsfuns.R

## Customize based R predict method to add CI and zero-out non-focal predictors
cpred_baseR.Rout: cpred_baseR.R

## Simulate data
sims_data.Rout: sims_data.R

## fit simple lm
mod_lm.Rout: mod_lm.R sims_data.rda

## New data for prediction
pred_data.Rout: pred_data.R sims_data.rda

## Base R prediction
predict_baseR.Rout: predict_baseR.R mod_lm.rda pred_data.rda

## emmeans
predict_emmeans.Rout: predict_emmeans.R effectsfuns.R mod_lm.rda pred_data.rda

## Plot marginal predictions
predict_plots.Rout: predict_plots.R predict_baseR.rda predict_emmeans.rda

######################################################################

skinny_effects_plot.Rout: skinny_effects_plot.R

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/makeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
