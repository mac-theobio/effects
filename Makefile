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

## Simulate data
sims_data.Rout: sims_data.R

## fit simple lm
mod_lm.Rout: mod_lm.R sims_data.rda

## Base R prediction


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
