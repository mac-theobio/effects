## This is mac-theobio/effects

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt notes.md"

######################################################################

Sources += $(wildcard *.R *.md *.Rnw *.rmd)
Sources += $(wildcard R/*.R)
Sources += $(wildcard man/*.Rd) NAMESPACE DESCRIPTION

%.tangle.r: %.Rnw
	R CMD Stangle $<

%.tex: %.Rnw
	R CMD Sweave $<

## Make Sweave weird depencies chain
%: %.pdf ;

Sources += glossary.md

autopipeR = defined

######################################################################

## vareffects package
vareffects_pkg.Rout: R/vareffects_pkg.R
effectsfuns.Rout: R/effectsfuns.R
implemented.Rout: R/implemented.R
utilities.Rout: R/utilities.R
plotsfuns.Rout: R/plotsfuns.R
methodfuns.Rout: R/methodfuns.R
pkgsExport.Rout: R/pkgsExport.R

Sources += $(wildcard dev/*.R)

demonstrate.Rout: dev/demonstrate.R
	$(pipeR)

######################################################################

## Customize based R predict method to add CI and zero-out non-focal predictors
cpred_baseR.Rout: cpred_baseR.R

## Simulate data
sims_data.Rout: sims_data.R

## What happens to vcov when we scale?
## vcov changes as Ben predicted!
## (elements not involving intercept do not change when we translate)
scales.Rout: scales.R sims_data.rda

## fit simple lm
mod_lm.Rout: mod_lm.R sims_data.rda

## New data for prediction
pred_data.Rout: pred_data.R sims_data.rda

## Base R prediction
predict_baseR.Rout: predict_baseR.R cpred_baseR.rda mod_lm.rda pred_data.rda

## emmeans
predict_emmeans.Rout: predict_emmeans.R mod_lm.rda pred_data.rda

## varpred: JD's effect
predict_jdeffect.Rout: predict_jdeffect.R mod_lm.rda pred_data.rda

## Plot marginal predictions
predict_plots.Rout: predict_plots.R predict_baseR.rda predict_emmeans.rda predict_jdeffect.rda

## Model with interaction
inter_mod_lm.Rout: inter_mod_lm.R
inter_predict.Rout: inter_predict.R effectsfuns.R inter_mod_lm.rda

######################################################################

skinny_effects_plot.Rout: skinny_effects_plot.R

######################################################################

Ignore += effects_writeup.tex
Ignore += effects_writeup*.pdf
## effects_writeup.pdf: effects_writeup.Rnw
effects_writeup.tex: predict_plots.rda effects_writeup.Rnw

######################################################################

bias_correction.pdf: bias_correction.rmd
	$(knitpdf)

######################################################################

## Package installation and checks
Ignore += vareffects_1*

build-package:
	R CMD build .

install-package:
	R CMD INSTALL vareffects_1*

check-package:
	echo "devtools::check('.')" | R --slave

update-doc:
	echo "devtools::document('.')" | R --slave

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

-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk

-include rnw.mk
