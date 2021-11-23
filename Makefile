## This is mac-theobio/effects

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt notes.md structure.md"

setssh:
	git remote set-url origin git@github.com:mac-theobio/effects.git

######################################################################

today: understanding_mm_special.pdf mediate_bin_preds_adjust.Rout.pdf \
	binom_correlated_preds.Rout.pdf glme_random_intercept_preds.Rout.pdf

######################################################################

Makefile: varpred.stamp

varpred.stamp: DESCRIPTION
	$(touch)
	$(MAKE) install || ($(rm) && false)

######################################################################

Sources += $(wildcard *.R *.md *.Rnw *.rmd *.bib)
Sources += $(wildcard R/*.R)
Sources += $(wildcard man/*.Rd) NAMESPACE DESCRIPTION

%.tangle.r: %.Rnw
	R CMD Stangle $<

%.tex: %.Rnw
	R CMD Sweave $<

## Make Sweave weird depencies chain
%: %.pdf ;

Sources += notes.md glossary.md structure.md

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

## Variable prediction manuscript

Ignore += variable_predictions.tex
Ignore += variable_predictions*.pdf

variable_predictions_funs.Rout: variable_predictions_funs.R
	$(wrapR)

## Continuous predictors
variable_predictions_objs.Rout: variable_predictions_objs.R variable_predictions_funs.rda

## Categorical predictors
categorical_predictors.Rout: categorical_predictors.R variable_predictions_funs.rda

## Linear mixed effect
lme_random_intercept.Rout: lme_random_intercept.R variable_predictions_funs.rda

## Multiple outcomes
multiple_outcomes.Rout: multiple_outcomes.R variable_predictions_funs.rda
multiple_outcomes_model.Rout: multiple_outcomes_model.R multiple_outcomes.rda
multiple_outcomes_preds.Rout: multiple_outcomes_preds.R multiple_outcomes_model.rda

## Complex interaction
### Cubic polynomial
cubic_predictors.Rout: cubic_predictors.R variable_predictions_funs.rda
cubic_predictors_model.Rout: cubic_predictors_model.R cubic_predictors.rda
cubic_predictors_preds.Rout: cubic_predictors_preds.R cubic_predictors_model.rda variable_predictions_funs.rda
cubic_predictors_preds_adjust.Rout: cubic_predictors_preds_adjust.R cubic_predictors_model.rda variable_predictions_funs.rda

## Mediation and confounders
mediate.Rout: mediate.R variable_predictions_funs.rda
mediate_model.Rout: mediate_model.R mediate.rda
mediate_preds.Rout: mediate_preds.R mediate_model.rda
mediate_bin_preds.Rout: mediate_bin_preds.R mediate_model.rda
mediate_bin_preds_adjust.Rout: mediate_bin_preds_adjust.R mediate_model.rda
mediate_bin_bias.Rout: mediate_bin_bias.R mediate_model.rda

## Correlated predictor glm
binom_correlated.Rout: binom_correlated.R variable_predictions_funs.rda
binom_correlated_model.Rout: binom_correlated_model.R binom_correlated.rda
binom_correlated_preds.Rout: binom_correlated_preds.R binom_correlated_model.rda

## One predictor, single intercept glme
glme_random_intercept.Rout: glme_random_intercept.R variable_predictions_funs.rda
glme_random_intercept_model.Rout: glme_random_intercept_model.R glme_random_intercept.rda
glme_random_intercept_preds.Rout: glme_random_intercept_preds.R glme_random_intercept_model.rda

## variable_predictions.pdf: variable_predictions.Rnw
variable_predictions.tex: glme_random_intercept_preds.rda cubic_predictors_preds_adjust.rda \
	cubic_predictors_preds.rda variable_predictions_objs.rda variable_predictions.Rnw

######################################################################

bias_correction_methods.pdf: bias_correction_methods.rmd
	$(knitpdf)

######################################################################

## Understanding model matrix*
understanding_mm.pdf: variable_predictions_funs.rda understanding_mm.rmd
	$(knitpdf)
understanding_mm_special.pdf: variable_predictions_funs.rda understanding_mm_special.rmd
	$(knitpdf)

######################################################################

## Compare varpred with emmeans
compare_emmeans_varpred.pdf: compare_emmeans_varpred.rmd variable_predictions_objs.rda \
	categorical_predictors.rda
	$(knitpdf)

## This should work but it doesn't
bad_example.Rout: bad_example.R categorical_predictors.rda

## Short lab reports
short_lab_report.pdf: binom_correlated_preds.rda variable_predictions_objs.rda \
	short_lab_report.rmd
	$(knitpdf)


######################################################################

cp2comp_exam:
	cp variable_predictions-pred_age_cont_plots.pdf \
	variable_predictions-qoi_pred_plot.pdf \
	variable_predictions-pred_cubic_plots.pdf \
	variable_predictions-pred_cont_joint_plots.pdf ../comp_exam/

######################################################################

## Is the model center a thing??

center.Rout: center.R

######################################################################

Sources += *.rmd

prediction.Rout: prediction.R
	$(wrapR)

# bias_correction.bib: bias_correction.bib
bias_correction.pdf: bias_correction.rmd prediction.rda
	$(knitpdf)

taylor.Rout: taylor.R
delta.Rout: delta.R

## Second-order approximation
aprroximation_bias_correction.pdf: aprroximation_bias_correction.rmd
	$(knitpdf)

## BB notes
logist_normal_bias.html: logist_normal_bias.rmd
	$(knithtml)

## JD: How are these made?
Ignore += simple_pred_spec_plot1.pdf simple_pred_spec_plot2.pdf

######################################################################

## Package installation and checks
Ignore += vareffects_1*

build-package:
	R CMD build .

install:
	make update-doc && make build-package && make install-tarball

Ignore += vareffects_1*
install-tarball:
	R CMD INSTALL vareffects_1.0.1.*

check-package:
	echo "devtools::check('.')" | R --slave

update-doc:
	echo "devtools::document('.')" | R --slave

build-manual:
	echo "devtools::build_manual(pkg = '.')" | R --slave

## Note from Ben  devtools::build_manual() ?

######################################################################

alldirs += dev

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
