## vareffects manuscript

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R *.bib *.tex *.bst *.Rmd)

## Make Sweave weird depencies chain
## %: %.pdf ;

######################################################################

autopipeR = defined

######################################################################

variable_predictions_funs.Rout: variable_predictions_funs.R
	$(wrapR)

## Figure 1 and Figure 2: Justification
### compare: emmeans, effects, varpred
justify.Rout: justify.R variable_predictions_funs.rda
justify_model.Rout: justify_model.R justify.rda
justify_preds.Rout: justify_preds.R justify_model.rda

## Figure 3: Quantity of interest
quantity_of_interest.Rout: quantity_of_interest.R variable_predictions_funs.rda

## Figure 4: Cubic polynomials
cubic_predictors.Rout: cubic_predictors.R variable_predictions_funs.rda
cubic_predictors_model.Rout: cubic_predictors_model.R cubic_predictors.rda
cubic_predictors_preds.Rout: cubic_predictors_preds.R cubic_predictors_model.rda variable_predictions_funs.rda

## Figure 5: Two predictors glm
glm_two_predictor.Rout: glm_two_predictor.R variable_predictions_funs.rda
glm_two_predictor_model.Rout: glm_two_predictor_model.R glm_two_predictor.rda
glm_two_predictor_preds.Rout: glm_two_predictor_preds.R glm_two_predictor_model.rda

## Figure 6: Mediated effect
mediate.Rout: mediate.R variable_predictions_funs.rda
mediate_model.Rout: mediate_model.R mediate.rda
mediate_preds.Rout: mediate_preds.R mediate_model.rda

######################################################################

Ignore += *.pdf

# vareffects_plosone.pdf: vareffects_plosone.tex
vareffects_plosone.tex: mediate_preds.Rout glm_two_predictor_preds.Rout cubic_predictors_preds.Rout quantity_of_interest.Rout justify_preds.Rout vareffects_plosone.bib

######################################################################

## JD thoughts and spinning

## Testing
jonathan.Rout: jonathan.R mediate.rda
	$(pipeR)

## Notes
jd_notes.html: jd_notes.Rmd

## Chained workflow

## Parameters
pipeRimplicit += pars
%.pars.Rout: %.R
	$(pipeR)

## x and y are standard-width normals with an exactly fixed correlation ρ
## x is a focal predictor and y a mediator
## w is a standard-width normal that is exactly uncorrelated with x
## meant as a distractor
pipeRimplicit += sim
## base.sim.Rout: sim.R base.R
%.sim.Rout: sim.R %.pars.rda
	$(pipeR)

## Which model to fit?
scriptStep += fit
fit += uni bi
fit_dep = %.sim.rda

## mediator.uni.fit.Rout: uni.R
## mediator.bi.fit.Rout: bi.R

## Anchor plot shows the curves, a center anchor, and a zero anchor
pipeRimplicit += anchorplot
## mediator.uni.anchorplot.Rout: anchorplot.R
## mediator.bi.anchorplot.Rout: anchorplot.R
%.anchorplot.Rout: %.fit.rda anchorplot.R
	$(pipeR)

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

makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/texi.mk
-include makestuff/pipeR.mk
-include makestuff/chains.mk

-include makestuff/git.mk
-include makestuff/visual.mk
