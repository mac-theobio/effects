## vareffects manuscript

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R *.bib *.bst *.Rmd)

Sources += notes.md

######################################################################

autopipeR = defined

######################################################################

variable_predictions_funs.Rout: variable_predictions_funs.R
	$(wrapR)

## Example: varpred workflow + binned observations
### binfun: For binning from variable_predictions_funs.R 
workflow_example.Rout: workflow_example.R variable_predictions_funs.rda

## Function to compute 'true' predictions based on true betas and sim data
## Implemented in varpred to take in true.beta as arg
# truepredfun.Rout: truepredfun.R

## Figure 1 and Figure 2: Justification
### compare: emmeans, effects, varpred
justify.Rout: justify.R variable_predictions_funs.rda
justify_model.Rout: justify_model.R justify.rda

pipeRdesc += justify_preds
## justify_preds.isolate.pdf: justify_preds.R justify_model.rda
## justify_preds.inter.pdf: justify_preds.R justify_model.rda
justify_preds.Rout: justify_preds.R justify_model.rda

pipeRdesc += justify_anchors
# justify_anchors.combined.pdf: justify_anchors.R justify_model.rda
justify_anchors.Rout: justify_anchors.R justify_model.rda

## One plot for all the justify plots
pipeRdesc += justify_anchors_all
## justify_anchors_all.ggp.pdf: justify_anchors_all.R justify_preds.rda justify_anchors.rda
justify_anchors_all.Rout: justify_anchors_all.R justify_preds.rda justify_anchors.rda

## Figure 3: Quantity of interest
pipeRdesc += quantity_of_interest 
# quantity_of_interest.ggp.pdf: quantity_of_interest.R variable_predictions_funs.rda
quantity_of_interest.Rout: quantity_of_interest.R variable_predictions_funs.rda

## Figure 4: Cubic polynomials
cubic_predictors.Rout: cubic_predictors.R variable_predictions_funs.rda
cubic_predictors_model.Rout: cubic_predictors_model.R cubic_predictors.rda

pipeRdesc += cubic_predictors_preds
# cubic_predictors_preds.ggp.pdf: cubic_predictors_preds.R cubic_predictors_model.rda variable_predictions_funs.rda
cubic_predictors_preds.Rout: cubic_predictors_preds.R cubic_predictors_model.rda variable_predictions_funs.rda
pipeRdesc += cubic_varpred_margins_pred
# cubic_varpred_margins_pred.ggp.pdf: cubic_varpred_margins_pred.R cubic_predictors_model.rda variable_predictions_funs.rda
cubic_varpred_margins_pred.Rout: cubic_varpred_margins_pred.R cubic_predictors_model.rda variable_predictions_funs.rda

## Figure 5: Two predictors glm
glm_two_predictor.Rout: glm_two_predictor.R variable_predictions_funs.rda
glm_two_predictor_model.Rout: glm_two_predictor_model.R glm_two_predictor.rda

pipeRdesc += glm_two_predictor_preds
# glm_two_predictor_preds.ggp.pdf: glm_two_predictor_preds.R glm_two_predictor_model.rda
glm_two_predictor_preds.Rout: glm_two_predictor_preds.R glm_two_predictor_model.rda

### Add confidence intervals (predictions vs effects)
pipeRdesc += glm_two_predictor_preds_effects
# glm_two_predictor_preds_effects.ggp.pdf: glm_two_predictor_preds_effects.R glm_two_predictor_model.rda
glm_two_predictor_preds_effects.Rout: glm_two_predictor_preds_effects.R glm_two_predictor_model.rda


## Figure 6: Mediated effect
mediate.Rout: mediate.R variable_predictions_funs.rda
mediate_model.Rout: mediate_model.R mediate.rda
pipeRdesc += mediate_preds
# mediate_preds.ggp.pdf: mediate_preds.R mediate_model.rda
mediate_preds.Rout: mediate_preds.R mediate_model.rda

######################################################################

## Paper

## draft.paper.pdf: paper.body.tex draft.head.tex

Ignore += *.paper.tex *.paper.pdf

Sources += *.head.tex $(paperfiles)
paperfiles = mscommands.tex begindoc.tex abstract.tex paper.body.tex msbib.tex enddoc.tex

%.paper.tex: %.head.tex $(paperfiles)
	$(catro)

######################################################################

# Thesis

# DO NOT edit draft.tex, created automatically, instead edit body.tex
Ignore += draft.pdf

## Files to edit
# draft.tex.pdf: body.tex drafthead.tex mscommands.tex
# draft.pdf: body.tex drafthead.tex mscommands.tex
# tab.ms.pdf: body.tex tabhead.tex mscommands.tex
# plos.pdf: body.tex ploshead.tex mscommands.tex

Ignore += draft.tex
msfiles = mscommands.tex begindoc.tex abstract.tex body.tex msbib.tex enddoc.tex
draft.tex: drafthead.tex $(msfiles)
	$(catro)

Sources += *head.tex $(msfiles)

######################################################################

## Modular diff stuff

msoldfiles = mscommands.tex begindoc.tex abstract.tex body.tex.fb7889b9.oldfile msbib.tex enddoc.tex
## msoldfiles = body.tex.7e1cc3fe.oldfile

Ignore += draft.old.tex
draft.old.tex: drafthead.tex $(msoldfiles)
	$(catro)

## draft.diff.pdf: body.tex
Ignore += draft.diff.tex
draft.diff.tex: draft.old.tex draft.tex
	$(latexdiff)

######################################################################

## Product templating stuff not used for thesis

Ignore += plos.tex
plos.tex: ploshead.tex $(msfiles)
	$(catro)

Ignore += *.ms.tex
%.ms.tex: %head.tex $(msfiles)
	$(catro)

######################################################################

## Try to make readable for JD on tablet (never worked out, but not clear if it was a tech problem)

Ignore += tab.adj.pdf
tab.adj.pdf: tab.ms.pdf Makefile
	pdfjam -o $@ --scale 1.25 $<

######################################################################

## JD thoughts and spinning

## Testing
jonathan.Rout: jonathan.R mediate.rda
	$(pipeR)

## Notes
Ignore += jd_notes.html
jd_notes.html: jd_notes.Rmd
	$(knithtml)

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

## base.uni.fit.Rout: uni.R
## base.bi.fit.Rout: bi.R

## Anchor plot shows the curves, a center anchor, and a zero anchor
pipeRimplicit += anchorplot
## base.uni.anchorplot.Rout: anchorplot.R
## base.bi.anchorplot.Rout: anchorplot.R
%.anchorplot.Rout: %.fit.rda anchorplot.R
	$(pipeR)

## Test grouping

hhgroup.Rout: hhgroup.R

## Range of possibilities for a splash plot
## It's quite confusing: how do I get different ratios of intercept and slope variance? Is it even possible?
anchorRange.Rout: anchorRange.R

######################################################################

## Seminar presentation pix
## 2022 Mar 15 (Tue)

alcohol_data.Rout: alcohol_data.R

## First try (no branching)
alcohol_plots.Rout: alcohol_plots.R alcohol_data.rds

## Branch the model fits
## alcohol.eduQuad.eduFit.Rout: eduQuad.R
## alcohol.eduUni.eduFit.Rout: eduUni.R
## alcohol.eduUni.eduFit.rtmp: eduUni.R
scriptStep += eduFit
eduFit += eduUni eduFull eduQuad
eduFit_dep = %_data.rds

## pipeRdesc works with _plain_ stems
pipeRdesc += eduPlot
pipeRimplicit += eduPlot
## alcohol.eduUni.eduPlot.Rout: eduPlot.R
## alcohol.eduQuad.eduPlot.Rout: eduPlot.R
## alcohol.eduFull.eduPlot.Rout: eduFull.R
%.eduPlot.Rout: eduPlot.R %.eduFit.rda
	$(pipeR)

## alcohol.eduQuad.eduPlot.pred.pdf:

pipeRdesc += eduzPlot
pipeRimplicit += eduzPlot
## zeroPlots are fragile so do them separately
## alcohol.eduUni.eduzPlot.Rout: eduzPlot.R
## alcohol.eduQuad.eduzPlot.Rout: eduzPlot.R
%.eduzPlot.Rout: eduzPlot.R %.eduPlot.rda
	$(pipeR)

pipeRdesc += glm_talk
glm_calcs.Rout: glm_calcs.R glm_two_predictor_model.rda
glm_talk.Rout: glm_talk.R glm_calcs.rda

######################################################################

## Algebra
Ignore += algebra.html
algebra.html: algebra.Rmd
	$(knithtml)

## Higher order interactions (cubic polynomial)
cubic_sim.Rout: cubic_sim.R
cubic_model.Rout: cubic_model.R cubic_sim.rda
cubic_preds.Rout: cubic_preds.R cubic_model.rda variable_predictions_funs.rda


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

-include makestuff/chains.mk
-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
