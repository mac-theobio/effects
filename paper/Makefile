## vareffects follow-on paper
## effects/paper

current: target
-include target.mk

all: draft.paper.pdf

vim_session:
	bash -cl "vmt paper.tex"

######################################################################

Sources += $(wildcard *.R *.bib *.bst *.Rmd *.rmd)

######################################################################

varpred.quick:
	cd ../varpred && make pull && make quickinstall

varpred.install:
	cd ../varpred && make pull && make install

vv.Rout: vv.R

######################################################################

## New notes 2023 Apr 19 (Wed)

Sources += notes.md

## New paper version

## draft.paper.pdf: paper.tex draft.head.tex

Ignore += *.paper.tex *.pdf

paperfiles = mscommands.tex begindoc.tex title.tex paper.tex msbib.tex enddoc.tex
Sources += *.head.tex $(paperfiles)

## draft.paper.tex: 
%.paper.tex: %.head.tex $(paperfiles)
	$(catro)

######################################################################

autopipeR = defined

######################################################################

## focal_check.pdf: focal_check.rmd

######################################################################

smoothfuns.Rout: smoothfuns.R

## Rename variables and see how the machinery works
## eco.weird.Rout: weird.R
%.weird.Rout: weird.R %.fake.rds
	$(pipeR)

Sources += $(wildcard *.md)

## eco.params.Rout: eco.R
## high.params.Rout: high.R
%.params.Rout: %.R
	$(pipeR)

## Fake data about nutrients and biomass ## data.md
## deprecate .data. for .fake. 2023 May 25 (Thu)
%.data.Rout: data.R %.params.rda
	$(pipeR)
## eco.jd.data.Rout: data.R eco.R
## high.data.Rout: data.R

## Two fake data branches ## fake.md ## IN Progress on bus May 25 (Thu)
scriptStep += fake
fake += bc jd
fake_dep = %.params.rda smoothfuns.rda
## eco.jd.fake.Rout: jd.R
## straight.bc.fake.Rout: bc.R straight.R
## straight.jd.fake.Rout: jd.R straight.R

## Plot only
scriptStep += scatter
scatter += robust
scatter_dep = %.fake.rds
## eco.jd.robust.scatter.Rout: robust.R

## Fit different models ## mfit.md
scriptStep += mfit
mfit += uniN quadN inter multi multiPK monster
mfit_dep = %.fake.rds
## eco.jd.uniN.mfit.Rout: uniN.R
## high.multi.mfit.Rout: multi.R
## eco.jd.inter.mfit.Rout: inter.R
## eco.jd.quadN.mfit.Rout: quadN.R
## high.quadN.mfit.Rout: quadN.R

## fit glm models glmfit.md
scriptStep += glmfit
glmfit += uniglmN multiglm interglm
glmfit_dep = %.fake.rds
## eco.jd.uniglmN.glmfit.Rout: uniglmN.R

## make different prediction objects ## vp.md
scriptStep += vp
vp += defAnchor anchor20 meanAnchor check
vp_dep += %.mfit.rda
## eco.jd.inter.defAnchor.vp.Rout: defAnchor.R
## eco.jd.inter.meanAnchor.vp.Rout: meanAnchor.R
# Temporary checks; Giving up for now!
## eco.jd.monster.check.vp.Rout: check.R

## make glm prediction objects ## gvp.md
## TODO: move defRef and defAnchor to def, if they can both be the same!
scriptStep += gvp
gvp += defRef obsRef
gvp_dep += %.glmfit.rda
## eco.jd.uniglmN.defRef.gvp.Rout: defRef.R
## eco.jd.multiglm.obsRef.gvp.Rout: obsRef.R

## Some sort of check (compare base predict to varpred)
## eco.jd.quadN.predict.Rout: predict.R eco.jd.quadN.mfit.rda
## high.quadN.predict.Rout: predict.R eco.jd.quadN.mfit.rda
%.predict.Rout: predict.R %.mfit.rda
	$(pipeR)

## Model plots plot.md
scriptStep += plot
plot += nitro centers
plot_dep += %.vp.rda

## eco.jd.uniN.defAnchor.nitro.plot.Rout: nitro.R
## eco.jd.quadN.defAnchor.nitro.plot.Rout: nitro.R
## eco.jd.quadN.anchor20.nitro.plot.Rout: nitro.R
## high.uniN.defAnchor.nitro.plot.Rout: nitro.R
## high.inter.defAnchor.nitro.plot.Rout: nitro.R
## eco.jd.inter.defAnchor.nitro.plot.Rout: nitro.R
## eco.jd.inter.defAnchor.centers.plot.Rout: centers.R

## TODO: When do we need a different track for glm at all?
## Can we save some info about the outcome variable?
## glm plots glmPlot.md
scriptStep += glmPlot
glmPlot += rnitro
glmPlot_dep += %.gvp.rda
## high.uniglmN.defRef.rnitro.glmPlot.Rout: rnitro.R
## high.uniglmN.obsRef.rnitro.glmPlot.Rout: rnitro.R
## high.multiglm.defRef.rnitro.glmPlot.Rout: rnitro.R high.R
## high.multiglm.obsRef.rnitro.glmPlot.Rout: rnitro.R high.R
## FIXME: The difference between obsRef and defRef is always small;
## hard to find a good example where bias-correct is correcting the bias
## eco.jd.interglm.defRef.rnitro.glmPlot.Rout: rnitro.R
## eco.jd.interglm.obsRef.rnitro.glmPlot.Rout: rnitro.R

## straight.jd.interglm.doNitro.pdf: rnitro.R
%.doNitro.pdf: %.defRef.rnitro.glmPlot.Rout.pdf %.obsRef.rnitro.glmPlot.Rout.pdf
	pdfjam -o $@ $(word 1, $^) 3 $(word 2, $^) 3 

## 2023 May 04 (Thu) Debugged reference-point inconsistency in uni
## eco.jd.refComp.Rout: refComp.R
## this compares two objects that are apparently the same.
%.refComp.Rout: refComp.R %.uniglmN.defRef.gvp.rda %.uniglmN.obsRef.gvp.rda
	$(pipeR)

######################################################################
## Composites

## Splash plot combines defAnchor and anchor20
## eco.jd.uniN.defAnchor.nitro.plot.Rout.pdf eco.jd.uniN.anchor20.nitro.plot.Rout.pdf
eco.jd.splash.Rout: splash.R eco.jd.uniN.defAnchor.nitro.plot.rda eco.jd.uniN.anchor20.nitro.plot.rda
	$(pipeR)

## Anchor check: does meanAnchor produce the same splash as defAnchor
## … in simple case
eco.jd.splashCheck.Rout: splashCheck.R eco.jd.uniN.defAnchor.nitro.plot.rda eco.jd.uniN.meanAnchor.nitro.plot.rda
	$(pipeR)

## Univariate/multi-variate
## eco.jd.uniMulti.Rout: uniMulti.R eco_data.R
uniMulti: eco.jd.uniN.defAnchor.nitro.plot.Rout.pdf eco.jd.multi.defAnchor.nitro.plot.Rout.pdf 
eco.jd.uniMulti.Rout: uniMulti.R eco.jd.uniN.defAnchor.nitro.plot.rda eco.jd.multi.defAnchor.nitro.plot.rda 
	$(pipeR)

## Compare quad plots with mean vs. default anchor (read and arrange only)
eco.jd.quadComp.Rout: quadComp.R eco.jd.quadN.defAnchor.nitro.plot.rda eco.jd.quadN.meanAnchor.nitro.plot.rda
	$(pipeR)

######################################################################

## Bicko debugging stuff

## CHECK: 2023 May 23 (Tue)
check_data.Rout: check_data.R
check_defRef.Rout: check_defRef.R check_data.rda 
check_obsRef.Rout: check_obsRef.R check_data.rda 

## Observed value approach (interactions on focal predictors)
observed_approach.Rout: observed_approach.R eco.interglm.glmfit.rda

## alternative pipeline

check_%.glmfit.Rout: altcheck.R check_%.rda
	$(pipeR)

check_data.glmfit.Rout: altcheck.R
check_data.defRef.rnitro.glmPlot.Rout.pdf:
check_data.obsRef.rnitro.glmPlot.Rout.pdf:

obsComp.pdf: check_defRef.Rout.pdf check_obsRef.Rout.pdf
	pdfjam -o $@ $(word 1, $^) 3 $(word 2, $^) 3 

## check_data.rnComp.pdf: altcheck.R check_data.R
%.rnComp.pdf: %.defRef.rnitro.glmPlot.Rout.pdf %.obsRef.rnitro.glmPlot.Rout.pdf
	pdfjam -o $@ $(word 1, $^) 3 $(word 2, $^) 3 

## high.multiglm.rnComp.pdf: high.R

######################################################################

## Focal interaction check 
focal_check.pdf: focal_check.rmd eco.fake.rds
	$(knitpdf)
simple_example.Rout: simple_example.R

######################################################################

### Makestuff

Sources += Makefile todo.md

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
