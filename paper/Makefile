## vareffects follow-on paper

current: target
-include target.mk

all: draft.paper.pdf

vim_session:
	bash -cl "vmt paper.tex body.tex"

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

## Rename variables and see how the machinery works
## eco.weird.Rout: weird.R
%.weird.Rout: weird.R %.data.rds
	$(pipeR)

Sources += $(wildcard *.md)

## eco.params.Rout: eco.R
## high.params.Rout: high.R
%.params.Rout: %.R
	$(pipeR)

## Fake data about nutrients and biomass ## data.md
%.data.Rout: data.R %.params.rda
	$(pipeR)
## eco.data.Rout: data.R eco.R
## high.data.Rout: data.R

## Plot only
scriptStep += scatter
scatter += robust
scatter_dep = %.data.rds
## eco.robust.scatter.Rout: robust.R

## Fit different models ## mfit.md
scriptStep += mfit
mfit += uniN quadN inter multi multiPK monster
mfit_dep = %.data.rds
## eco.uniN.mfit.Rout: uniN.R
## high.multi.mfit.Rout: multi.R
## eco.inter.mfit.Rout: inter.R
## eco.quadN.mfit.Rout: quadN.R
## high.quadN.mfit.Rout: quadN.R

## fit glm models glmfit.md
scriptStep += glmfit
glmfit += uniglmN
glmfit_dep = %.data.rds
## eco.uniglmN.glmfit.Rout: uniglmN.R

## make different prediction objects ## vp.md
scriptStep += vp
vp += defAnchor anchor20 meanAnchor check
vp_dep += %.mfit.rda
## eco.inter.defAnchor.vp.Rout: defAnchor.R
## eco.inter.meanAnchor.vp.Rout: meanAnchor.R
# Temporary checks; Giving up for now!
## eco.monster.check.vp.Rout: check.R

## make glm prediction objects ## gvp.md
## TODO: move defRef and defAnchor to def!, if they can both be the same
scriptStep += gvp
gvp += defRef obsRef realObsRef
gvp_dep += %.glmfit.rda
## eco.uniglmN.defRef.gvp.Rout: defRef.R
## eco.uniglmN.obsRef.gvp.Rout: obsRef.R
## eco.uniglmN.realObsRef.gvp.Rout: realObsRef.R

## Some sort of check (compare base predict to varpred)
## eco.quadN.predict.Rout: predict.R eco.quadN.mfit.rda
## high.quadN.predict.Rout: predict.R eco.quadN.mfit.rda
%.predict.Rout: predict.R %.mfit.rda
	$(pipeR)

## Model plots plot.md
scriptStep += plot
plot += nitro centers
plot_dep += %.vp.rda

## eco.uniN.defAnchor.nitro.plot.Rout: nitro.R
## eco.quadN.defAnchor.nitro.plot.Rout: nitro.R
## eco.quadN.anchor20.nitro.plot.Rout: nitro.R
## eco.inter.defAnchor.nitro.plot.Rout: nitro.R
## eco.inter.defAnchor.centers.plot.Rout: centers.R

## TODO: When do we need a different track for glm at all?
## Can we save some info about the outcome variable?
## glm plots glmPlot.md
scriptStep += glmPlot
glmPlot += rnitro
glmPlot_dep += %.gvp.rda
## eco.uniglmN.defRef.rnitro.glmPlot.Rout: rnitro.R
## eco.uniglmN.obsRef.rnitro.glmPlot.Rout: rnitro.R

######################################################################
## Composites

## Splash plot combines defAnchor and anchor20
## eco.uniN.defAnchor.nitro.plot.Rout.pdf eco.uniN.anchor20.nitro.plot.Rout.pdf
eco.splash.Rout: splash.R eco.uniN.defAnchor.nitro.plot.rda eco.uniN.anchor20.nitro.plot.rda
	$(pipeR)

## Anchor check: does meanAnchor produce the same splash as defAnchor
## … in simple case
eco.splashCheck.Rout: splashCheck.R eco.uniN.defAnchor.nitro.plot.rda eco.uniN.meanAnchor.nitro.plot.rda
	$(pipeR)

## Univariate/multi-variate
## eco.uniMulti.Rout: uniMulti.R eco_data.R
uniMulti: eco.uniN.defAnchor.nitro.plot.Rout.pdf eco.multi.defAnchor.nitro.plot.Rout.pdf 
eco.uniMulti.Rout: uniMulti.R eco.uniN.defAnchor.nitro.plot.rda eco.multi.defAnchor.nitro.plot.rda 
	$(pipeR)

## Compare quad plots with mean vs. default anchor (read and arrange only)
eco.quadComp.Rout: quadComp.R eco.quadN.defAnchor.nitro.plot.rda eco.quadN.meanAnchor.nitro.plot.rda
	$(pipeR)

######################################################################



## Focal interaction check 
focal_check.pdf: focal_check.rmd eco.data.rds
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
