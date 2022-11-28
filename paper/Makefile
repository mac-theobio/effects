## vareffects follow-on paper

current: target
-include target.mk

vim_session:
	bash -cl "vmt paper.tex body.tex"

######################################################################

Sources += $(wildcard *.R *.bib *.bst *.Rmd)

## Sources += notes.md

######################################################################

## New paper version

## draft.paper.pdf: paper.tex draft.head.tex

Ignore += *.paper.tex *.paper.pdf

paperfiles = mscommands.tex begindoc.tex title.tex pabs.tex paper.tex msbib.tex enddoc.tex
Sources += *.head.tex $(paperfiles)

## draft.paper.tex: 
%.paper.tex: %.head.tex $(paperfiles)
	$(catro)

######################################################################

autopipeR = defined

######################################################################

## New MS examples 2022 Aug 10 (Wed)

## Fake data about nutrients and biomass
eco_data.Rout: eco_data.R

## Fit different models
## eco.uniN.mfit.Rout: uniN.R
## eco.fullN.mfit.Rout: fullN.R
## eco.inter.mfit.Rout: inter.R
scriptStep += mfit
mfit += uniN fullN quadN inter
mfit_dep = %_data.rds

## make different varpred objects
## eco.fullN.defAnchor.vp.Rout: defAnchor.R
scriptStep += vp
vp += defAnchor anchor20
vp_dep += %.mfit.rda

## Model plots
pipeRdesc += nitroPlot
pipeRimplicit += nitroPlot
## eco.uniN.defAnchor.nitroPlot.Rout: nitroPlot.R
## eco.uniN.anchor20.nitroPlot.Rout: nitroPlot.R
## eco.fullN.defAnchor.nitroPlot.Rout: nitroPlot.R
## eco.quadN.defAnchor.nitroPlot.Rout: nitroPlot.R
## eco.quadN.anchor20.nitroPlot.Rout: nitroPlot.R
## eco.inter.defAnchor.nitroPlot.Rout: nitroPlot.R
%.nitroPlot.Rout: nitroPlot.R %.vp.rda
	$(pipeR)

## Composites
eco.splash.Rout: splash.R eco.uniN.defAnchor.nitroPlot.rda eco.uniN.anchor20.nitroPlot.rda
	$(pipeR)

## Univariate/multi-variate
## eco.mcomp.Rout: mcomp.R eco_data.R
eco.mcomp.Rout: mcomp.R eco.uniN.defAnchor.nitroPlot.rda eco.fullN.defAnchor.nitroPlot.rda 
	$(pipeR)

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
