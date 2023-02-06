## vareffects follow-on paper

current: target
-include target.mk

vim_session:
	bash -cl "vmt paper.tex body.tex"

######################################################################

Sources += $(wildcard *.R *.bib *.bst *.Rmd)

######################################################################

## New paper version

## draft.paper.pdf: paper.tex draft.head.tex

Ignore += *.paper.tex *.pdf

paperfiles = mscommands.tex begindoc.tex title.tex pabs.tex paper.tex msbib.tex enddoc.tex
Sources += *.head.tex $(paperfiles)

## draft.paper.tex: 
%.paper.tex: %.head.tex $(paperfiles)
	$(catro)

######################################################################

autopipeR = defined

######################################################################

## eco.uniN.defAnchor.nitroPlot.Rout

## New MS examples 2022 Aug 10 (Wed)
Sources += $(wildcard *.md)

## Fake data about nutrients and biomass
eco_data.Rout: eco_data.R

## Fit different models ## mfit.md
scriptStep += mfit
mfit += uniN quadN inter multi multiPK
mfit_dep = %_data.rds
## eco.uniN.mfit.Rout: uniN.R
## eco.multi.mfit.Rout: multi.R
## eco.inter.mfit.Rout: inter.R

## make different prediction objects ## vp.md
scriptStep += vp
vp += defAnchor anchor20
vp_dep += %.mfit.rda
## eco.inter.defAnchor.vp.Rout: defAnchor.R

## Model plots plot.md
scriptStep += plot
plot += nitro centers
plot_dep += %.vp.rda
## eco.uniN.defAnchor.nitro.plot.Rout: nitro.R
## eco.multiPK.defAnchor.centers.plot.Rout: centers.R
## eco.inter.defAnchor.centers.plot.Rout: centers.R

## Composites
## eco.uniN.defAnchor.nitro.plot.Rout.pdf eco.uniN.anchor20.nitro.plot.Rout.pdf
eco.splash.Rout: splash.R eco.uniN.defAnchor.nitro.plot.rda eco.uniN.anchor20.nitro.plot.rda
	$(pipeR)

## Univariate/multi-variate
## eco.uniMulti.Rout: uniMulti.R eco_data.R
uniMulti: eco.uniN.defAnchor.nitro.plot.Rout.pdf eco.multi.defAnchor.nitro.plot.Rout.pdf 
eco.uniMulti.Rout: uniMulti.R eco.uniN.defAnchor.nitro.plot.rda eco.multi.defAnchor.nitro.plot.rda 
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
