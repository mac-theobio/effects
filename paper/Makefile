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

## eco.uniN.defAnchor.nitroPlot.Rout

## New MS examples 2022 Aug 10 (Wed)

## Fake data about nutrients and biomass
eco_data.Rout: eco_data.R

## Fit different models 
## eco.uniN.mfit.Rout: uniN.R
## eco.multi.mfit.Rout: multi.R
## eco.maximal.mfit.Rout: maximal.R
scriptStep += mfit
mfit += uniN multi quadN maximal
mfit_dep = %_data.rds

## make different varpred objects, using N as focal variable
## eco.maximal.defAnchor.vp.Rout: defAnchor.R
scriptStep += vp
vp += defAnchor anchor20
vp_dep += %.mfit.rda

## Model plots
scriptStep += plot
plot += nitro ncomp
plot_dep += %.vp.rda
## eco.uniN.defAnchor.nitro.plot.Rout: nitro.R
## eco.maximal.defAnchor.ncomp.plot.Rout: ncomp.R

## Composites
eco.splash.Rout: splash.R eco.uniN.defAnchor.nitro.plot.rda eco.uniN.anchor20.nitro.plot.rda
	$(pipeR)

## Univariate/multi-variate
## eco.mcomp.Rout: mcomp.R eco_data.R
eco.mcomp.Rout: mcomp.R eco.uniN.defAnchor.nitro.plot.rda eco.multi.defAnchor.nitro.plot.rda 
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
