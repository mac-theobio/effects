## This is a development subdirectory of Bicko's vareffects project

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

autopipeR = defined

Sources += inputs/fake.rds $(wildcard *.R)

fit.Rout: fit.R inputs/fake.rds

varEf.Rout: varEf.R fit.rda

######################################################################

## Teaching JD how to vareffects

## Ancient, can we drop? 2021 Oct 22 (Fri)
demonstrate.Rout: demonstrate.R

mediate.Rout: mediate.R

spline.Rout: spline.R

## Work on this one!!!
binom.Rout: binom.R

######################################################################

## Spline exploration

bs.Rout: bs.R

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
