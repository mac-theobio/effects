## vareffects manuscript

current: target
-include target.mk


vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R *.bib *.tex)

## Make Sweave weird depencies chain
%: %.pdf ;

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

######################################################################

# vareffects_plosone.bib: vareffects_plosone.bib;
vareffects_plosone.pdf: quantity_of_interest.Rout justify_preds.Rout vareffects_plosone.bib vareffects_plosone.tex

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

-include makestuff/git.mk
-include makestuff/visual.mk
