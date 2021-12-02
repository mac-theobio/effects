library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

rpcall("mediate_bin_preds_adjust.Rout mediate_bin_preds_adjust.R mediate_model.rda")
loadEnvironments()
startGraphics()

## Not mediated
### No bias adjustment
pred_none_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="none"
	, modelname="none"
)

plot(pred_none_notmediated)
