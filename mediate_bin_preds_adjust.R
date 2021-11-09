library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

commandEnvironments()
makeGraphics()

## Not mediated
### No bias adjustment
pred_none_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="none"
	, modelname="none"
)

### Delta-sigma
pred_delta_mod_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="delta"
	, sigma="mod"
	, modelname="delta-mod"
)

### Delta-lp
pred_delta_lp_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="delta"
	, sigma="lp"
	, modelname="delta-lp"
)

### mcculloch-sigma
pred_mc_mod_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="mcculloch"
	, sigma="mod"
	, modelname="mc-mod"
)

### mcculloch-sigma
pred_mc_lp_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="mcculloch"
	, sigma="lp"
	, modelname="mc-lp"
)

### population
pred_pop_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, steps=50
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="pop"
)

### Combine all predictions
vlist <- list(pred_none_notmediated
	, pred_delta_mod_notmediated
	, pred_delta_lp_notmediated
	, pred_mc_mod_notmediated
	, pred_mc_lp_notmediated
	, pred_pop_notmediated
)

pred_notmediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
#	+ geom_hline(yintercept=pred_prop_notmed, lty=2, col="black")
	+ geom_hline(data=observed_df_med, aes(yintercept=zbin), lty=2, col="red")
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="black")
	+ labs(y="Predictions", colour="Method", title="Not mediated")
	+ theme(legend.position="right")
)
print(pred_notmediated_plots)

## Mediated
### No bias adjustment
pred_none_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="none"
	, modelname="none"
)

### Delta-sigma
pred_delta_mod_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="delta"
	, sigma="mod"
	, modelname="delta-mod"
)

### Delta-lp
pred_delta_lp_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="delta"
	, sigma="lp"
	, modelname="delta-lp"
)

### mcculloch-sigma
pred_mc_mod_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="mcculloch"
	, sigma="mod"
	, modelname="mc-mod"
)

### mcculloch-sigma
pred_mc_lp_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="mcculloch"
	, sigma="lp"
	, modelname="mc-lp"
)

### population
pred_pop_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="pop"
)

### Combine all predictions
vlist <- list(pred_none_mediated
	, pred_delta_mod_mediated
	, pred_delta_lp_mediated
	, pred_mc_mod_mediated
	, pred_mc_lp_mediated
	, pred_pop_mediated
)

pred_mediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
#	+ geom_hline(yintercept=pred_prop_notmed, lty=2, col="black")
	+ geom_hline(data=observed_df_med, aes(yintercept=zbin), lty=2, col="red")
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="black")
	+ labs(y="Predictions", colour="Method", title="Not mediated")
	+ theme(legend.position="right")
)
print(pred_mediated_plots)

quit()
pred_mediate_plots <- ggarrange(pred_x_notmediated_plots
	, pred_x_mediated_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_mediate_plots)

saveEnvironment()

