library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Not mediated
pred_x_trad_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=FALSE
	, modelname="everything"
)

pred_x_centered_notmediated <- varpred(mod_notmediated_bin
	, "x"
	, isolate=TRUE
	, modelname="centered mm"
)
pred_prop_notmed <- mean(pred_x_centered_notmediated$preds$fit)

### Combine all predictions
vlist <- list(pred_x_trad_notmediated, pred_x_centered_notmediated)

pred_x_notmediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ geom_hline(yintercept=pred_prop_notmed, lty=2, col="black")
	+ geom_hline(data=observed_df_med, aes(yintercept=zbin), lty=2, col="red")
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="black")
	+ labs(y="Predictions", colour="Method", title="Not mediated")
	+ theme(legend.position="bottom")
)
print(pred_x_notmediated_plots)

## Mediated
pred_x_trad_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=FALSE
	, modelname="everything"
)

pred_x_centered_mediated <- varpred(mod_mediated_bin
	, "x"
	, isolate=TRUE
	, modelname="centered mm"
)
pred_prop_med <- mean(pred_x_centered_mediated$preds$fit)

### Combine all predictions
vlist <- list(pred_x_trad_mediated, pred_x_centered_mediated)

pred_x_mediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ geom_hline(yintercept=pred_prop_med, lty=2, col="black")
	+ geom_hline(data=observed_df_med, aes(yintercept=zbin), lty=2, col="red")
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="black")
	+ labs(y="Predictions", colour="Method", title="Mediated")
	+ theme(legend.position="bottom")
)
print(pred_x_mediated_plots)


pred_mediate_plots <- ggarrange(pred_x_notmediated_plots
	, pred_x_mediated_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_mediate_plots)

saveEnvironment()

