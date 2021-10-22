library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

commandEnvironments()
makeGraphics()

## Not mediated
pred_x_trad_notmediated <- varpred(mod_notmediated
	, "x"
	, isolate=FALSE
	, modelname="everything"
)

pred_x_centered_notmediated <- varpred(mod_notmediated
	, "x"
	, isolate=TRUE
	, modelname="centered mm"
)

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
	+ labs(y="Predictions", colour="Method", title="Not mediated")
	+ theme(legend.position="bottom")
)
print(pred_x_notmediated_plots)

## Mediated
pred_x_trad_mediated <- varpred(mod_mediated
	, "x"
	, isolate=FALSE
	, modelname="everything"
)

pred_x_centered_mediated <- varpred(mod_mediated
	, "x"
	, isolate=TRUE
	, modelname="centered mm"
)

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

