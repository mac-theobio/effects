library(shellpipes)
library(vareffects); varefftheme()
library(splines)
library(ggpubr)
library(ggplot2)
library(dplyr)

commandEnvironments()
makeGraphics()

## Focal polynomial

### All uncertainties 
pred_x1_trad_cubic <- varpred(mod_cubic
	, "x1"
	, modelname="everything"
)

### Centered predictions
pred_x1_centered_cubic <- varpred(mod_cubic
	, "x1"
	, isolate=TRUE
	, modelname="centered mm"
)

### Combine
vlist <- list(pred_x1_trad_cubic, pred_x1_centered_cubic)

pred_x1_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=y), colour="grey", lty=2)
	+ geom_vline(data=true_prop_df, aes(xintercept=x1), colour="grey", lty=2)
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ labs(title="a) Focal polynomial", y="Predictions", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_x1_cubic_plots)

## Non-focal polynomial

### All uncertainties 
pred_x2_trad_cubic <- varpred(mod_cubic
	, "x2"
	, modelname="everything"
)

### Centered predictions
pred_x2_centered_cubic <- varpred(mod_cubic
	, "x2"
	, isolate=TRUE
	, modelname="centered mm"
)

### Combine
vlist <- list(pred_x2_trad_cubic, pred_x2_centered_cubic)

pred_x2_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=y), colour="grey", lty=2)
	+ geom_vline(data=true_prop_df, aes(xintercept=x2), colour="grey", lty=2)
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ labs(title="b) Non-focal is a polynomial", y="Predictions", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_x2_cubic_plots)

## Combine all prediction for faceting
pred_cubic_plots <- ggarrange(pred_x1_cubic_plots
	, pred_x2_cubic_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_cubic_plots)


saveEnvironment()
