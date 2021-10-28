library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

commandEnvironments()
makeGraphics()

## Correlated
pred_x_trad_bin_corr <- varpred(mod_bin_corr
	, "x"
	, isolate=FALSE
	, modelname="everything"
)

pred_x_centered_bin_corr <- varpred(mod_bin_corr
	, "x"
	, isolate=TRUE
	, modelname="centered mm"
)

### Combine all predictions
vlist <- list(pred_x_trad_bin_corr, pred_x_centered_bin_corr)

pred_x_bin_corr_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ geom_hline(data=observed_df_corr, aes(yintercept=z), lty=2)
	+ labs(y="Predictions", colour="Method", title="Correlated")
	+ theme(legend.position="bottom")
)
print(pred_x_bin_corr_plots)

## No correlation
pred_x_trad_bin_noncorr <- varpred(mod_bin_noncorr
	, "x"
	, isolate=FALSE
	, modelname="everything"
)

pred_x_centered_bin_noncorr <- varpred(mod_bin_noncorr
	, "x"
	, isolate=TRUE
	, modelname="centered mm"
)

### Combine all predictions
vlist <- list(pred_x_trad_bin_noncorr, pred_x_centered_bin_noncorr)

pred_x_bin_noncorr_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ geom_hline(data=observed_df_noncorr, aes(yintercept=z), lty=2)
	+ labs(y="Predictions", colour="Method", title="No correlation")
	+ theme(legend.position="bottom")
)
print(pred_x_bin_noncorr_plots)


pred_bin_corr_plots <- ggarrange(pred_x_bin_corr_plots
	, pred_x_bin_noncorr_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_bin_corr_plots)

saveEnvironment()

