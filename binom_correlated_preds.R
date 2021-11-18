library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(emmeans)

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
true_corr <- mean(pred_x_centered_bin_corr$preds$fit)

pred_x_centered_bin_corr_pop <- varpred(mod_bin_corr
	, "x"
	, bias.adjust="population"
	, modelname="pop"
)
true_corr_pop <- mean(pred_x_centered_bin_corr_pop$preds$fit)

print(true_corr)
em_preds <- emmeans(mod_bin_corr, "x", at=list(x=pred_x_centered_bin_corr$preds$x), type="response")
em_preds <- as.data.frame(em_preds)
mean(em_preds$prob)

### Combine all predictions
vlist <- list(pred_x_trad_bin_corr, pred_x_centered_bin_corr, pred_x_centered_bin_corr_pop)

pred_x_bin_corr_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ scale_colour_manual(breaks = c("everything", "centered mm", "pop")
		, values=c(everything="blue", "centered mm"="black", "pop"="red")
	)
	+ geom_hline(data=observed_df_corr, aes(yintercept=z), lty=2, col="grey")
	+ geom_hline(yintercept=true_corr, lty=2)
	+ geom_hline(yintercept=true_corr_pop, lty=2, col="red")
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
true_noncorr <- mean(pred_x_centered_bin_noncorr$preds$fit)

pred_x_centered_bin_noncorr_pop <- varpred(mod_bin_noncorr
	, "x"
	, bias.adjust="population"
	, modelname="pop"
)
true_noncorr_pop <- mean(pred_x_centered_bin_noncorr_pop$preds$fit)

### Combine all predictions
vlist <- list(pred_x_trad_bin_noncorr, pred_x_centered_bin_noncorr, pred_x_centered_bin_noncorr_pop)

pred_x_bin_noncorr_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ scale_colour_manual(breaks = c("everything", "centered mm", "pop")
		, values=c(everything="blue", "centered mm"="black", "pop"="red")
	)
	+ geom_hline(data=observed_df_noncorr, aes(yintercept=z), lty=2, col="grey")
	+ geom_hline(yintercept=true_noncorr, lty=2)
	+ geom_hline(yintercept=true_noncorr_pop, lty=2, col="red")
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

