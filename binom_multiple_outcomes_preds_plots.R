library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmmTMB)

loadEnvironments()
startGraphics()

### Combine all the predictions
all_vars_pred_plots <- lapply(all_pred_vars, function(x){
	vlist <- list(all_varpred_none_df[[x]], all_varpred_pop_df[[x]])
	obs_dd <- observed_props_list[[x]]
	p1 <- (combinevarpred(vlist, plotit = TRUE)
		+ geom_point(data=obs_dd, aes(colour=model), size=0.8)
		+ scale_colour_manual(breaks = c("none", "bias corrected", "observed")
			, values=c("none"="red", "bias corrected"="black", "observed"="grey")
		)
		+ scale_linetype_manual(values=c("none"="solid", "bias corrected"="solid"))
		+ labs(y="Probability of improved service", colour="Method")
		+ guides(linetype="none")
		+ theme(legend.position="bottom")
	)
	return(p1)
})

#print(all_vars_pred_plots)


binom_outcomes_plots <- ggarrange(all_vars_pred_plots[[1]] + rremove("ylab")
	, all_vars_pred_plots[[2]] + rremove("ylab")
	, all_vars_pred_plots[[3]] + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, nrow=3
)
binom_outcomes_plots <- annotate_figure(binom_outcomes_plots
	, left = "Probability of improved service"
)
print(binom_outcomes_plots)

saveVars(binom_outcomes_plots)
