library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmmTMB)

commandEnvironments()
makeGraphics()

### Combine all the predictions
all_vars_pred_plots <- lapply(all_pred_vars, function(x){
	vlist <- list(all_varpred_none_df[[x]], all_varpred_pop_df[[x]])
	obs_dd <- observed_props_list[[x]]
	p1 <- (combinevarpred(vlist, plotit = TRUE, facet_scales="free_y")
		+ geom_point(data=obs_dd, aes(colour=model), size=0.8)
		+ scale_colour_manual(breaks = c("none", "pop", "observed")
			, values=c("none"="red", "pop"="black", "observed"="blue")
		)
		+ scale_linetype_manual(values=c("none"="solid", "pop"="solid"))
		+ labs(y="Probability of improved service", colour="Method")
		+ guides(linetype="none")
		+ theme(legend.position="bottom")
	)
	return(p1)
})

print(all_vars_pred_plots)

saveEnvironment()
