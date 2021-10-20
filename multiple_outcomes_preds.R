library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

commandEnvironments()
makeGraphics()

## Services

within.category <- FALSE

### All uncertainties 
pred_services_trad_cont_joint <- varpred(mod_cont_joint
	, "services"
	, within.category=within.category
	, modelname="everything"
)

### Centered predictions
pred_services_centered_cont_joint <- varpred(mod_cont_joint
	, "services"
	, isolate=TRUE
	, within.category=within.category
	, zero_out_interaction=FALSE
	, modelname="centered mm"
)

### Combine all predictions
vlist <- list(pred_services_trad_cont_joint, pred_services_centered_cont_joint)

pred_services_cont_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_point(data=true_prop_df, aes(x=services, y=fit), colour="red")
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ labs(y="Predictions", colour="Method")
	+ theme(legend.position="bottom")
)

## Age

### All uncertainties 
pred_age_trad_cont_joint <- varpred(mod_cont_joint
	, c("services", "age")
	, x.var="age"
	, within.category=within.category
	, modelname="everything"
)

### Centered predictions
pred_age_centered_cont_joint <- varpred(mod_cont_joint
	, c("services", "age")
	, x.var="age"
	, isolate=TRUE
	, within.category=within.category
	, zero_out_interaction=FALSE
	, modelname="centered mm"
)


### Combine all predictions
vlist <- list(pred_age_trad_cont_joint, pred_age_centered_cont_joint)

pred_age_cont_joint <- comparevarpred(vlist=vlist
	, lnames=NULL
	, plotit=FALSE
	, addmarginals=FALSE
)

## Marginals
### Services
pred_marg_age_prop <- (pred_age_cont_joint$preds
	%>% group_by(services)
	%>% summarize(fit=mean(fit))
	%>% ungroup()
	%>% mutate(services=paste0("services: ", services))
)
print(pred_marg_age_prop)

### Focal predictor
focal_prop_df <- (pred_age_cont_joint$preds
	%>% group_by(services)
	%>% summarize(age=mean(age))
	%>% ungroup()
	%>% mutate(services=paste0("services: ", services))
)
print(focal_prop_df)

### Truth
true_prop_df <- (true_prop_df
	%>% mutate(services=paste0("services: ", services))
)

pred_age_cont_joint_plots <- (plot(pred_age_cont_joint)
	+ geom_hline(data=pred_marg_age_prop, aes(yintercept=fit), lty=2, colour="grey")
	+ geom_vline(data=focal_prop_df, aes(xintercept=age), lty=2, colour="grey")
	+ geom_hline(data=true_prop_df, aes(yintercept=fit), lty=2, colour="yellow")
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="blue", "centered mm"="black")
	)
	+ labs(y="Predictions", colour="Method")
	+ theme(legend.position="bottom")
)


pred_cont_joint_plots <- ggarrange(pred_services_cont_plots
	, pred_age_cont_joint_plots
	, common.legend=TRUE
	, legend="bottom"
	, nrow=2
)
print(pred_cont_joint_plots)

saveEnvironment()
