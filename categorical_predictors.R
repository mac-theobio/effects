library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
N_obs <- 500
gender_prop <- c(0.3, 0.7)
gender_levels <- c("Female", "Male")

############################################################################################
# Section: 4.4.2 Categorical predictors
############################################################################################

### No interaction model

#### Simulation
sim_catni <- linearsim(N=N_obs
	, form=~x1+x2
	, betas=c(1.5,0.8,0.2)
	, link_scale = TRUE
)
sim_df_catni <- sim_catni$data
print(sim_catni$betas)
head(sim_df_catni)
colnames(sim_df_catni) <- c("age", "gender", "hhsize")
focal_prop_catni <- (sim_df_catni
	%>% group_by(gender)
	%>% summarize(hhsize=mean(hhsize))
)
focal_prop_catni
#### Model
##### Non-centered
mod_catni <- lm(hhsize~age+gender, sim_df_catni)
summary(mod_catni)

#### Variable effect
##### Traditional CI
pred_age_trad_catni <- varpred(mod_catni, "gender", isolate=FALSE, pop.ave="none", modelname="everything")
pred_age_trad_catni_plot <- (plot(pred_age_trad_catni) 
#	+ geom_hline(yintercept=true_prop_catni, lty=2, colour="grey")
#	+ geom_hline(yintercept=mean(pred_age_trad_catni$preds$fit), lty=2, colour="blue")
	+ geom_point(data=focal_prop_catni, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_age_trad_catni_plot)

quit()
##### Centered model matrix
pred_age_centered_catni <- varpred(mod_catni, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)
pred_age_all_catni <- pred_age_centered_catni
pred_age_all_catni$preds <- do.call("rbind", list(pred_age_trad_catni$preds, pred_age_centered_catni$preds, pred_age_vcov_catni$preds))
pred_age_all_catni_plot <- (plot(pred_age_all_catni)
	+ geom_hline(yintercept=true_prop_catni, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_centered_catni$preds$fit), lty=2, colour="yellow")
	+ geom_vline(xintercept=focal_prop_catni, lty=2, colour="grey")
#	+ geom_point(data=binned_df_catni, aes(x=age, y=hhsize, coulour="binned"))
	+ scale_colour_manual(breaks = c("everything", "zero-vcov", "centered mm")
		, values=c(everything="blue", "centered mm"="red", "zero-vcov"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_age_all_catni_plot)


saveEnvironment()
