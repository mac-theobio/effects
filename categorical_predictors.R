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
N_obs <- 100

############################################################################################
# Section: 4.4.2 Categorical predictors
############################################################################################

### No interaction model

#### Simulation
sim_catni <- linearsim(N=N_obs
	, form=~1+x1+x2
	, betas=c(1.5, 0.5, 0.2)
	, pgausian=list(p=1, fun=rnorm, mean=0, sd=1)
	, pcat=list(p=1, fun=sample, nlevels=2
		, labels=c("Male", "Female")
		, prob=c(0.4, 0.6)
		, replace=TRUE
	)
	, link_scale=TRUE
	, vnames=c("age", "gender", "hhsize")
)
sim_df_catni <- sim_catni$data
print(sim_catni$betas)
head(sim_df_catni)
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
pred_gender_trad_catni <- varpred(mod_catni, "gender", isolate=FALSE
	, pop.ave="none", modelname="everything"
)
pred_gender_trad_catni_plot <- (plot(pred_gender_trad_catni) 
	+ geom_point(data=focal_prop_catni, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_gender_trad_catni_plot)

##### Centered model matrix
pred_gender_centered_catni <- varpred(mod_catni, "gender", isolate=TRUE
	, pop.ave="none", modelname="centered mm"
)
pred_gender_centered_catni_plot <- (plot(pred_gender_centered_catni) 
	+ geom_point(data=focal_prop_catni, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_gender_centered_catni_plot)

pred_gender_all_catni <- pred_gender_centered_catni
pred_gender_all_catni$preds <- do.call("rbind", list(pred_gender_trad_catni$preds, pred_gender_centered_catni$preds))
pred_gender_all_catni_plot <- (plot(pred_gender_all_catni)
	+ geom_point(data=focal_prop_catni, aes(x=gender, y=hhsize, colour="truth"))
	+ scale_colour_manual(breaks = c("truth", "everything", "centered mm")
		, values=c(truth="red", everything="blue", "centered mm"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_gender_all_catni_plot)


### With interaction between non-focal predictors

#### Simulation
sim_catwi_nf <- linearsim(N=N_obs
	, form=~1+x1+x2+x3+x1:x2
	, betas=c(1.5, 0.5, 2, 0.2, 1)
	, pgausian=list(p=2, fun=rnorm, mean=0, sd=1)
	, pcat=list(p=1, fun=sample, nlevels=2
		, labels=c("Male", "Female")
		, prob=c(0.4, 0.6)
		, replace=TRUE
	)
	, link_scale=TRUE
	, vnames=c("age", "wealthindex", "gender", "hhsize")
)
sim_df_catwi_nf <- sim_catwi_nf$data
print(sim_catwi_nf$betas)
head(sim_df_catwi_nf)
focal_prop_catwi_nf <- (sim_df_catwi_nf
	%>% group_by(gender)
	%>% summarize(hhsize=mean(hhsize))
)
focal_prop_catwi_nf
#### Model
##### Non-centered
mod_catwi_nf <- lm(hhsize~age+wealthindex+gender+age:wealthindex, sim_df_catwi_nf)
summary(mod_catwi_nf)

#### Variable effect
##### Traditional CI
pred_gender_trad_catwi_nf <- varpred(mod_catwi_nf, "gender", isolate=FALSE
	, pop.ave="none", modelname="everything"
)
pred_gender_trad_catwi_nf_plot <- (plot(pred_gender_trad_catwi_nf) 
	+ geom_point(data=focal_prop_catwi_nf, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_gender_trad_catwi_nf_plot)

##### Centered model matrix
pred_gender_centered_catwi_nf <- varpred(mod_catwi_nf, "gender", isolate=TRUE
	, pop.ave="none", modelname="centered mm"
)
pred_gender_centered_catwi_nf_plot <- (plot(pred_gender_centered_catwi_nf) 
	+ geom_point(data=focal_prop_catwi_nf, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_gender_centered_catwi_nf_plot)

pred_gender_all_catwi_nf <- pred_gender_centered_catwi_nf
pred_gender_all_catwi_nf$preds <- do.call("rbind", list(pred_gender_trad_catwi_nf$preds, pred_gender_centered_catwi_nf$preds))
pred_gender_all_catwi_nf_plot <- (plot(pred_gender_all_catwi_nf)
	+ geom_point(data=focal_prop_catwi_nf, aes(x=gender, y=hhsize, colour="truth"))
	+ scale_colour_manual(breaks = c("truth", "everything", "centered mm")
		, values=c(truth="red", everything="blue", "centered mm"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_gender_all_catwi_nf_plot)


### With interaction between focal and non-focal predictors

#### Simulation
sim_catwi_f <- linearsim(N=N_obs
	, form=~1+x1+x2+x3+x1:x3
	, betas=c(1.5, 0.5, 2, 0.2, 1)
	, pgausian=list(p=2, fun=rnorm, mean=0, sd=1)
	, pcat=list(p=1, fun=sample, nlevels=2
		, labels=c("Male", "Female")
		, prob=c(0.4, 0.6)
		, replace=TRUE
	)
	, link_scale=TRUE
	, vnames=c("age", "wealthindex", "gender", "hhsize")
)
sim_df_catwi_f <- sim_catwi_f$data
print(sim_catwi_f$betas)
head(sim_df_catwi_f)
focal_prop_catwi_f <- (sim_df_catwi_f
	%>% group_by(gender)
	%>% summarize(hhsize=mean(hhsize))
)
focal_prop_catwi_f
#### Model
##### Non-centered
mod_catwi_f <- lm(hhsize~age+wealthindex+gender+age:gender, sim_df_catwi_f)
summary(mod_catwi_f)

#### Variable effect
##### Traditional CI
pred_gender_trad_catwi_f <- varpred(mod_catwi_f, "gender", isolate=FALSE
	, pop.ave="none", modelname="everything"
)
pred_gender_trad_catwi_f_plot <- (plot(pred_gender_trad_catwi_f) 
	+ geom_point(data=focal_prop_catwi_f, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_gender_trad_catwi_f_plot)

##### Centered model matrix
pred_gender_centered_catwi_f <- varpred(mod_catwi_f, "gender", isolate=TRUE
	, pop.ave="none", modelname="centered mm"
)
pred_gender_centered_catwi_f_plot <- (plot(pred_gender_centered_catwi_f) 
	+ geom_point(data=focal_prop_catwi_f, aes(x=gender, y=hhsize), colour="red")
	+ labs(y="Predicted household size")
)
print(pred_gender_centered_catwi_f_plot)

pred_gender_all_catwi_f <- pred_gender_centered_catwi_f
pred_gender_all_catwi_f$preds <- do.call("rbind", list(pred_gender_trad_catwi_f$preds, pred_gender_centered_catwi_f$preds))
pred_gender_all_catwi_f_plot <- (plot(pred_gender_all_catwi_f)
	+ geom_point(data=focal_prop_catwi_f, aes(x=gender, y=hhsize, colour="truth"))
	+ scale_colour_manual(breaks = c("truth", "everything", "centered mm")
		, values=c(truth="red", everything="blue", "centered mm"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_gender_all_catwi_f_plot)

pred_gender_cat_plots <- ggarrange(pred_gender_all_catni_plot
	, pred_gender_all_catwi_nf_plot + rremove("ylab")
	, pred_gender_all_catwi_f_plot #+ rremove("ylab")
#	, nrow = 1
	, labels = c("a)", "b)", "c)")
	, legend = "bottom"
	, common.legend = TRUE
)
print(pred_gender_cat_plots)

saveEnvironment()
