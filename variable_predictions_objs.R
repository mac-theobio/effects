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
beta_age <- 0.1 # Focal predictor effect

############################################################################################
# Section: 2 Quantities of interest
############################################################################################

## Coefficient estimate
qoi_df <- linearsim(N=1000, form=~1+x1+x2
	, betas=c(1.5, 1, 2)
	, pgausian=list(p=2, fun=rnorm, mean=c(0.2,0), sd=1)
	, pcat=list(p=0)
	, link_scale = TRUE
)$data
colnames(qoi_df) <- c("age", "wealthindex", "hhsize")
qoi_mod1 <- lm(hhsize~age+wealthindex, qoi_df)
qoi_mod2 <- lm(hhsize~age*wealthindex, qoi_df)

## Prediction plot
qoi_age_pred <- varpred(qoi_mod1, "age")
qoi_age_pred_plot <- (plot(qoi_age_pred, ci=FALSE)
	+ geom_segment(aes(x=0,y=coef(qoi_mod1)[[1]],xend=1,yend=coef(qoi_mod1)[[1]]), colour="blue")
	+ geom_segment(aes(x=min(qoi_age_pred$preds$age), y=coef(qoi_mod1)[[1]]+coef(qoi_mod1)[[2]], xend=1, yend=coef(qoi_mod1)[[1]]+coef(qoi_mod1)[[2]]), linetype=2)
	+ geom_segment(aes(x=min(qoi_age_pred$preds$age), y=coef(qoi_mod1)[[1]], xend=0, yend=coef(qoi_mod1)[[1]]), linetype=2)
	+ geom_segment(aes(x=1,y=coef(qoi_mod1)[[1]],xend=1,yend=coef(qoi_mod1)[[1]]+coef(qoi_mod1)[[2]]), colour="blue")
	+ geom_segment(aes(x=1,y=min(qoi_age_pred$preds$fit),xend=1,yend=coef(qoi_mod1)[[1]]), linetype=2)
	+ geom_segment(aes(x=0,y=min(qoi_age_pred$preds$fit),xend=0,yend=coef(qoi_mod1)[[1]]), linetype=2)
	+ annotate('text', x = 0.5, y = 1, label = "Delta(x)==1",parse = TRUE,size=5, colour="red") 
	+ annotate('text', x = -1, y = 2, label = paste0("Delta(y)==", round(coef(qoi_mod1)[[2]],3)),parse = TRUE,size=5, colour="red") 
	+ annotate('text', x = 0, y = 4, label = paste0("Marginal~effect(x)==", round(coef(qoi_mod1)[[2]],3)),parse = TRUE,size=5, colour="blue") 
	+ coord_cartesian(expand=FALSE)
	+ labs(y="Predicted household size")
)
print(qoi_age_pred_plot)

############################################################################################
# Uncertainty propagation
############################################################################################

## Continuous predictors

### No interaction model

#### Simulation
sim_df_cni <- linearsim(N=N_obs, form=~1+x1+x2
	, betas = c(1.5, 0.1, 2)
	, pgausian=list(p=2,fun=rnorm, mean=c(0.2,0), sd=c(1,1))
	, pcat=list(p=0)
	, link_scale=TRUE
)$data
colnames(sim_df_cni) <- c("age", "wealthindex", "hhsize")
true_prop_cni <- mean(sim_df_cni$hhsize)
focal_prop_cni <- mean(sim_df_cni$age)
#### Model
##### Non-centered
mod_cni <- lm(hhsize~age+wealthindex, sim_df_cni)
##### Centered predictor
sim_df_cni_cen <- sim_df_cni
sim_df_cni_cen$age <- drop(scale(sim_df_cni$age, scale=FALSE))
mod_cen_cni <- lm(hhsize~age+wealthindex, sim_df_cni_cen)

#### Binned data
binned_df_cni <- binfun(mod_cni, "age", "wealthindex", bins=10)

#### Variable effect
##### Traditional CI
pred_age_trad_cni <- varpred(mod_cni, "age", isolate=FALSE, pop.ave="none", modelname="everything")
pred_age_trad_cni_plot <- (plot(pred_age_trad_cni) 
	+ geom_hline(yintercept=true_prop_cni, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_trad_cni$preds$fit), lty=2, colour="blue")
	+ geom_vline(xintercept=focal_prop_cni, lty=2, colour="grey")
	+ geom_point(data=binned_df_cni, aes(x=age, y=hhsize), colour="grey")
	+ labs(y="Predicted household size")
)
print(pred_age_trad_cni_plot)

##### Centered
###### Variance-covariance
pred_age_vcov_cni <- varpred(mod_cen_cni, "age", isolate=FALSE
	, vcov. = zero_vcov(mod_cen_cni, "age"), pop.ave="none"
	, modelname = "zero-vcov"
)
pred_age_vcov_cni$preds$age <- pred_age_vcov_cni$preds$age + focal_prop_cni
##### Centered model matrix
pred_age_centered_cni <- varpred(mod_cni, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)
pred_age_all_cni <- pred_age_centered_cni
pred_age_all_cni$preds <- do.call("rbind", list(pred_age_trad_cni$preds, pred_age_centered_cni$preds, pred_age_vcov_cni$preds))
pred_age_all_cni_plot <- (plot(pred_age_all_cni)
	+ geom_hline(yintercept=true_prop_cni, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_centered_cni$preds$fit), lty=2, colour="yellow")
	+ geom_vline(xintercept=focal_prop_cni, lty=2, colour="grey")
#	+ geom_point(data=binned_df_cni, aes(x=age, y=hhsize, coulour="binned"))
	+ scale_colour_manual(breaks = c("everything", "zero-vcov", "centered mm")
		, values=c(everything="blue", "centered mm"="red", "zero-vcov"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_age_all_cni_plot)

### With interaction between non-focal

#### Simulation
sim_df_wi_nf <- linearsim(N=N_obs, form=~1+x1+x2+x3+x2:x3
	, betas=c(1.5,0.1,2,1.5,1)
	, pgausian=list(p=3,fun=rnorm, mean=c(0.2,0,0), sd=c(1,1,1))
	, pcat=list(p=0)
	, link_scale=TRUE
)$data
colnames(sim_df_wi_nf) <- c("age", "wealthindex", "expenditure", "hhsize")
true_prop_wi_nf <- mean(sim_df_wi_nf$hhsize)
focal_prop_wi_nf <- mean(sim_df_wi_nf$age)
#### Model
##### Non-centered
mod_wi_nf <- lm(hhsize~age+wealthindex+expenditure+wealthindex:expenditure, sim_df_wi_nf)
##### Centered predictor
sim_df_wi_nf_cen <- sim_df_wi_nf
sim_df_wi_nf_cen$age <- drop(scale(sim_df_wi_nf$age, scale=FALSE))
mod_cen_wi_nf <- lm(hhsize~age+wealthindex+expenditure+wealthindex:expenditure, sim_df_wi_nf_cen)

#### Binned data
binned_df_wi_nf <- binfun(mod_wi_nf, "age", c("wealthindex", "expenditure"), bins=10)

#### Variable effect
##### Traditional CI
pred_age_trad_wi_nf <- varpred(mod_wi_nf, "age", isolate=FALSE, pop.ave="none", modelname="everything")
pred_age_trad_wi_nf_plot <- (plot(pred_age_trad_wi_nf) 
	+ geom_hline(yintercept=true_prop_wi_nf, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_trad_wi_nf$preds$fit), lty=2, colour="blue")
	+ geom_vline(xintercept=focal_prop_wi_nf, lty=2, colour="grey")
	+ geom_point(data=binned_df_wi_nf, aes(x=age, y=hhsize), colour="grey")
	+ labs(y="Predicted household size")
)
print(pred_age_trad_wi_nf_plot)

##### Centered
###### Variance-covariance
pred_age_vcov_wi_nf <- varpred(mod_cen_wi_nf, "age", isolate=FALSE
	, vcov. = zero_vcov(mod_cen_wi_nf, "age"), pop.ave="none"
	, modelname = "zero-vcov"
)
pred_age_vcov_wi_nf$preds$age <- pred_age_vcov_wi_nf$preds$age + focal_prop_wi_nf
##### Centered model matrix
pred_age_centered_wi_nf <- varpred(mod_wi_nf, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)
pred_age_all_wi_nf <- pred_age_centered_wi_nf
pred_age_all_wi_nf$preds <- do.call("rbind", list(pred_age_trad_wi_nf$preds, pred_age_centered_wi_nf$preds, pred_age_vcov_wi_nf$preds))
pred_age_all_wi_nf_plot <- (plot(pred_age_all_wi_nf)
	+ geom_hline(yintercept=true_prop_wi_nf, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_centered_wi_nf$preds$fit), lty=2, colour="yellow")
	+ geom_vline(xintercept=focal_prop_wi_nf, lty=2, colour="grey")
#	+ geom_point(data=binned_df_wi_nf, aes(x=age, y=hhsize, coulour="binned"))
	+ scale_colour_manual(breaks = c("everything", "zero-vcov", "centered mm")
		, values=c(everything="blue", "centered mm"="red", "zero-vcov"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_age_all_wi_nf_plot)


### With interaction between focal and non-focal predictor

#### Simulation
sim_df_wi_f <- linearsim(N=N_obs, form=~1+x1+x2+x3+x1:x2
	, betas=c(1.5,0.1,2,1.5,1)
	, pgausian=list(p=3,fun=rnorm, mean=c(0.2,0,0), sd=c(1,1,1))
	, pcat=list(p=0)
	, link_scale=TRUE
)$data
colnames(sim_df_wi_f) <- c("age", "wealthindex", "expenditure", "hhsize")
true_prop_wi_f <- mean(sim_df_wi_f$hhsize)
focal_prop_wi_f <- mean(sim_df_wi_f$age)
#### Model
##### Non-centered
mod_wi_f <- lm(hhsize~age+wealthindex+expenditure+age:wealthindex, sim_df_wi_f)
##### Centered predictor
sim_df_wi_f_cen <- sim_df_wi_f
sim_df_wi_f_cen$age <- drop(scale(sim_df_wi_f_cen$age, scale=FALSE))
mod_cen_wi_f <- lm(hhsize~age+wealthindex+expenditure+age:wealthindex, sim_df_wi_f_cen)

#### Binned data
binned_df_wi_f <- binfun(mod_wi_f, "age", c("wealthindex", "expenditure"), bins=10)

#### Variable effect
##### Traditional CI
pred_age_trad_wi_f <- varpred(mod_wi_f, "age", isolate=FALSE, pop.ave="none", modelname="everything")
pred_age_trad_wi_f_plot <- (plot(pred_age_trad_wi_f) 
	+ geom_hline(yintercept=true_prop_wi_f, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_trad_wi_f$preds$fit), lty=2, colour="blue")
	+ geom_vline(xintercept=focal_prop_wi_f, lty=2, colour="grey")
	+ geom_point(data=binned_df_wi_f, aes(x=age, y=hhsize), colour="grey")
	+ labs(y="Predicted household size")
)
print(pred_age_trad_wi_f_plot)

##### Centered
###### Variance-covariance
pred_age_vcov_wi_f <- varpred(mod_cen_wi_f, "age", isolate=FALSE
	, vcov. = zero_vcov(mod_cen_wi_f, "age"), pop.ave="none"
	, modelname = "zero-vcov"
)
## rescale age
pred_age_vcov_wi_f$preds$age <- pred_age_vcov_wi_f$preds$age + focal_prop_wi_f


##### Centered model matrix
pred_age_centered_wi_f <- varpred(mod_wi_f, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)
head(pred_age_centered_wi_f$preds)
pred_age_all_wi_f <- pred_age_centered_wi_f
pred_age_all_wi_f$preds <- do.call("rbind", list(pred_age_trad_wi_f$preds, pred_age_centered_wi_f$preds, pred_age_vcov_wi_f$preds))
pred_age_all_wi_f_plot <- (plot(pred_age_all_wi_f)
	+ geom_hline(yintercept=true_prop_wi_f, lty=2, colour="grey")
	+ geom_hline(yintercept=mean(pred_age_centered_wi_f$preds$fit), lty=2, colour="yellow")
	+ geom_vline(xintercept=focal_prop_wi_f, lty=2, colour="grey")
#	+ geom_point(data=binned_df_wi_f, aes(x=age, y=hhsize, coulour="binned"))
	+ scale_colour_manual(breaks = c("everything", "zero-vcov", "centered mm")
		, values=c(everything="blue", "centered mm"="red", "zero-vcov"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
)
print(pred_age_all_wi_f_plot)

pred_age_cont_plots <- ggarrange(pred_age_all_cni_plot
	, pred_age_all_wi_nf_plot + rremove("ylab")
	, pred_age_all_wi_f_plot #+ rremove("ylab")
#	, nrow = 1
	, labels = c("a)", "b)", "c)")
	, legend = "bottom"
	, common.legend = TRUE
)
print(pred_age_cont_plots)

saveEnvironment()
