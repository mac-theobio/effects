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
nHH_obs <- 100
beta_age <- 0.1 # Focal predictor effect

############################################################################################
# Section: 2 Quantities of interest
############################################################################################

## Coefficient estimate
qoi_df <- linearsim(nHH=1000, form=~1+x1*x2
	, betas=c(1.5, 1, 2, 0.5)
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
sim_df_cni <- linearsim(nHH=nHH_obs, form=~1+x1+x2
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

### With interaction between non-focal

#### Simulation
sim_df_wi_nf <- linearsim(nHH=nHH_obs, form=~1+x1+x2+x3+x2:x3
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

### With interaction between focal and non-focal predictor

#### Simulation
sim_df_wi_f <- linearsim(nHH=nHH_obs, form=~1+x1+x2+x3+x1:x2
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


#### Combine all predictions

true_prop_cont_df <- data.frame(
	y=c(true_prop_cni
		, true_prop_wi_nf
		, true_prop_wi_f
	)
	, .varpred=c("a) No interaction"
		, "b) Non-focal interaction"
		, "c) Focal-non-focal interaction"
	)

)
lnames <- c("a) No interaction"
	, "a) No interaction"
	, "a) No interaction"
	, "b) Non-focal interaction"
	, "b) Non-focal interaction"
	, "b) Non-focal interaction"
	, "c) Focal-non-focal interaction"
	, "c) Focal-non-focal interaction"
	, "c) Focal-non-focal interaction"
)
vlist <- list(pred_age_trad_cni
	, pred_age_vcov_cni
	, pred_age_centered_cni
	, pred_age_trad_wi_nf
	, pred_age_vcov_wi_nf
	, pred_age_centered_wi_nf
	, pred_age_trad_wi_f
	, pred_age_vcov_wi_f
	, pred_age_centered_wi_f
)
pred_age_cont_plots <- (comparevarpred(vlist=vlist, lnames=lnames
		, plotit=TRUE, addmarginals=TRUE
		, margindex=c(3,6,9)
		, facet_ncol=2
	)
	+ geom_hline(data=true_prop_cont_df, aes(yintercept=y), lty=2, colour="yellow")
	+ scale_colour_manual(breaks = c("everything", "centered mm", "zero-vcov")
		, values=c(everything="blue", "centered mm"="red", "zero-vcov"="black")
	)
	+ labs(y="Predicted household size", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_age_cont_plots)

saveEnvironment()
