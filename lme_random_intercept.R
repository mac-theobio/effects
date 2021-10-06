library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmmTMB)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
nHH_obs <- 50
perHH <- 10

############################################################################################
# Linear mixed effect model
############################################################################################

## Varying intercept: one grouping factor

### No interaction model

#### Simulation
sim_df_cni_lme <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, form=~1+x1
	, betas = c(1.5, 0.1)
	, hhSD = 2
	, pgausian=list(p=1,fun=rnorm, mean=0.2, sd=1)
	, pcat=list(p=0)
	, link_scale=TRUE
	, vnames=c("age", "hhsize")
)$data
head(sim_df_cni_lme)

true_prop_cni_lme <- mean(sim_df_cni_lme$hhsize)
focal_prop_cni_lme <- mean(sim_df_cni_lme$age)

#### Model
mod_cni_lme <- glmmTMB(hhsize~age+(1|hhid)
	, data=sim_df_cni_lme
	, family = gaussian()
)
summary(mod_cni_lme)

#### Variable effect
##### Traditional CI
pred_age_trad_cni_lme <- varpred(mod_cni_lme, "age", isolate=FALSE, pop.ave="none", modelname="everything")

##### Centered
##### Centered model matrix
pred_age_centered_cni_lme <- varpred(mod_cni_lme, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)

### With interaction between non-focal

#### Simulation
sim_df_wi_nf_lme <- linearsim(nHH=nHH_obs
	, perHH = perHH
	, form=~1+x1+x2+x3+x2:x3
	, hhSD=2
	, betas=c(1.5,0.1,2,1.5,1)
	, pgausian=list(p=3,fun=rnorm, mean=c(0.2,0,0), sd=c(1,1,1))
	, pcat=list(p=0)
	, link_scale=TRUE
	, vnames=c("age", "wealthindex", "expenditure", "hhsize")
)$data
head(sim_df_wi_nf_lme)

true_prop_wi_nf_lme <- mean(sim_df_wi_nf_lme$hhsize)
focal_prop_wi_nf_lme <- mean(sim_df_wi_nf_lme$age)
#### Model
##### Non-centered
mod_wi_nf_lme <- glmmTMB(hhsize~age+wealthindex+expenditure+wealthindex:expenditure+(1|hhid)
	, data=sim_df_wi_nf_lme
	, family=gaussian()
)
summary(mod_wi_nf_lme)

#### Binned data
binned_df_wi_nf_lme <- binfun(mod_wi_nf_lme, "age", c("wealthindex", "expenditure"), bins=10)

#### Variable effect
##### Traditional CI
pred_age_trad_wi_nf_lme <- varpred(mod_wi_nf_lme, "age", isolate=FALSE, pop.ave="none", modelname="everything")

##### Centered model matrix
pred_age_centered_wi_nf_lme <- varpred(mod_wi_nf_lme, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)

### With interaction between focal and non-focal predictor

#### Simulation
sim_df_wi_f_lme <- linearsim(nHH=nHH_obs
	, perHH=perHH
	, form=~1+x1+x2+x3+x1:x2
	, hhSD=2
	, betas=c(1.5,0.1,2,1.5,1)
	, pgausian=list(p=3,fun=rnorm, mean=c(0.2,0,0), sd=c(1,1,1))
	, pcat=list(p=0)
	, link_scale=TRUE
	, vnames=c("age", "wealthindex", "expenditure", "hhsize")
)$data
true_prop_wi_f_lme <- mean(sim_df_wi_f_lme$hhsize)
focal_prop_wi_f_lme <- mean(sim_df_wi_f_lme$age)
#### Model
##### Non-centered
mod_wi_f_lme <- glmmTMB(hhsize~age+wealthindex+expenditure+age:wealthindex+(1|hhid)
	, data=sim_df_wi_f_lme
	, family=gaussian()
)

#### Binned data
binned_df_wi_f_lme <- binfun(mod_wi_f_lme, "age", c("wealthindex", "expenditure"), bins=10)

#### Variable effect
##### Traditional CI
pred_age_trad_wi_f_lme <- varpred(mod_wi_f_lme, "age", isolate=FALSE, pop.ave="none", modelname="everything")

##### Centered
##### Centered model matrix
pred_age_centered_wi_f_lme <- varpred(mod_wi_f_lme, "age", isolate=TRUE
	, pop.ave="none", modelname = "centered mm"
)


#### Combine all predictions

true_prop_cont_lme_df <- data.frame(
	y=c(true_prop_cni_lme
		, true_prop_wi_nf_lme
		, true_prop_wi_f_lme
	)
	, .varpred=c("a) No interaction"
		, "b) Non-focal interaction"
		, "c) Focal-non-focal interaction"
	)

)
lnames <- c("a) No interaction"
	, "a) No interaction"
	, "b) Non-focal interaction"
	, "b) Non-focal interaction"
	, "c) Focal-non-focal interaction"
	, "c) Focal-non-focal interaction"
)
vlist <- list(pred_age_trad_cni_lme
	, pred_age_centered_cni_lme
	, pred_age_trad_wi_nf_lme
	, pred_age_centered_wi_nf_lme
	, pred_age_trad_wi_f_lme
	, pred_age_centered_wi_f_lme
)
pred_age_cont_lme_plots <- (comparevarpred(vlist=vlist, lnames=lnames
		, plotit=TRUE, addmarginals=TRUE
		, margindex=c(2,4,6)
		, facet_ncol=2
	)
	+ geom_hline(data=true_prop_cont_lme_df, aes(yintercept=y), lty=2, colour="yellow")
	+ scale_colour_manual(breaks = c("everything", "centered mm")
		, values=c(everything="black", "centered mm"="red")
	)
	+ labs(y="Predicted household size", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_age_cont_lme_plots)

saveEnvironment()
