library(shellpipes)
library(tidyr)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
N <- 1000
b0 <- 20
age_b11 <- 0.1
age_b12 <- 0.8
age_b13 <- 0.3
b2 <- 0.8
age_sd <- 1
wealthindex_sd <- 1
hhsize_sd <- 10


############################################################################################
# Cubic polynomial
############################################################################################

### 1 polynomial (focal)
### 1 non-focal predictor

#### Simulation
sim_df_cubic <- (
	data.frame(age=rnorm(N, 0, age_sd)
		, wealthindex=rnorm(N, 0, wealthindex_sd)
	)
	%>% mutate(eta = b0 + age_b11*age + age_b12*age^2 + age_b13*age^3 + b2*wealthindex
		, hhsize=rnorm(N, mean=eta, sd=hhsize_sd)
	)
)
head(sim_df_cubic)

saveVars(sim_df_cubic, comparevarpred)
