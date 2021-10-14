library(shellpipes)
library(tidyr)
library(dplyr)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
N <- 100
b0 <- 1.5
b1 <- 2
b2 <- 0.8
x2_sd <- 1


############################################################################################
# Cubic polynomial
############################################################################################

### 1 polynomial (focal)
### 1 non-focal predictor

#### Simulation
sim_df_cubic <- (
	data.frame(x1=runif(N, -1,1)^3
		, x2=rnorm(N, 0, x2_sd)
	)
	%>% mutate(eta = b0+b1*x1+b2*x2
		, y=rnorm(N, mean=eta, sd=1)
	)
	%>% select(-eta)
)
head(sim_df_cubic)

saveVars(sim_df_cubic, comparevarpred)
