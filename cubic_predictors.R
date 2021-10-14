library(shellpipes)
library(tidyr)
library(dplyr)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
N <- 100
b0 <- 20
b11 <- 0.1
b12 <- 0.8
b13 <- 0.3
b2 <- 0.8
x2_sd <- 1
y_sd <- 13

xx <- runif(N, -1, 1)

############################################################################################
# Cubic polynomial
############################################################################################

### 1 polynomial (focal)
### 1 non-focal predictor

#### Simulation
sim_df_cubic <- (
	data.frame(x1=rnorm(N, 0, x2_sd)
		, x2=rnorm(N, 0, x2_sd)
	)
	%>% mutate(eta = b0+b11*x1+b12*x1^2 + b13*x1^3+b2*x2
		, y=rnorm(N, mean=eta, sd=y_sd)
	)
)
head(sim_df_cubic)

plot(sim_df_cubic$x1, sim_df_cubic$eta)
saveVars(sim_df_cubic, comparevarpred)
