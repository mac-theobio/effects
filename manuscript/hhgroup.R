library(dplyr)
library(tidyr)

library(shellpipes)

hhid <- 1:4
year <- 2022:2025
N <- 16

dat <- (crossing(hhid, year) 
	%>% mutate(NULL
		, age=rnorm(N, mean=0, sd=1)
	)
	%>% group_by(hhid) # hh - related RE
	%>% mutate(NULL
		, hh_re=rnorm(1, mean=0, sd=2)
	)
)

print(dat, length=N)
