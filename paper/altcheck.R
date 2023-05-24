library(dplyr)

library(shellpipes)

loadEnvironments()

summary(dat)

dat <- (dat
	%>% mutate(NULL
		, robustProp = status/wt
		, numTest = wt
	)
)

saveEnvironment()

objects()
