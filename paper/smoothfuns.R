
library(shellpipes)

## Take the log of values that are large compared to scale; smoothly return a finite value for negative numbers
safelog <- function (x, scale=1){
	return(log(scale*log(1+exp(x/scale))))
}

saveEnvironment()
