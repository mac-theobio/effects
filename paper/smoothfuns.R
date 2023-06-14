
library(shellpipes)

## Take the log of values that are large compared to scale; smoothly return a finite value for negative numbers
## Vectorized by hand; bad?, efficient? Rabbit hole!
safelog <- function (x, scale=1, thresh=25){
	if (scale <= 0) stop("safelog: scale must be positive")
	y <-log(scale*log(1+exp(x/scale)))
	pLarge <- x > scale*thresh
	nLarge <- x < -scale*thresh
	y[pLarge] <- (log(x[pLarge]))
	y[nLarge] <- log(scale*exp(x[nLarge]/scale))
	return(y)
}

## Testing

safelog(exp(2))
safelog(exp(-2))

## How smooth are we?
safelog(2.75, 0.1)
safelog(2.75, 0.1)-safelog(2.75, 0.1, thresh=30)

## This one is a bigger difference than I like, but at a fairly weird limit
safelog(-2.75, 0.1)
safelog(-2.75, 0.1)-safelog(-2.75, 0.1, thresh=30)

## try(safelog(3, -1))

saveEnvironment()
