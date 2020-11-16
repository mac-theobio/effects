library(emmeans)
library(ggplot2); theme_set(theme_bw())

##
source("makestuff/makeRfuns.R") ## Will eventually be a package
startGraphics()

## two models, with and without scaled/centered focal predictor
## (centering is the important part here ...)
basemod <- lm(mpg~disp+wt,data=mtcars)
scaledmod <- lm(mpg~scale(disp)+scale(wt),data=mtcars)

## make an emmeans object and do the (weird) default plot
## Why do we need both a formula and a "keep" argument – JD
e_base <- emmeans(basemod, ~disp, cov.keep="disp", CIs=TRUE)

## Plot the emmeans object by hand
## JD: Do the tails in the middle look suspiciously flat?
## Can we compare to the base "predict" function?
## BC: can you try this?
## JD: Is it easy to supply a different (denser) range of points for x?
d_base <- as.data.frame(e_base)
pp <- (ggplot(d_base, aes(disp, emmean))
    + geom_line()
    + geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL),
    	colour=NA, alpha=0.1
	)
)

print(pp)

## FIXME: Make this work based on variable names instead of parameters names
## zero out all but focal effects
zero_vcov <- function(m, focal_params) {
    v <- vcov(m)
    focal_var <- v[focal_params,focal_params]
    v[] <- 0 ## set all to zero, preserving dims/names
    v[focal_params, focal_params] <- focal_var
    return(v)
}

## zero_vcov(basemod,"disp")
## zero_vcov(basemod,c("disp","wt"))

e_disp_base <- emmeans(basemod, ~disp, cov.keep="disp", CIs=TRUE,
	vcov. = zero_vcov(basemod, "disp")
)

## works, but "anchors" to an inappropriate point (x=0)
pp %+% as.data.frame(e_disp_base)

## If we use the scaled model it works as desired
e_disp_scaled <- emmeans(scaledmod, ~disp, cov.keep="disp", CIs=TRUE,
	vcov. = zero_vcov(scaledmod, "scale(disp)")
)

plot(e_disp_scaled)
print(pp %+% as.data.frame(e_disp_scaled))

## identical results here because wt is zero at the model center/reference point
e4 <- emmeans(scaledmod, ~disp, cov.keep="disp", CIs=TRUE,
                   vcov. = zero_vcov(scaledmod, c("scale(disp)","scale(wt)")))
pp %+% as.data.frame(e4)



