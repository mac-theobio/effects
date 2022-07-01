library(vareffects)
library(ggplot2)
library(scales)
set.seed(991)

xtransfun <- function(x, trans=exp) {
	pp <- x$preds
	x.var <- attr(pp, "x.var")
	pp[[x.var]] <- trans(pp[[x.var]])
	x$preds <- pp
	return(x)
}

#inverse log
invlog <- function(x){
  exp(x)
}

isq<-function(x){
  log(x)
}

invlog_trans <- trans_new(name="invlog"
	, transform=base::exp
	, inverse=base::log
)

reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv,
              log_breaks(base = base),
              domain = c(1e-100, Inf))
}

N <- 100
x1 <- sample(1:10, size=N, replace=TRUE)
x2 <- rnorm(N, 2, 1)
y <- 5 + 2*x1 + 1.5*x2 + rnorm(N)
df <- data.frame(y=y, x1=x1, x2=x2, x1s=drop(scale(x1)))
m1 <- lm(y ~ x1 + x2, data=df)
v1 <- varpred(m1, "x1", modelname="main")

m1s <- lm(y ~ x1s + x2, data=df)
v1s <- varpred(m1s, "x1s", modelname="scaled")
s1 <- scale(df$x1)
mm <- attr(s1, "scaled:center")
ss <- attr(s1, "scaled:scale")
plot(v1s, xtrans_fun=function(x, s=ss, m=mm){return(m + x*s)})


v1s$preds$x1s <- mm + v1s$preds$x1s*ss

df$x1l <- log(df$x1)
m1l <- lm(y ~ log(x1) + x2, data=df)
m1l2 <- lm(y ~ x1l + x2, data=df)
v1l <- varpred(m1l, "x1", bias.adjust="none", modelname="main")
v1l2 <- varpred(m1l2, "x1l", bias.adjust="none", modelname="main")
plot(v1l, ci=FALSE)
plot(v1l2, ci=FALSE) + scale_x_continuous(trans=invlog_trans
		, breaks=c(1,10)
)
plot(v1l2, xtrans_fun=exp)

