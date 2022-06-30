library(vareffects)

N <- 100
x1 <- sample(1:10, size=N, replace=TRUE)
x2 <- rnorm(N, 2, 1)
y <- 5 + 2*x1 + 1.5*x2 + rnorm(N)
df <- data.frame(y=y, x1=x1, x2=x2, x1s=drop(scale(x1)))
m1 <- lm(y ~ x1 + x2, data=df)
v1 <- varpred(m1, "x1", modelname="main")
plot(v1, ci=FALSE)

m1s <- lm(y ~ x1s + x2, data=df)
v1s <- varpred(m1s, "x1s", modelname="scaled")
plot(v1s, ci=FALSE)

s1 <- scale(df$x1)
mm <- attr(s1, "scaled:center")
ss <- attr(s1, "scaled:scale")

v1s$preds$x1s <- mm + v1s$preds$x1s*ss
plot(v1s)

