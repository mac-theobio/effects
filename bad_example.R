library(shellpipes)
library(ggplot2)

## Use this call to make bad_example.Rout independently
rpcall("bad_example.Rout bad_example.R")

commandEnvironments()
startGraphics()

set.seed(9991)

## Simple model with interaction on the non-focal predictors.
## We expect predictions to work in this case but that's not the
## case :(

N <- 20

beta0 <- 1.5
beta1 <- 1.0
beta2 <- 2
beta3 <- 1.5
beta23 <- 1
betas <- c(beta0, beta1, beta2, beta3, beta23)

x1_sd <- 1
x1_mean <- 0.2

x2_sd <- 1
x2_mean <- 0

x3_sd <- 1
x3_mean <- 0

form <- ~ 1 + x1 + x2 + x3 + x2:x3

x1 <- rnorm(N, x1_mean, x1_sd)
x2 <- rnorm(N, x2_mean, x2_sd)
x3 <- rnorm(N, x3_mean, x3_sd)
df <- data.frame(x1=x1, x2=x2, x3=x3)
X <- model.matrix(form, df)
df$y <- rnorm(N, as.vector(X %*% betas), 1)

## Model

mod <- lm(y ~ x1 + x2 + x3 + x2:x3, data=df)
summary(mod)

## Simple predictions
true_y <- mean(df$y)
print(true_y)

quants <- seq(0, 1, length.out=100)
new_df <- data.frame(x1 = as.vector(quantile(df$x1, quants))
	, x2 = mean(df$x2)
	, x3 = mean(df$x3)
)

preds <- predict(mod, newdata=new_df)

preds_df <- data.frame(x1=new_df$x1, y=preds)

print(ggplot(preds_df, aes(x=x1, y=y))
 + geom_line()
 + geom_hline(aes(yintercept=true_y, colour="truth"), lty=2)
 + geom_hline(aes(yintercept=mean(y), colour="pred"), lty=2)
 + geom_vline(aes(xintercept=mean(df$x1)), colour="grey", lty=2)
)
