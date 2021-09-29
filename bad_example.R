library(vareffects)
library(shellpipes)
library(dplyr)
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
x1_new <- as.vector(quantile(df$x1, quants))
new_df <- data.frame(x1 = x1_new 
	, x2 = mean(df$x2)
	, x3 = mean(df$x3)
)

preds <- predict(mod, newdata=new_df)
preds_df <- data.frame(x1=new_df$x1, y=preds)

mm <- model.matrix(mod)
col_mean <- colMeans(mm)
mm_mean <- t(replicate(length(x1_new), col_mean))
mm_mean[, "x1"] <- x1_new

pred2 <- as.vector(mm_mean %*% coef(mod))
preds_df2 <- data.frame(x1=x1_new, y=pred2)

print(ggplot(preds_df, aes(x=x1, y=y))
 + geom_line()
 + geom_line(data=preds_df2, aes(x=x1, y=y))
 + geom_hline(aes(yintercept=true_y, colour="truth"), lty=2)
 + geom_hline(aes(yintercept=mean(y), colour="pred-pred"), lty=2)
 + geom_hline(aes(yintercept=mean(preds_df$y), colour="pred-var"), lty=2)
 + geom_vline(aes(xintercept=mean(df$x1)), colour="grey", lty=2)
)


#### Categorical
N <- 200
b0 <- 1.5
b_x1 <- 0.8
b_x2b <- 0.1
cat_props <- c(0.4, 0.6)
df <- data.frame(x1 = rnorm(N, 0.2, 1)
	, x2 = sample(c("Female", "Male"), N, prob = cat_props, replace = TRUE)
)
X <- model.matrix(~1 + x1 + x2, df)
betas <- c(b0, b_x1, b_x2b)
df$y <- rnorm(nrow(df), mean= X %*% betas, sd=1)
head(df)

focal_prop_catni <- (df
	%>% group_by(x2)
	%>% summarize(y=mean(y))
)
focal_prop_catni

mod <- lm(y ~ x1 + x2, data=df)
p1 <- varpred(mod, "x2")
plot(p1) + geom_point(data=focal_prop_catni, aes(x=x2, y=y), colour="red")

x1_new <- unique(df$x2)
mm <- model.matrix(mod)
col_mean <- colMeans(mm)
mm_mean <- t(replicate(length(x1_new), col_mean))

pred2 <- as.vector(mm_mean %*% coef(mod))
preds_df2 <- data.frame(x1=x1_new, y=pred2)

preds_df2

steps <- length(x1_new)

rowMean <- apply(mm, 2, mean)
modMean <- t(replicate(steps, rowMean))
mf <- model.frame(mod)

# Model matrix with progression of focal variable
varFrame <- mf[rep(1, steps), ]
varFrame["x2"] <- x1_new
varFrame <- model.frame(terms(mod), varFrame, drop.unused.levels = TRUE)
newMat <- model.matrix(terms(mod), varFrame)

## Better way to do this?
modVar <- modMean
focal_cols <- grep("x2", colnames(mm), value = TRUE)
inter_cols <- focal_cols[grepl(":", focal_cols)]
modVar[, focal_cols] <- newMat[, focal_cols]
modVar[, inter_cols] <- modMean[,inter_cols]*2
modVar[1, inter_cols] <- 0

pred2 <- as.vector(modVar %*% coef(mod))
preds_df2 <- data.frame(x1=x1_new, y=pred2)

preds_df2

p1
quit()

############################################################################################
# Global variables
N_obs <- 100
gender_prop <- c(0.4, 0.6)
gender_levels <- c("Female", "Male")

############################################################################################
# Section: 4.4.2 Categorical predictors
############################################################################################

### No interaction model

#### Simulation
sim_catni <- linearsim(N=N_obs, form=~1+x1+x2
	, addvars=rnorm(N_obs, 0.2, 1)
	, betas=c(1.5, 0.8, 0.1)
	, pcat=2, catprop=list(x2=gender_prop)
	, catlevels=list(x2=gender_levels)
	, link_scale = TRUE
)
sim_df_catni <- sim_catni$data
head(sim_df_catni)
table(df$x2)
table(sim_df_catni$x2)
focal_prop_catni <- (sim_df_catni
	%>% group_by(x2)
	%>% summarize(y=mean(y))
)
focal_prop_catni
mod <- lm(y ~ x1 + x2, data=sim_df_catni)
p1 <- varpred(mod, "x2")
plot(p1) + geom_point(data=focal_prop_catni, aes(x=x2, y=y), colour="red")
