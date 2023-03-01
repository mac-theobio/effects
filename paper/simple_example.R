library(varpred)
library(dplyr)
library(ggplot2)

# Global variables
N <- 100
b0 <- 20
age_b11 <- 0.1
age_b12 <- 2
b2 <- 0
age_sd <- 1
wealthindex_sd <- 1
hhsize_sd <- 10

df <- (
		data.frame(age=rnorm(N, 0, age_sd)
			, wealthindex=rnorm(N, 0, wealthindex_sd)
		)
		%>% mutate(
			eta = b0 + age_b11*age + age_b12*age^2 + b2*wealthindex
				, hhsize=rnorm(N, mean=eta, sd=hhsize_sd)
		)
)

## model center
center_df <- (df
	%>% summarize_all(mean)
)

## Model
mod <- lm(hhsize ~ poly(age, degree=2) + wealthindex
	, data=df
)
summary(mod)

## Varpred
pred <- varpred(mod, "age")

print(plot(pred)
	+ geom_point(data=center_df, aes(x=age, y=hhsize, size=3), colour="grey")
	+ geom_hline(yintercept=mean(pred$preds$fit), colour="red")
)
