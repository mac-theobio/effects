
set.seed(1011)

b_0 <- 6
b_1 <- 5

x <- scale(rnorm(100))
ell <- b_0 + b_1*x

m_l <- mean(ell)
s_l <- sd(ell)
unc = plogis(mean(ell))
ellpp <- unc^2*exp(-m_l)*(2*exp(-m_l)*unc - 1)

mean = mean(plogis(ell))
cor = unc + ellpp*s_l^2/2
mean
unc
cor

print(c(NULL
	, delta = mean - unc
	, delta_2 = cor-unc
))

quit()

l = exp(x)/(1+exp(x))
  = 1/(1+exp(-x)) = 1/s

l' =  -s'/s^2
	= exp(-x) l^2

l'' = 2ll' exp(-x) - exp(-x) l^2
    = l exp(-x) (2l' - l)
    = l^2 exp(-x)(2 exp(-x) l - 1)
