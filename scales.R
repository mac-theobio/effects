
source("makestuff/makeRfuns.R") ## Will eventually be a package
commandEnvironments() ## Read in any environments specified as dependencies

m <- lm(y1 ~ x1+x2, data=sim_df)
vcov(m)

mc <- lm(y1 ~ x1c+x2c, data=sim_df)
vcov(mc)

ms <- lm(y1 ~ x1s+x2s, data=sim_df)
vcov(ms)

mstd <- lm(y1 ~ x1std+x2std, data=sim_df)
vcov(mstd)


