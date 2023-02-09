library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

mm <- model.matrix(mod)
cm <- colMeans(mm)
bb <- coefficients(mod)

print(matrix(cm, nrow=1) %*% bb)
mean(dat$mass)

veff <- varpred(mod, "nitro", at=list(nitro=dat$nitro), returnall=TRUE)

# varpred
## center
vmm <- veff$raw$model.matrix
head(vmm)

cmm <- colMeans(vmm)

print(matrix(cmm, nrow=1) %*% bb)

## means
colMeans(vmm)

# data
## means
print(cm)

l1 <- sweep(vmm, 2, cmm, FUN="/")
l1 <- sweep(l1, 2, cm, FUN="*")
print(l1)
print(matrix(colMeans(l1), nrow=1) %*% bb)

pred <- as.vector(l1 %*% bb)
mean(pred)

head(veff$m2)
all.equal(veff$m2, vmm)
saveEnvironment()

