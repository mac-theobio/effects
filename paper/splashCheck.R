library(ggplot2); theme_set(theme_bw(base_size=14))

library(shellpipes)

print(getEnvObj("predeff", "def") + ggtitle("Default"))
print(getEnvObj("predeff", "mean") + ggtitle("mean"))

