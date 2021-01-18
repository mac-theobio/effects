library(dplyr)
library(jdeffects)
library(shellpipes)

jdtheme()
startGraphics()

set.seed(233)

beta_int = 3
beta_dep = 7
sig = 12
dmean = 5

dep <- runif(100)
eps <- runif(100)

ind <- beta_dep*dep + beta_int + sig*eps

## Unscaled
m_un <- lm(ind~dep)
pvp_un <- varpred(m_un, "dep")
plot(pvp_un)

## Centered dep
dep_c <- scale(dep, scale=FALSE)
m_c <- lm(ind~dep_c)
pvp_c <- varpred(m_c, "dep_c")
plot(pvp_c)

## Plot them on one axis
pvp_df <- (bind_rows(
	(pvp_un$preds
		%>% data.frame()
		%>% rename("xvar"=dep)
		%>% mutate(model="dep_un")) 
	, (pvp_c$preds
		%>% data.frame()
		%>% rename(xvar=dep_c)
		%>% mutate(model="dep_c"))
	)
)
head(pvp_df)

### Make pvp jdeffects object and use plot method
class(pvp_df) <- c("jdeffects", "data.frame")
plot(pvp_df)
