# vareffects

Visualize variable effects for linear models 

## Install package

```r
while (!require("remotes")) {
    install.packages("remotes")
}
remotes::install_github("mac-theobio/effects")
## also need varpred for this ...
remotes::install_github("cygubicko/varpred")
```

**or**

clone this repo and run `make install`

## Make manuscript

clone repo

```bash
make Makefile
make pullall
cd manuscript
make <msver>.pdf
```

where <msver> is “draft” for thesis-chapter draft and “draft.paper” for new MS draft 2022 Aug 10 (Wed)
