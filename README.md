# PermafrostDB
R package that provides convenience functions for interacting with the Carleton [permafrost database](https://github.com/geocryology/COLDASS). This library provides wrappers to query, import and export data.

# Installation from github

## From source
```bash
git pull https://github.com/geocryology/PermafrostDB
cd PermafrostDB
Rscript -e "devtools::install()"
```

## With `install_github`
Generate a personal authentication token at [https://github.com/settings/tokens](https://github.com/settings/tokens)
```r
library(devtools)
install_github("geocryology/PermafrostDB", ref="master")
```
