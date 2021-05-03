# PermafrostDB
R package that provides convenience functions for interacting with a permafrost database that implements the [COLDASS schema](https://github.com/geocryology/COLDASS). This library provides wrappers to query, import and export data.

# Installation from github

## From source
```bash
git pull https://github.com/geocryology/PermafrostDB
cd PermafrostDB
Rscript -e "devtools::install()"
```

## With `install_github`

```r
library(devtools)
install_github("geocryology/PermafrostDB", ref="main")
```
