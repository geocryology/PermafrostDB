# PermafrostDB
R package for accessing permafrost database

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
install_github("geocryology/PermafrostDB", ref="master", auth_token="<COPY YOUR AUTH TOKEN HERE>" )
```
