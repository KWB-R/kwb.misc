[![R-CMD-check](https://github.com/KWB-R/kwb.misc/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.misc/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.misc/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.misc/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.misc/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.misc)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.misc)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.misc)](https://kwb-r.r-universe.dev/)

# kwb.misc

Miscellaneous functions for data analysis with R at KWB, not
yet intended for distribution. So far, functions of the following
lib-files have been imported: hsLibMiaCsoData.R,
hsLibDataAvailability.R, hsLibTimeshift.R, hsLibDataSource.R,
hsLibImpSenData.R, hsLibRainDist.R, hsLibCalib.R.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.misc' from GitHub
remotes::install_github("KWB-R/kwb.misc")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.misc](https://kwb-r.github.io/kwb.misc)

Development: [https://kwb-r.github.io/kwb.misc/dev](https://kwb-r.github.io/kwb.misc/dev)
