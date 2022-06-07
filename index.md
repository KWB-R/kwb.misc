[![R-CMD-check](https://github.com/KWB-R/kwb.misc/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.misc/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.misc/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.misc/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.misc/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.misc)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.misc)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.misc)](https://kwb-r.r-universe.dev/)

Miscellaneous functions for data analysis with R at KWB, not
yet intended for distribution. So far, functions of the following
lib-files have been imported: hsLibMiaCsoData.R,
hsLibDataAvailability.R, hsLibTimeshift.R, hsLibDataSource.R,
hsLibImpSenData.R, hsLibRainDist.R, hsLibCalib.R.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install kwb.misc in R
install.packages('kwb.misc')

# Browse the kwb.misc manual pages
help(package = 'kwb.misc')

```
