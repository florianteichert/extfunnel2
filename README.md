[![DOI](https://zenodo.org/badge/632107602.svg)](https://zenodo.org/badge/latestdoi/632107602)

# extfunnel2

Inspired by the archived
<a href="https://cran.r-project.org/package=extfunnel">extfunnel</a>
package by
<a href="https://doi.org/10.1016/j.jclinepi.2011.10.009">Langan et
al</a>. The function creates an extended funnel plot with shaded
contours that show the impact of a new study with a certain effect
estimate and standard error (or sample size) on the conclusions of an
updated meta-analysis. Uses ggplot2 instead of base R, allows
specification of the meta-analytic model as well as simulation using
sample size per group in addition to the standard error.


## Installation

`extfunnel2` is not on CRAN. You can install the latest version from
GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("florianteichert/extfunnel2")
```

## Examples

``` r
library(extfunnel2)
?extfunnel2
```

## How to cite this package in your article

> Teichert, F. (2023). extfunnel2: 
> Create Extended Funnel Plots. R package version 0.1.0.
> <https://doi.org/10.5281/zenodo.7916484>

## Please also cite
> Langan, D., Higgins, J. P. T., Gregory, W., & Sutton, A. J. (2012). 
> Graphical augmentations to the funnel plot assess the impact of 
> additional evidence on a meta-analysis. 
> Journal of Clinical Epidemiology, 65(5), 511–519. 
> <https://doi.org/10.1016/j.jclinepi.2011.10.009>


## References

1. extfunnel package. <https://cran.r-project.org/package=extfunnel> 
<br><br>
2. Langan, D., Higgins, J. P. T., Gregory, W., & Sutton, A. J. (2012). Graphical augmentations to the funnel plot assess the impact of additional evidence on a meta-analysis. Journal of Clinical Epidemiology, 65(5), 511–519. <https://doi.org/10.1016/j.jclinepi.2011.10.009> 
<br><br> 
3. Ferreira, M. L., Herbert, R. D., Crowther, M. J., Verhagen, A., & Sutton, A. J. (2012). When is a further clinical trial justified? BMJ , 345, e5913. <https://doi.org/10.1136/bmj.e5913>
