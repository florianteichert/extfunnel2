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

> Florian Teichert. extfunnel2: 
> Create Extended Funnel Plots. R package version 0.1.0. 2023. 
> <https://doi.org/10.5281/zenodo.7916484>.

## References

extfunnel package<br> <https://cran.r-project.org/package=extfunnel>
<br><br> Langan D, Higgins JPT, Gregory W, et al. Graphical
augmentations to the funnel plot assess the impact of additional
evidence on a meta-analysis. J Clin Epidemiol 2012;65:511--9.\
<https://doi.org/10.1016/j.jclinepi.2011.10.009> <br><br> Ferreira ML,
Herbert RD, Crowther MJ, et al. When is a further clinical trial
justified? BMJ 2012;345:e5913. <br> <https://doi.org/10.1136/bmj.e5913>
