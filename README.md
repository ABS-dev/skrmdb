# skrmdb <img src="man/figures/logo.png" width="150" align="right" />

Package to estimate ED50 by the methods of Spearman-Karber, Reed-Muench, and Dragstedt-Behrens.

## To update or install skrmdb:

### From Github

From **within R**

1. Install last release

```
devtools::install_github("ABS-dev/skrmdb", ref = "v4.4.6")
```

2. Installing work-in-progress on hte main development branch

```
devtools::install_github("ABS-dev/skrmdb")
```

### For CVB employees

After setting up Rstudio to work with [Package Manager](https://ncahconnect.usda.net/CVBverse/package_manager.html), you can install the most recent version of `skrmdb` like so:

``` r
install.package("skrmdb")
```

You can install an archived version of `skrmdb` thus:

``` r
devtools::install_version('skrmdb', '0.5.0')
```

Refer to the [skrmdb overview](https://ncahrpackage.usda.net/client/#/repos/cvb-gitlab/packages/skrmdb/overview#package-details) on Package Manager to find a list of the archived versions.


## Package vignette

Read online at: https://www.aphis.usda.gov/animal_health/vet_biologics/publications/STATWI0001.pdf

## Package manual

Read online at: https://github.com/ABS-dev/skrmdb/blob/master/inst/doc/skrmdb-manual.pdf

## Issues

Report any issues or requests on the package  [issues](https://github.com/ABS-dev/skrmdb/issues) page.
