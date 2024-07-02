# vioplot

## Version 0.5.0

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vioplot)](https://cran.r-project.org/package=vioplot)
[![Travis Build Status](https://travis-ci.com/TomKellyGenetics/vioplot.svg?branch=master)](https://travis-ci.com/TomKellyGenetics/vioplot)
[![CircleCI](https://circleci.com/gh/TomKellyGenetics/vioplot.svg?style=svg)](https://circleci.com/gh/TomKellyGenetics/vioplot)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TomKellyGenetics/vioplot?branch=master&svg=true)](https://ci.appveyor.com/project/TomKellyGenetics/vioplot)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/TomKellyGenetics/vioplot/branch/master/graph/badge.svg)](https://codecov.io/gh/TomKellyGenetics/vioplot)
[![GitHub Views](http://hits.dwyl.com/tomkellygenetics/vioplot.svg)](http://hits.dwyl.com/tomkellygenetics/vioplot)
[![Downloads](https://cranlogs.r-pkg.org/badges/vioplot)](https://CRAN.R-project.org/package=vioplot)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/vioplot?color=orange)](https://CRAN.R-project.org/package=vioplot)

### Violin Plots in R

This package allows extensive customisation of violin plots. 

## Installation

To get the current released version from CRAN:

```R
install.packages("vioplot")
```

To get the development version from github:

```R
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/vioplot", ref = "dev")
```

## Running

See the relevant vignette for more details:

* Customising colour and shape with scalar inputs or vectors applied separately to each violin.

https://rawgit.com/TomKellyGenetics/vioplot/vignettes/vignettes/violin_customisation.html

* Formula input enabled with S3 methods.

https://rawgit.com/TomKellyGenetics/vioplot/vignettes/vignettes/violin_formulae.html

* Control of violin area for proportional widths

https://rawgit.com/TomKellyGenetics/vioplot/vignettes/vignettes/violin_area.html

* Control of the y-axis including disabling labels and log-scale

https://rawgit.com/TomKellyGenetics/vioplot/vignettes/vignettes/violin_ylog.html

* Split violins to directly compare paired data.

https://rawgit.com/TomKellyGenetics/vioplot/vignettes/vignettes/violin_split.html

## Functionality

vioplot (0.3)  is backwards-compatible with vioplot (0.2). The following features are supported:

* `vioplot()` generates a violin plot by plotting a violin for each group of variables.

* `vioplot()` also takes additional arguments to specify `main`, `sub`, `xlab`, and `ylab` as used in `plot` or `title`. Graphical parameters can be passed to plotting parameters.

* `vioplot()` can take vectorised forms of colour variables `col`, `border`, and `rectCol` to modify the colours separately for each violin respectively. This also applies to a new variable `lineCol` to modify the colour of the boxplots. 

* `vioplot.formula()` is enabled to take formula and dataframe inputs as used for boxplot and stats operations. The default axes labels are the variable names used for the formula and names are factor levels.

* additional `areaEqual`, `plotCentre` and `side` options enables further customisation. 

See the NEWS.md file for more detail on added features in the 0.3 release.

## Development and sources

For development history of version 0.3.0 prior to package documentation, see the original repo: https://github.com/TomKellyGenetics/R-violin-plot/commits?author=TomKellyGenetics

Modifications inspired by the following StackOverFlow threads and GitHub Gists:

* http://stackoverflow.com/questions/14975853/how-can-i-create-violin-plot-in-different-colours

* http://stackoverflow.com/questions/19416768/vioplot-r-how-to-set-axis-labels

* http://stackoverflow.com/questions/20250063/axis-titles-on-vioplot

* http://stackoverflow.com/questions/22410606/violin-plot-with-list-input

* https://gist.github.com/mbjoseph/5852613

### Attribution

This repository is a proposed submission for an updated version of the vioplot originally released by Daniel Adler (University of Göttingen, Germany) on CRAN. This package has been orphaned on CRAN and is no longer actively maintained. I acknowledge with contributions of Daniel Adler as the original developer and Tom Elliot (University of Auckland, New Zealand) for a pull request and welcome further contributions to improve or maintain this package. 

This package update was developed and released open-source (in accordance with the original package BSD License) while as a PhD candidate at the University of Otago (Dunedin, New Zealand). I can be contacted at my present address and affiliation is (RIKEN Centre for Integrative Medical Sciences, Yokohama, Japan) at <tom.kelly[at]riken.jp>.

### Citation

The following information can be retrieved from within an R session by using `citation(vioplot)`. Please acknowledge as follows if features included in this version are used.

To cite the enhanced vioplot package in publications use:

  Daniel Adler and S. Thomas Kelly (2022). vioplot: violin plot. R package version 0.5.0
  https://github.com/TomKellyGenetics/vioplot

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {vioplot: violin plot},
    author = {Daniel Adler, S. Thomas Kelly, Tom Elliot, and Jordan Adamson},
    year = {2022},
    note = {R package version 0.5.0},
    url = {https://github.com/TomKellyGenetics/vioplot},
  }

Please also acknowledge the original package: citation("vioplot")


