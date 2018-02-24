[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vioplotx)](https://cran.r-project.org/package=vioplotx)
[![Build Status](https://travis-ci.org/TomKellyGenetics/vioplotx.svg?branch=master)](https://travis-ci.org/TomKellyGenetics/vioplotx)


To get the current released version from CRAN:

```R
install.packages("vioplot")
```

To get the modified version from github:

```R
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/vioplotx")
```

# Running

See the relevant vignette for more details:

* Customising colour and shape with scalar inputs or vectors applied separately to each violin.

https://rawgit.com/TomKellyGenetics/vioplotx/master/vignettes/violin_customisation.html

* Formula input enabled with S3 methods.

https://rawgit.com/TomKellyGenetics/vioplotx/master/vignettes/violin_formulae.html

* Control of violin area for proportional widths

https://rawgit.com/TomKellyGenetics/vioplotx/master/vignettes/violin_area.html

* Control of the y-axis including disabling labels and log-scale

https://rawgit.com/TomKellyGenetics/vioplotx/master/vignettes/violin_ylog.html

* Split violins to directly compare paired data.

https://rawgit.com/TomKellyGenetics/vioplotx/master/vignettes/violin_split.html

Vioplotx works in much the same manner as Vioplot

* `vioplotx::vioplotx()` generates a violin plot takes the same arguments as `vioplotx::vioplotx()` to plot a violin for each group of variables.

* `vioplotx::vioplotx()` also takes additional arguments to specify `main`, `sub`, `xlab`, and `ylab` as used in `plot` or `title`

* `vioplotx::vioplotx()` can take vectorised forms of colour variables `col`, `border`, and `rectCol` to modify the colours separately for each violin respectively. This also applies to a new variable `lineCol` to modify the colour of the boxplots. 

* `vioplox::vioplotx.formula()` is enabled to take formula and dataframe inputs as used for boxplot and stats operations.

* additional `areaEqual`, `plotCentre` and `side` options enables further customisation. 

For development history prior to package documentation, see the original repo: https://github.com/TomKellyGenetics/R-violin-plot/commits?author=TomKellyGenetics

Modifications inspired by the following StackOverFlow threads:

* http://stackoverflow.com/questions/14975853/how-can-i-create-violin-plot-in-different-colours

* http://stackoverflow.com/questions/19416768/vioplot-r-how-to-set-axis-labels

* http://stackoverflow.com/questions/20250063/axis-titles-on-vioplot

* http://stackoverflow.com/questions/22410606/violin-plot-with-list-input
