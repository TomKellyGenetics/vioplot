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

Vioplotx works in much the same manner as Vioplot

* `vioplotx::vioplotx()` generates a violin plot takes the same arguments as `vioplotx::vioplotx()` to plot a violin for each group of variables.

* `vioplotx::vioplotx()` also takes additional arguments to specify `main`, `sub`, `xlab`, and `ylab` as used in `plot` or `title`

* `vioplotx::vioplotx()` can take vectorised forms of colour variables `col`, `border`, and `rectCol` to modify the colours separately for each violin respectively. This also applies to a new variable `lineCol` to modify the colour of the boxplots. 


For development history prior to package documentation, see the original repo: https://github.com/TomKellyGenetics/R-violin-plot/commits?author=TomKellyGenetics
