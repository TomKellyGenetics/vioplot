# vioplot 0.5.0 (2024)

New features (pilot release with limited API and documentation)

- adds function to annotate sample size per group
- adds function to plot outliers  by standard deviation magnitude threshold

Updates to Violin plot

- enable angled axis labels with srt.axis
- pass axis aesthetic parameters from formula input

Documentation

- add demo of histogram plots (#19 by @Jadamso)
- updated vignettes for split histograms (#19 by @Jadamso) 
- expands vignettes for annotated violins and annotated split violins

Bug fixes

- formula input detects group names but avoids overlapping axis labels

# vioplot 0.4.0 (2022)

New feature

- adds feature for histograms in dedicated function
discussed on GitHub issue #15 and PR #18

Documentation

- adds vignette for histograms #18

- adds examples to overlay information with base R graphics
discussed in issues #16 and #17 on GitHub

- updates documentation for h parameter #14

Bug fixes

- allow suppression of y-axes with `yaxt = 'n'` without disabing x-axes
(should be independent parameter). Resolves unexpected behaviour reported
on GitHub issue #16.

- allow NA values when plotting repeated values
resolves bug in #13 commit bd68db3c10ee5b8a550568f449fecd1d47a62197

# vioplot 0.3.7 (2021)

Updates maintainer contact details.

# vioplot 0.3.6 (2021)

Bug fixes.

- allow plotting repeated non-unique values over threshold number with checks #13

# vioplot 0.3.5 (2020)

Bug fixes.

- allow reuse of vector inputs

- correct graphical paramters: xaxt, xlim

- correct log scales (xlog) for horizontal violins

- document axes labels for split violins

# vioplot 0.3.4 (2019)

Bug fixes.

- avoids altering base plotting parameters `par()`

- resolves issues calling log inputs without an explicit `log` parameter as text

# vioplot 0.3.3 (2019)

Minor release with improvements to passing parameters.

- improved passing of base R plotting parameters

- resolves issues with variable names and factor levels in formula inputs

# vioplot 0.3.2 (2019)

Minor release with improvements to passing parameters.

- improved handling for formula input: levels for names and variable names for axes labels

- improved passing of graphical parameters to title, and axis

- axes for log-scale are automatically generated and horizontal plots are supported

Examples for formula input added for convenience (this method is recommended).

# vioplot 0.3.1 (2019)

Minor release with continuous integration testing, improved vignettes, and License. Compatible with GitHub and CRAN Release.

# vioplot 0.3.0 (2018)

## Major changes

- formula inputs

vioplot is now compatible with all inputs of boxplot or beanplot, including formula inputs (implemented as S3 methods).

- plot customisation

Various features of violins can be tweaked with plotting parameters, such as colours and shapes of aspects of the violin. These can be applied to all violins with a single (scalar) input or applied separately to each violin with multiple (vector) inputs 

- defaults

This version is fully compatible with inputs to vioplot 0.2. The only difference in behaviour changing the default colour from a glaring magenta to a monochrome grey (more appropriate in a wider range of professional settings). Code written for previous versions should run without breaking or changes in behaviour apart from the default colour.
