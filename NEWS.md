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
