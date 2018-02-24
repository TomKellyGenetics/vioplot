# vioplot 0.3 (2018)

Violin plots have been considerably improved with backwards-compatibility with vioplot 0.2 (2005).

##Changes to defaults

* Note that the default colour is a neutral grey50 instead for bright magenta. This is the only change in functionality of the default function.

##Added features

* Additional colour and shape (pch) arguments to customise the appearance of violin plots

* Vectorised colour and shape inputs. Separate specifications can be given for each violin as vector inputs. Scalar inputs are applied across all violins.

* Formula inputs (as used for boxplot and beanplot) are now compatible with vioplot

* The y-axis can be customised. This includes the use of log-scales or removing axis labels.

* Titles and axis labels can be used and are passed to plot. These can be resized with cex parameters.

* Violins can be scaled so that they have equal area rather than width

* Violins can be split and plotting side-by-side sequentially

##Added Vignettes

* These demonstrate these added features in addition to them being documented.

##Added Tests

* Unit tests are performed for each added feature.
