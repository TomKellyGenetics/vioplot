## Test environments
* Windows 10 desktop R 4.3.3
* ubuntu 14.04 (on travis-ci), R 3.6.3, 4.0.0, dev
* MacOS 10.13.6 (on travis-ci), 4.0.0
* ubuntu 14.04 (on circle-ci), R 3.6.1
* local elementary OS 0.4 (Ubuntu 16.04) install R 3.4.2
* local Windows 7 install, R 3.2.1
* Windows Server 2012 (on AppVeyor), R 4.0.1
* win-builder (devel and release)
* MacOS 18.6.0 R 4.2.0
* CentOS 7 R 4.1.2
* CentOS 7 R 4.2.1 

── R CMD check results ───────────────────── vioplot 0.5.0 ────
Duration: 59.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Updates

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

All changes expected to be backwards compatible without affecting reverse dependencies.
New experimental features are fully-documented with separate functions to
avoid problems with existing functions.

#NOTE
None.
