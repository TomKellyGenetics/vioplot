##First submission

This is my first submission of this package, although it is based on the existing vioplot (0.2) package.

Since this package has been orphaned since 2005 (12 years to date), I propose to take over as maintainer. I have continued to develop this a fork of this package under the repository: https://github.com/TomKellyGenetics/vioplotx

Additional features have been added with backwards-compatibility with vioplot 0.2 and added compatibility with formula inputs (as used for beanplot and boxplot).

## Test environments
* local elementary OS 0.4 (Ubuntu 16.04) install R (3.4.2)
* local Windows 7 install, R 3.2.1
* ubuntu 14.04 (on travis-ci), R 3.4.2
* Windows Server 2012 (on AppVeyor), R 3.4.3
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 3 NOTES:

#NOTES

##Package size

Large directory is due to the lengthy Vignettes. These are essential to understanding the range of added features compared to vioplot 0.2.

##License

Deprecated License has been kept to conform with License of the orphaned vioplot (0.2) package released by Daniel Adler <dadler@uni-goettingen.de>. 

##attach

Is needed to process formula input. The input data is detached within the function.
