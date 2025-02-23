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

── R CMD check results ───────────────────── 
──── vioplot ────
✔ | F W  S  OK | Context
✔ |      7   0 | color custom and vectorised                 
✔ |      5   0 | formula input                               
✔ |      5   0 | controlling area                            
✔ |      4   0 | different input classes                     
✔ |      7   0 | color custom and vectorised                 
✔ |      5   0 | formula input                               
✔ |      4   0 | side option                                 
✔ |      5   0 | NA handling for vector or formula input     
✔ |      2   0 | names input                                 
✔ |      1   0 | side option                                 
✔ |      2   0 | unequal group size                          
✔ |     13   0 | log-scale                                   
✔ |     17   0 | log-scale [1.8s]                            

══ Results ══════════════════════════════════════════════════
Duration: 7.9 s
──── 0.5.1 ────


0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Updates

Documentation

- updates examples for adding annotation labels using unlist() with deprecated behaviours

Bug fixes

- avoids error in CRAN checks on development version 

#NOTE

Fixes issued notified to maintainer on 2025-02-23
> Please correct before 2025-03-09 to safely retain your package on CRAN.

No changes to source code or package functions. Documentation updated for compliance.
