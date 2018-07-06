## Test environments
* local windows 7 install, R 3.5.1
* debian 9.4, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* New submission

* Possibly mis-spelled words in DESCRIPTION:
  Omics (3:30)
  omics (9:126)
  
  Both are spelled correctly. First is capital because of title case.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of wilson 
(https://github.molgen.mpg.de/loosolab/wilson/tree/master/revdep). 
All packages that I could install passed.
