# Submission
## Test environments
* local windows 10 install, R 3.6.3 and devel
* debian 10, R 3.6.3
* win-builder (devel and release)

## R CMD check results
There were no NOTEs or WARNINGs.

With r-devel there is an Error arising in the ComplexHeatmap package but as this is fixed in their github the Error will be resolved with the next update.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of wilson 
(https://gitlab.gwdg.de/loosolab/software/wilson/tree/master/revdep). 
All packages that I could install passed.
