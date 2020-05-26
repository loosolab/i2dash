# Submission
## Test environments
* local windows 10 install, R 4.0.0
* debian 10, R 4.0.0 and devel
* win-builder (devel and release)

## R CMD check results
There was one NOTE and no ERRORs or WARNINGs.

NOTE
Imports includes 31 non-default packages.
Importing from so many packages makes the package vulnerable to any of them becoming unavailable. Move as many as possible to Suggests and use conditionally.

I checked all packages and none can be moved from imports.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of wilson 
(https://gitlab.gwdg.de/loosolab/software/wilson/tree/master/revdep). 
All packages that I could install passed.
