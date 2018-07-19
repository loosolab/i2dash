# Submission 1
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

## Reviewer comments
Thanks, we see you have lots of cuntions without examples (which may be reasonable given they are not intended t be called by users directly). 
Nevertheless, it would be good to have them tested, so please create, if examples are not feasible, at least tests that execute the various functions in your package.

Best,
Uwe Ligges

# Submission 2
## Test environments
* local windows 7 install, R 3.5.1
* debian 9.4, R 3.4.1
* win-builder (devel and release)

## Submission comments
Addressed reviewer comments by adding tests where possible.

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
