## 4th resubmission 

This is the third resubmission. In this version, we have:

* Fixed the grapes-greater-than-grapes.Rd: \value and grapes-less-than-greater-than-grapes.Rd: \value
* Replaced \dontrun with \donttest.
* Omitted any default path in writing functions in examples/vignettes/tests and changed to tempdir().

## 3nd resubmission 

This is the third resubmission. In this version, we have:

* Fixed the package names by adding single quotes
* Added 'cph' tag to Authors@R field
* Added small executable examples to the Rd-files
* Fixed missing \value of .Rd files

## 2nd resubmission 

This is the second resubmission. In this version, we have:

* Fixed the file `LICENSE` to contain only year and copyright holder
* Fixed a broken link to shiny in the `vignette/i2dash-intro.Rmd` file
* Fixed / Removed an invalid URI in `inst/CITATION`

## Resubmission

This is a resubmission. In this version I have:

* Fixed the test

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Arsenij Ustjanzew <arsenij.ustjanzew@mpi-bn.mpg.de>'
  
  New submission
  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  
  File 'LICENSE':
    MIT License
    
    Copyright (c) 2019 Mario Looso, Jens Preussner and Arsenij Ustjanzew
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking installed package size ... NOTE
    installed size is  5.6Mb
    sub-directories of 1Mb or more:
      doc   5.3Mb

0 errors √ | 0 warnings √ | 2 notes x

* This is a new release.
