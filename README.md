# WIlsON: Webbased Interactive Omics visualizatioN -  The R Package
[![CRAN](https://www.r-pkg.org/badges/version/wilson)](https://cran.r-project.org/package=wilson)

[![pipeline status](https://gitlab.gwdg.de/loosolab/software/wilson/badges/master/pipeline.svg)](https://gitlab.gwdg.de/loosolab/software/wilson/commits/master)

## Abstract
#### Objective
High-throughput (HT) studies of complex biological systems generate a massive amount of so called omics data. The results are typically summarized using spreadsheet like data formats.  Visualization of this data is a key aspect of both, the analysis and the understanding of biological systems under investigation. While users have many visualization methods and tools to choose from, the challenge is to properly handle these tools and create clear, meaningful, and integrated visualizations based on pre-processed datasets.

#### Results
The WIlsON R package employs the R Shiny and Plotly web-based frameworks using a client-server based approach comprising a range of interface and plotting modules. These can be joined to allow a user to select a meaningful combination of parameters for the creation of various plot types (e.g. bar, box, line, scatter, heat). The modular setup of elements assures a concise code base and simplifies maintenance. An app thus created can be mounted on an R Shiny Server or inside R Studio. Data must be supplied server-side using a custom tab-delimited format derived from the SummarizedExperiment format (Clarion) and can principally originate from any analysis (e.g. RNA-Seq, ChIP-Seq, Mass Spectrometry, Microarray) that results in numeric data (e.g. count, score, log2foldchange, zscore, pvalue) attributed to a feature (e.g. gene, transcript, probe, protein).

#### Conclusions
The WIlsON R package includes a toolbox of R Shiny modules that can be used to construct a wide array of web-interfaces for plotting feature-based data.

## Availability
All components of the WIlsON R package have been implemented in an integrated web application that is available for download from the Github repository [wilson-apps](https://github.molgen.mpg.de/loosolab/wilson-apps/) and can be tested on our [official demonstration server](http://loosolab.mpi-bn.mpg.de/wilson).

Usage instructions can be found in the extensive [documentation](https://github.molgen.mpg.de/loosolab/wilson-apps/wiki/).

Get a Docker container [here](https://hub.docker.com/r/loosolab/wilson/).

Please make sure to check our other projects at [loosolab](http://loosolab.mpi-bn.mpg.de/).

## Organization and Philosophy
Visualizations are organized hierarchically as Shiny modules, such that larger visualizations are built from small, general, and reusable components. 

### Installation
The module source code is made available as an R package and can be installed locally with

```r
install.packages("BiocManager")
BiocManager::install("wilson")
```

On Windows, make sure that `Rtools` are available. Only versions < 2.3.1.

To enable interactive plot downloads install [Orca](https://github.com/plotly/orca).

## CLARION input format

CLARION: generiC fiLe formAt foR quantItative cOmparsions of high throughput screeNs

CLARION is a data format especially developed to be used with WIlsON, which relies on a tab-delimited table with a metadata header to describe the following columns. It is based on the Summarized Experiment format and supports all types of data which can be reduced to features and their annotation (e.g. genes, transcripts, proteins, probes) with assigned numerical values (e.g. count, score, log2foldchange, z-score, p-value). Most result tables derived from RNA-Seq, ChIP/ATAC-Seq, Proteomics, Microarrays, and many other analyses can thus be easily reformatted to become compatible without having to modify the code of WIlsON for each specific experiment.

Please check the following link for details considering the [CLARION format](https://github.molgen.mpg.de/loosolab/wilson-apps/wiki/Local-usage%3AInput-format).

## How to cite
*Schultheis H, Kuenne C, Preussner J, Wiegandt R, Fust A, Bentsen M, Looso M*. WIlsON: Webbased Interactive Omics VisualizatioN. (2018), doi: https://doi.org/10.1093/bioinformatics/bty711

## License
This project is licensed under the MIT license.
