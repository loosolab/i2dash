# WIlsON: Webbased Interactive Omics visualizatioN -  The R Package

## Abstract
#### Objective
High-throughput (HT) studies of complex biological systems generate a massive amount of so called omics data. The results are typically summarized using spreadsheet like data formats.  Visualization of this data is a key aspect of both, the analysis and the understanding of biological systems under investigation. While users have many visualization methods and tools to choose from, the challenge is to properly handle these tools and create clear, meaningful, and integrated visualizations based on pre-processed datasets.

#### Results
The WIlsON R package employs the R Shiny and Plotly web-based frameworks using a client-server based approach comprising a range of interface and plotting modules. These can be joined to allow a user to select a meaningful combination of parameters for the creation of various plot types (e.g. bar, box, line, scatter, heat). The modular setup of elements assures a concise code base and simplifies maintenance. An app thus created can be mounted on an R Shiny Server or inside R Studio. Data must be supplied server-side using a custom tab-delimited format derived from the SummarizedExperiment format (Clarion) and can principally originate from any analysis (e.g. RNA-Seq, ChIP-Seq, Mass Spectrometry, Microarray) that results in numeric data (e.g. count, score, log2foldchange, zscore, pvalue) attributed to a feature (e.g. gene, transcript, probe, protein).

#### Conclusions
The WIlsON R package includes a toolbox of R Shiny modules that can be used to construct a wide array of web-interfaces for plotting feature-based data.

## Availability
All components of the WIlsON R package have been implemented in an integrated web application that is available for download from the Github repository [wilson-apps](https://github.molgen.mpg.de/loosolab/wilson-apps) and can be tested on our [official demonstration server](http://loosolab.mpi-bn.mpg.de/apps/wilson/).

Get a Docker container [here](https://hub.docker.com/r/loosolab/wilson/).

Please make sure to check our other projects at http://loosolab.mpi-bn.mpg.de/.

## Organization and Philosophy
Visualizations are organized hierarchically as Shiny modules, such that larger visualizations are built from small, general, and reusable components. 

### Installation
The module source code is made available as an R package and can be installed locally with

```r
library(devtools)
install_github("loosolab/wilson", host="github.molgen.mpg.de/api/v3")
```

## Data Format
CLARION: generiC fiLe formAt foR quantItative cOmparsions of high throughput screeNs

CLARION is a data format especially developed to be used with WIlsON, which relies on a tab-delimited table with a metadata header to describe the following columns. Most results derived from a variety of analyses can thus be easily reformatted to become compatible, without having to modify the code of WIlsON for specific experiments. For details considering CLARION please visit the Introduction pages of our [official demonstration server](http://loosolab.mpi-bn.mpg.de/apps/wilson/). 

## How to cite
* Schultheis H, Kuenne C, Preussner J, Wiegandt R, Fust A, Looso M. WIlsON: Webbased Interactive Omics VisualizatioN. Bioinformatics  (2017), doi: https://XY

## License
This project is licensed under the MIT license.
