# **i2dash**: - **I**nteractive & **i**terative **dash**boards in R <img src="vignettes/images/i2dash_logo.png" align="right" width="150px"/>


## Abstract

Scientific communication and data visualization are important aspects to illustrate complex concepts and results from data analyses. The R package **i2dash** provides functionality to create customized, web-based dashboards for data presentation, exploration and sharing. **i2dash** integrates easily into existing data analysis pipelines and can organize scientific findings thematically across different pages and layouts.

### Main features

- Easy integration into existing analysis pipelines in R
- Support for multiple components, such as htmlwidgets, tabular data, text, images etc.
- Creation of web-based, sharable, static or interactive dashboards
- Enables a flexible and iterative cycle of dashboard development

![](man/figures/i2dash_image.jpg)

*A customized dashboard can be integrated into an existing data analysis pipelines in R (left). After initialization, pages containing components with customized content can be added step-by-step to the dashboard at any stage of the data analysis. The final dashboard is assembled into an R markdown file, and shared together with RDS data files for further use within RStudio, or can also be deployed on a R Shiny Server or as stand-alone HTML file.*

## Installation:

The package can be installed with the `r BiocStyle::CRANpkg("remotes")` library:

```{r, eval=FALSE}
remotes::install_git(url = "https://gitlab.gwdg.de/loosolab/software/i2dash.git", repos = BiocManager::repositories())
```

## Where to start

-  [**Basics**]() This tutorial gives on overview over the core functions of **i2dash** and explains how you can build your first dashboard.

## Extension

It is possible to extend the core functionality of **i2dash** with templates for components and pre-defined pages. This enables to provide an enhanced user interactivity e.g. dynamic change of plot settings. Further, extensions allow an easier integration of complex calculations and data manipulation, hidden behind functions.

- [**i2dash.scrnaseq**](https://gitlab.gwdg.de/loosolab/software/i2dash.scrnaseq) enables an enhanced user interactivity and contains simple but effective tools for the creation of an i2dashboard with focus on single-cell RNA-sequencing data visualization and exploration.

## How to cite

Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.

## License

This project is licensed under the MIT license.
