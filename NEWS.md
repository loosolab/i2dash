# wilson 2.4.1
- columnSelector and Clarion bugfix
# wilson 2.4.0
- implemented text-to-selection feature for orTextual
- added several convenience functions to Clarion-class (e.g. write)
- implemented install_app function
# wilson 2.3.1
- Rtools no longer required for windows users
- Interactive plot download now uses [Orca](https://github.com/plotly/orca)
- removed i2dash until the package is published
# wilson 2.3.0
- implemented:
  - heatmap_to_i2dash
  - geneView_to_i2dash
  - scatterplot_to_i2dash
# wilson 2.2.1
- fixed orTextual selection bug
# wilson 2.2.0
- export plotting functions
# wilson 2.1.1
- fixed multiple unique_id bug in tobias_parser
# wilson 2.1.0
- implemented tobias_parser
# wilson 2.0.3
- reactive transformation parameter
# wilson 2.0.2
- fixed CRAN check Note/ Error
# wilson 2.0.1
- tests added
# wilson 2.0.0 
## Features
- clarion class:
  - easier data-format validation by providing several checks
  - simplified module usage (only forward clarion object) for top-level modules (e.g. filter & plot)
  - provide functions for frequent tasks (e.g. get_name, is_delimited, etc.)

- geneView: 
  - group columns by one or more factors

- pca:
  - color & shape grouping by selected factor(s)

- scatterPlot:
  - add name to hovertext if available (only interactive)

## Misc
- improved notifications (closable, more)
- overall code quality improvements via usage of packages goodpractice and lintr
- removed deprecated colorPicker

# wilson 1.0.0
first public release
