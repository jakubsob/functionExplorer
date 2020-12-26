
<!-- README.md is generated from README.Rmd. Please edit that file -->

# functionExplorer

<!-- badges: start -->

[![R build
status](https://github.com/jakubsob/functionExplorer/workflows/R-CMD-check/badge.svg)](https://github.com/jakubsob/functionExplorer/actions)
[![license](https://img.shields.io/badge/license-mit-lightgrey.svg)](https://choosealicense.com/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/jakubsob/functionExplorer/branch/master/graph/badge.svg)](https://codecov.io/gh/jakubsob/functionExplorer?branch=master)
[![Travis build
status](https://img.shields.io/travis/com/jakubsob/functionExplorer.svg?logo=travis)](https://travis-ci.com/jakubsob/functionExplorer)
<!-- badges: end -->

A Shiny app for exploring dependencies between functions from selected
GitHub repository. Uses
<a href = "https://github.com/Appsilon/shiny.semantic">shiny.semantic</a>.

## Installation

Install from GitHub:

``` r
devtools::install_github("jakubsob/functionExplorer")
```

and run:

``` r
functionExplorer::run_app()
```

## User Interface

The app opens to the following interface:

<div class="figure">

<img src="man/figures/README-load-data.png" alt="Start page of app." width="100%" />

<p class="caption">

Start page of app.

</p>

</div>

To show tutorial on how to use click question mark button in top right
corner.

To download data use download button to open modal and fill in name of
repository to download:

<div class="figure">

<img src="man/figures/README-load-data-popup.png" alt="Data downloading popup." width="100%" />

<p class="caption">

Data downloading popup.

</p>

</div>

After download use button to parse data:

<div class="figure">

<img src="man/figures/README-downloaded-data.png" alt="Tables with loaded and parsed data." width="100%" />

<p class="caption">

Tables with loaded and parsed data.

</p>

</div>

Go to graph tab to view plotted network:

<div class="figure">

<img src="man/figures/README-plot-all.png" alt="Function dependencies network." width="100%" />

<p class="caption">

Function dependencies network.

</p>

</div>

Select node based on value from select input in sidebar or by clicking
node on graph:

<div class="figure">

<img src="man/figures/README-plot-node.png" alt="Function dependencies network with selected node." width="100%" />

<p class="caption">

Function dependencies network with selected node.

</p>

</div>
