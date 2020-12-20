
<!-- README.md is generated from README.Rmd. Please edit that file -->

# functionExplorer

<!-- badges: start -->

[![R build
status](https://github.com/jakubsob/functionExplorer/workflows/R-CMD-check/badge.svg)](https://github.com/jakubsob/functionExplorer/actions)
[![license](https://img.shields.io/badge/license-mit-lightgrey.svg)](https://choosealicense.com/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

An app for exploring dependencies between functions from selected GitHub
repository.

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

<div class="figure">

<img src="man/figures/README-load-data.png" alt="Start page of app." width="100%" />
<p class="caption">
Start page of app.
</p>

</div>

<div class="figure">

<img src="man/figures/README-load-data-popup.png" alt="Data downloading popup." width="100%" />
<p class="caption">
Data downloading popup.
</p>

</div>

<div class="figure">

<img src="man/figures/README-downloaded-data.png" alt="Tables with loaded and parsed data." width="100%" />
<p class="caption">
Tables with loaded and parsed data.
</p>

</div>

<div class="figure">

<img src="man/figures/README-plot-all.png" alt="Function dependencies network." width="100%" />
<p class="caption">
Function dependencies network.
</p>

</div>

<div class="figure">

<img src="man/figures/README-plot-node.png" alt="Function dependencies network with selected node." width="100%" />
<p class="caption">
Function dependencies network with selected node.
</p>

</div>
