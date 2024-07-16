# shiny.destory

<!-- badges: start -->
  
[![R-CMD-check](https://github.com/ashbaldry/shiny.destroy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashbaldry/shiny.destroy/actions/workflows/R-CMD-check.yaml)
  
<!-- badges: end -->

The aim of {shiny.destroy} is to allow dynamic shiny modules to be removed from both the UI and server.

## Installation

```r
require("remotes")
remotes::install_github("ashbaldry/shiny.destroy")
```

## Usage



## Example

![Example shiny.destroy application](/man/figures/example_app.gif)

The code for this example is available in the [examples directory](/inst/examples-shiny)

## Limitations

To include sub-modules within the destroyable module, the server-side module will need to be passed through `makeModuleServerDestroyable` and included as an argument to the parent module to ensure those observers are safely removed.
