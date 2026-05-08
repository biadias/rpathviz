# Simulation Plots with rsimplotviz

``` r

library(rpathviz)
library(Rpath)
library(data.table)
```

## Overview

[`rsimplotviz()`](https://biadias.github.io/rpathviz/reference/rsimplotviz.md)
creates an interactive [plotly](https://plotly.com/r/) time series from
an `Rsim` output object. You can plot all species at once or focus on
individual groups, and toggle between absolute and relative biomass.

------------------------------------------------------------------------

## Setting up an Rsim scenario

We use the fictional R Ecosystem (`REco.params`) bundled with `Rpath`.

``` r

# Build the balanced Rpath model
REco <- rpath(REco.params, eco.name = "R Ecosystem")

# Create an Rsim scenario and run it for 50 years
scene       <- rsim.scenario(REco, REco.params, years = 1:50)
Rsim.output <- rsim.run.patched(scene, years = 1:50)
```

------------------------------------------------------------------------

## Plot all species — relative biomass

When `spname = "all"` (the default), all species are plotted as relative
biomass (biomass relative to the starting value). Use the legend to
toggle individual species on and off.

``` r

rsimplotviz(Rsim.output, eco.name = "R Ecosystem", spname = "all",
            palette = "rsim_pal_dark")
```

------------------------------------------------------------------------

## Light color palette

Switch to `"rsim_pal_light"` for a softer, pastel look.

``` r

rsimplotviz(Rsim.output, eco.name = "R Ecosystem", spname = "all",
            palette = "rsim_pal_light")
```

------------------------------------------------------------------------

## Single species — absolute biomass

To examine one species in its original biomass units, pass its name to
`spname` and set `rel_bio = FALSE`.

``` r

rsimplotviz(Rsim.output, eco.name = "R Ecosystem",
            spname  = "Foragefish2",
            rel_bio = FALSE)
```

------------------------------------------------------------------------

## Single species — relative biomass

Setting `rel_bio = TRUE` rescales the single species to its starting
biomass, making it easy to compare trends across species or scenarios.

``` r

rsimplotviz(Rsim.output, eco.name = "R Ecosystem",
            spname  = "Foragefish2",
            rel_bio = TRUE)
```

------------------------------------------------------------------------

## Custom color palette

Supply any vector of hex colors.
[`rsimplotviz()`](https://biadias.github.io/rpathviz/reference/rsimplotviz.md)
will interpolate if you provide fewer colors than there are species.

``` r

my_pal <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51",
            "#023e8a", "#8ecae6", "#219ebc", "#023047", "#ffb703")

rsimplotviz(Rsim.output, eco.name = "R Ecosystem", palette = my_pal)
```
