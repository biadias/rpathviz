# Interactive plots for Rsim

Interactive plots for Rsim plot using plotly:: package and tidyverse::

add another paragraph

## Usage

``` r
rsimplotviz(
  Rsim.output,
  eco.name = NULL,
  spname = "all",
  rel_bio = FALSE,
  palette = "rsim_pal_dark"
)
```

## Arguments

- Rsim.output:

  An object of class Rsim.

- eco.name:

  Optional name of the ecosystem.

- spname:

  A character vector of species names to plot. If "all", all species
  will be plotted and rel_bio will use the default FALSE.

- rel_bio:

  Logical value of whether to plot a single group biomass as relative or
  absolute value.

- palette:

  A character vector of colors to use.

## Value

An interactive plotly object

## Contributors

rsim.plot function from Rpath by Kerim Aydin

## Examples

``` r
if (FALSE) { # \dontrun{
# create the Rsim object
runObj <- Rpath::rsim.scenario(Rpath::rpath(Rpath::REco.params), Rpath::REco.params, years= 1:50)
Rsim.output <- Rpath::rsim.run(runObj, years= 1:50)

# plot the Rsim object two default color blind friendly palettes
rsimplotviz(Rsim.output, eco.name= "Anchovy Bay", spname = "all",
palette= "rsim_pal_dark")
rsimplotviz(Rsim.output, eco.name= "Anchovy Bay", spname = "all",
palette= "rsim_pal_light")
rsimplotviz(Rsim.output, eco.name= "Anchovy Bay", spname = "Foragefish2",
rel_bio=FALSE, palette= "rsim_pal_light")

} # }
```
