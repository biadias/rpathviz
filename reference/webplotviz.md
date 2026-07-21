# Interactive plots for Rsim

Interactive plots for Rsim plot using plotly:: package and tidyverse::

add another paragraph

## Usage

``` r
webplotviz(
  Rpath.obj,
  eco.name = attr(Rpath.obj, "eco.name"),
  line.col = "grey",
  h_spacing = 3,
  node_size_min = 1,
  node_size_max = 30,
  fleet_color = "#B40F20",
  groups_palette = "rpath_pal_dark",
  text_size = 3,
  max.overlaps = 50,
  gradient = TRUE,
  labels = TRUE,
  cluster_method = c("fast_greedy", "louvain", "walktrap", "edge_betweenness"),
  low_tl_spread = 1
)
```

## Arguments

- Rpath.obj:

  An object of class Rpath `rpath()`.

- eco.name:

  Optional name of the ecosystem. Default is the `eco.name` attribute
  from the rpath object created from running `rpath()`.

- line.col:

  description.

- h_spacing:

  horizontal spacing multiplier, it spreads nodes horizontally.

- node_size_min:

  from scale_value() Minimum value of the new scale (default is 1).

- node_size_max:

  from scale_value() Maximum value of the new scale (default is 30).

- fleet_color:

  single color for fleet nodes.

- groups_palette:

  color palette for non-fleet groups. Two palette options rpath_pal_dark
  and rpath_pal_light.

- text_size:

  size of the text labels.

- max.overlaps:

  maximum number of overlapping labels allowed by ggrepel. Increase for
  denser label coverage, decrease to reduce clutter. Default is 50.

- gradient:

  Logical. If `TRUE`(default), edges are drawn with a colour gradient
  (prey-to-predator) using `after_stat(index)`. If `FALSE`, edges are
  drawn in a fixed `line.col` colour, which renders significantly faster
  for large food webs.

- labels:

  Logical. If `TRUE` (default), group names are drawn using `ggrepel` to
  avoid overlap. Set to `FALSE` to skip labels entirely, which roughly
  halves render time on large food webs.

- cluster_method:

  Community detection algorithm used to colour node clusters. One of
  `"fast_greedy"` (default), `"louvain"`, `"walktrap"`, or
  `"edge_betweenness"`. `"fast_greedy"` and `"louvain"` are recommended
  for large food webs; `"edge_betweenness"` is the most accurate but
  significantly slower.

- low_tl_spread:

  Extra horizontal spread multiplier for low trophic-level nodes. At the
  lowest trophic level the x-spacing is `h_spacing * low_tl_spread`; at
  the highest trophic level it equals `h_spacing`; values in between are
  linearly interpolated. Default is `1` (uniform spacing, identical to
  the original behaviour).

## Value

Returns a plot visualization of the food web.

## Contributors

webplot original function from Rpath by Kerim Aydin

## Plotting

Caution: For large food webs see the examples.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read in Rpath parameter file, generate and name model object
Rpath.obj <- Rpath::rpath(Rpath::AB.params, eco.name = "Anchovy Bay")
# Plot food web diagram with all groups labeled, including fleets
webplotviz(Rpath.obj, h_spacing = 3, text_size = 3)

# For large food webs, use labels = FALSE to speed up rendering, then save
# with ggsave() for best results.
# 1) assign the plot to an object
# 2) use ggplot2::ggsave() to save the plot for a fast visualization
Rpath.obj <- Rpath::rpath(Rpath::Ecosense.EBS, eco.name = "Eastern Bering Sea")
wp <- webplotviz(Rpath.obj, h_spacing = 3, text_size = 3,
  node_size_min = 1, node_size_max = 50, labels = FALSE)
ggplot2::ggsave("figures/EBSfoodwebplot2.png", wp, width = 16, height = 10)


} # }
```
