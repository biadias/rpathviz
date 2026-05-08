## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 9,
  fig.height = 6
)

## ----setup, message=FALSE-----------------------------------------------------
library(rpathviz)
library(Rpath)

## ----anchovy-bay-basic--------------------------------------------------------
# Build the Rpath balanced model
Rpath.obj <- rpath(AB.params, eco.name = "Anchovy Bay")

# Plot with default settings
webplotviz(Rpath.obj)

## ----anchovy-bay-custom-------------------------------------------------------
webplotviz(
  Rpath.obj,
  h_spacing     = 5,
  node_size_min = 2,
  node_size_max = 20,
  text_size     = 4
)

## ----anchovy-bay-light--------------------------------------------------------
webplotviz(Rpath.obj, groups_palette = "rpath_pal_light")

## ----anchovy-bay-custom-pal---------------------------------------------------
webplotviz(
  Rpath.obj,
  groups_palette = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
  fleet_color    = "#023e8a"
)

## ----rendering-options, fig.width = 9, fig.height = 6-------------------------
# labels = FALSE: fastest render, useful for large-web previews
webplotviz(Rpath.obj, labels = FALSE)

## ----gradient-on, fig.width = 9, fig.height = 6-------------------------------
# gradient = TRUE: prey–predator colour gradient on edges
webplotviz(Rpath.obj, gradient = TRUE)

## ----ebs-setup----------------------------------------------------------------
# Build the Eastern Bering Sea model
EBS.obj <- rpath(Ecosense.EBS, eco.name = "Eastern Bering Sea")

## ----ebs-plot, fig.width = 14, fig.height = 9---------------------------------
EBS.plot <- webplotviz(
  EBS.obj,
  h_spacing     = 3,
  text_size     = 2.5,
  node_size_min = 1,
  node_size_max = 50,
  labels        = FALSE
)
EBS.plot

## ----ebs-save, eval = FALSE---------------------------------------------------
# ggplot2::ggsave(
#   "EBS_foodweb.png",
#   EBS.plot,
#   width  = 16,
#   height = 10,
#   dpi    = 300
# )

