#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL AUTHORS: rsim.plot function from Rpath by Kerim Aydin
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Interactive plots for Rsim plot using plotly:: package and tidyverse::
#------------------------------------------------------------------------------#


library(plotly)
library(tidyverse)
library(colorspace) # default color palette, but I can simplify this later
library(grDevices)



rsim.plot.interactive <- function(Rsim.output,
                                  spname = "all",
                                  indplot = FALSE,
                                  palette = "rainbow") {
  rsim_name <- deparse(substitute(Rsim.output))
  mrg <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 50,
    pad = 20
  )
  
  if ("all" %in% spname) {
    spname <- colnames(Rsim.output$out_Biomass)[2:ncol(Rsim.output$out_Biomass)]
  }
  # Extract and calculate rel. B
  if (!indplot) {
    # Multiple functional groups
    biomass <- Rsim.output$out_Biomass[, spname, drop = FALSE]
    start.bio <- biomass[1, ]
    start.bio[start.bio == 0] <- 1
    rel.bio <- sweep(biomass, 2, start.bio, "/") # divide each column by its start value
  } else {
    # For a single functional group
    spnum <- which(Rsim.output$params$spname == spname)
    biomass <- Rsim.output$out_Biomass[, spnum]
    rel.bio <- biomass / biomass[1]
    rel.bio <- matrix(rel.bio,
                      ncol = 1,
                      dimnames = list(NULL, spname))
  }
  
  # Create a time vector
  time <- seq_len(nrow(rel.bio))
  df <- cbind(time, as.data.frame(rel.bio))
  df_long <- tidyr::pivot_longer(df,
                                 cols = -time,
                                 names_to = "Species",
                                 values_to = "RelativeBiomass")
  df_long$Species <- factor(df_long$Species, levels = spname)
  n_species <- length(spname)
  #b_palette <- colorspace::rainbow_hcl
  
  if (is.character(palette) &&
      length(palette) == 1 && exists(palette, mode = "function")) {
    # If the palette is pre-determined e.g. "rainbow" or "terrain.colors"
    # https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
    pal_fun <- match.fun(palette)
    my_colors <- pal_fun(n_species)
  } else if (is.character(palette) && length(palette) > 1) {
    # If you decide to give a vector of colors
    if (length(palette) < n_species) {
      # the function will interpolate if there not enough colors
      my_colors <- grDevices::colorRampPalette(palette)(n_species)
    } else {
      # If enough colors or more
      my_colors <- palette[1:n_species]
    }
  } else {
    my_colors <- rainbow(n_species)
  }
  
  # Plotly object
  rsim.int.plotly <- plot_ly(
    data = df_long,
    x = ~ time,
    y = ~ RelativeBiomass,
    color = ~ Species,
    colors = my_colors,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    layout(
      title = paste("Relative Biomass Over Time:" , rsim_name),
      xaxis = list(title = "Months"),
      yaxis = list(title = "Relative Biomass"),
      margin = mrg
    )
  
  return(rsim.int.plotly)
}
