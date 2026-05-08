## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(rpathviz)
library(Rpath)
library(data.table)

## ----patch-rsim, include=FALSE------------------------------------------------
# Patch for Rpath 1.1.0: rsim.run fails on models without life-history stanzas
# because it tries to set colnames on NULL stanza matrices.
rsim.run.patched <- function(Rsim.scenario, method = "RK4", years = 1:100) {
  scene <- data.table::copy(Rsim.scenario)
  if (method != "RK4" && method != "AB")
    stop("Invalid method name for solving nonlinear equations")
  scene.years <- row.names(Rsim.scenario$fishing$ForcedFRate)
  syear <- which(as.character(head(years, 1)) == scene.years)
  eyear <- which(as.character(tail(years, 1)) == scene.years)
  if (eyear < syear)  stop("End year cannot be less than start year.")
  if (length(syear) != 1) stop("Starting year not found in scenario.")
  if (length(eyear) != 1) stop("Ending year not found in scenario.")
  scene$rundate <- paste(Sys.time(), ":salt:", runif(1))
  if (method == "RK4") {
    rout <- Rpath:::rk4_run(scene$params, scene$start_state, scene$forcing,
                            scene$fishing, scene$stanzas, syear, eyear)
  } else {
    derv <- Rpath:::deriv_vector(scene$params, scene$start_state, scene$forcing,
                                 scene$fishing, scene$stanzas, syear, 0, 0)
    rout <- Rpath:::Adams_run(scene$params, scene$start_state, scene$forcing,
                              scene$fishing, scene$stanzas, syear, eyear, derv)
  }
  sps <- scene$params$spname[1:(1 + scene$params$NUM_BIO)]
  colnames(rout$out_Biomass)    <- sps
  colnames(rout$out_Catch)      <- sps
  colnames(rout$annual_Catch)   <- sps
  colnames(rout$annual_Biomass) <- sps
  colnames(rout$annual_QB)      <- sps
  colnames(rout$annual_Qlink)   <- 1:(length(rout$annual_Qlink[1, ]))
  # Guard: only set stanza colnames when matrices exist (no-stanza models)
  if (!is.null(rout$out_SSB)  && length(dim(rout$out_SSB))  == 2)
    colnames(rout$out_SSB)  <- scene$stanzas$Oldest
  if (!is.null(rout$out_eggs) && length(dim(rout$out_eggs)) == 2)
    colnames(rout$out_eggs) <- scene$stanzas$Oldest
  if (!is.null(rout$out_Ninf) && length(dim(rout$out_Ninf)) == 2)
    colnames(rout$out_Ninf) <- scene$stanzas$Oldest
  if (!is.null(rout$out_Winf) && length(dim(rout$out_Winf)) == 2)
    colnames(rout$out_Winf) <- scene$stanzas$Oldest
  if (!is.null(rout$out_Nrec) && length(dim(rout$out_Nrec)) == 2)
    colnames(rout$out_Nrec) <- scene$stanzas$Groups
  if (!is.null(rout$out_Wrec) && length(dim(rout$out_Wrec)) == 2)
    colnames(rout$out_Wrec) <- scene$stanzas$Groups
  ys    <- min(as.numeric(rownames(scene$fishing$ForcedCatch)))
  ylist <- seq(ys, length.out = length(rout$annual_Catch[, 1]))
  rownames(rout$annual_Catch)   <- ylist
  rownames(rout$annual_Biomass) <- ylist
  rownames(rout$annual_QB)      <- ylist
  rownames(rout$annual_Qlink)   <- ylist
  rout$pred            <- scene$params$spname[scene$params$PreyTo    + 1]
  rout$prey            <- scene$params$spname[scene$params$PreyFrom  + 1]
  rout$Gear_Catch_sp   <- scene$params$spname[scene$params$FishFrom  + 1]
  rout$Gear_Catch_gear <- scene$params$spname[scene$params$FishThrough + 1]
  rout$Gear_Catch_disp <- ifelse(scene$params$FishTo == 0, "Landing", "Discard")
  rout$start_state       <- scene$start_state
  rout$params$NUM_LIVING <- scene$params$NUM_LIVING
  rout$params$NUM_DEAD   <- scene$params$NUM_DEAD
  rout$params$NUM_GEARS  <- scene$params$NUM_GEARS
  rout$params$spname     <- scene$params$spname
  class(rout) <- "Rsim.output"
  attr(rout, "eco.name") <- attr(scene, "eco.name")
  return(rout)
}

## ----rsim-setup---------------------------------------------------------------
# Build the balanced Rpath model
REco <- rpath(REco.params, eco.name = "R Ecosystem")

# Create an Rsim scenario and run it for 50 years
scene       <- rsim.scenario(REco, REco.params, years = 1:50)
Rsim.output <- rsim.run.patched(scene, years = 1:50)

## ----rsim-all-dark------------------------------------------------------------
rsimplotviz(Rsim.output, eco.name = "R Ecosystem", spname = "all",
            palette = "rsim_pal_dark")

## ----rsim-all-light-----------------------------------------------------------
rsimplotviz(Rsim.output, eco.name = "R Ecosystem", spname = "all",
            palette = "rsim_pal_light")

## ----rsim-single-abs----------------------------------------------------------
rsimplotviz(Rsim.output, eco.name = "R Ecosystem",
            spname  = "Foragefish2",
            rel_bio = FALSE)

## ----rsim-single-rel----------------------------------------------------------
rsimplotviz(Rsim.output, eco.name = "R Ecosystem",
            spname  = "Foragefish2",
            rel_bio = TRUE)

## ----rsim-custom-pal----------------------------------------------------------
my_pal <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51",
            "#023e8a", "#8ecae6", "#219ebc", "#023047", "#ffb703")

rsimplotviz(Rsim.output, eco.name = "R Ecosystem", palette = my_pal)

