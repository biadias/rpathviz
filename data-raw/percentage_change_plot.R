#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL CODE AUTHORS: Evan Gaertner
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
#R SCRIPT FOR PLOTTING PERCENT CHANGE BETWEEN ECOSYSTEM SCENARIOS OR MODELS UPDATES. 
# The original code was written in Phyton by Evan, here is a translation by me. 
#------------------------------------------------------------------------------#


# My idea is write this as a function so we can have percent change for old vs updated models and also percent change between scenarios. 
# The user is responsible for formatting their dt according to the structure bellow
#  file structure = node, group_name, TL_base_model, biomass_base_model, TL_sc1, B_sc1, etc...
#  file structure = node, group_name, TL_base_model, biomass_base_model, TL_updated_model, B_updated_model. 
# I want to have 5 scenarios something like this:
#
# plot_bmsc_diffbd <- function(file_path, n_scenarios = 3) {
#   if(n_scenarios < 1 || n_scenarios > 5) {
#   stop("n_scenarios must be between 1 and 5")
#}

library(tidyverse)
theme_set(theme_minimal(base_family = "Arial"))

#fname <- "C:/Users/biadias/Dropbox/EwE PhD BD/EwE M/ATL HERRING PAPER/ECOSIM/BaseModel_Scs_Diff.csv"
 
scenarios <- read_csv(fname)

#Rename columns # I could also use janitor::
scenarios <- scenarios %>%
  dplyr::rename(
    Node     = Node,
    Group_name = Group_name,
    TL_BM    = "TL_BM",
    B_BM     = "Biomass_t.km-2_BM",
    TL_SC1   = "TL_Sc1",
    B_SC1    = "Biomass_t.km-2_Sc1",
    TL_SC2   = "TL_Sc2",
    B_SC2    = "Biomass_t.km-2_Sc2",
    TL_SC3   = "TL_Sc3",
    B_SC3    = "Biomass_t.km-2_Sc3"
  )

# Percent change in Biomass (dp_bm1, dp_bm2, dp_bm3) relative to the baseline (B_BM)
scenarios <- scenarios %>%
  mutate(
    dp_bm1 = (B_SC1 - B_BM) / B_BM * 100,
    dp_bm2 = (B_SC2 - B_BM) / B_BM * 100,
    dp_bm3 = (B_SC3 - B_BM) / B_BM * 100
  )

# I am renaming here, because my FG names are abbreviated
bar_labels_all <- c("Phytoplankton", "Bacteria", "Microzooplankton", "Copepod-S", "Copepod-L", 
                    "Gelatinous zooplankton", "Micronekton", "Macrobenthos polychaetae", 
                    "Macrobenthos crustaceans", "Macrobenthos mollusks", "Other macrobenthos", 
                    "Megabenthos filters", "Other megabenthos", "Shrimp", "Mesopelagics", 
                    "Atlantic herring", "Alosine", "Atlantic mackerel", "Squid", "Butterfish", 
                    "Small pelagics", "Bluefish", "Striped bass", "Dogfish-S", "Dogfish-L", 
                    "Atlantic Cod-S", "Atlantic Cod-M", "Atlantic Cod-L", "Haddock", "Hake-S", 
                    "Hake-L", "Yellow flounder", "Summer flounder", "Skate", "Demersal benthivores", 
                    "Demersal piscivores", "Demersal omnivores", "Medium pelagics", "Pelagic sharks", 
                    "Large pelagics (HMS)", "Pinnipeds", "Baleen whales", "Odontocetes", 
                    "Seabirds", "Detritus")

scenarios$bar_label <- bar_labels_all

#Compute maximum and minimum percent changes per row. this is important so we can order the plot stacking!!!
scenarios <- scenarios %>%
  mutate(
    dp_max = pmax(dp_bm1, dp_bm2, dp_bm3),
    dp_min = pmin(dp_bm1, dp_bm2, dp_bm3)
  )

#Identify indices corresponding to the two groups. this is based on the Original code in Phyton. 
#    (based on the condition abs(dp_max) >= abs(dp_min))
idx_pos <- which(abs(scenarios$dp_max) >= abs(scenarios$dp_min))
idx_neg <- which(abs(scenarios$dp_max) < abs(scenarios$dp_min))

#For the positive group, sort rows in increasing order of dp_max I also wanted to order everything by less to more change
scenarios_pos <- scenarios[idx_pos, ] %>% arrange(dp_max)

#For the negative group, sort rows in increasing order of dp_min.
scenarios_neg <- scenarios[idx_neg, ] %>% arrange(dp_min)

# Combine the two groups so that negative ones come first (mimicking the original order)
scenarios_ordered <- bind_rows(scenarios_neg, scenarios_pos) %>%
  # Create a new y-axis position (1 = bottom, increasing upward)
  mutate(ypos = row_number())

# Reshape the data into long format.
scenarios_long <- scenarios_ordered %>%
  select(bar_label, dp_bm1, dp_bm2, dp_bm3, ypos) %>%
  pivot_longer(
    cols = starts_with("dp_bm"),
    names_to  = "scenario",
    values_to = "dp"
  ) %>%
  mutate(
    # Rename scenarios to match the original label names
    scenario = recode(scenario,
                      dp_bm1 = "Senario 1",
                      dp_bm2 = "Senario 2",
                      dp_bm3 = "Senario 3")
  )

#Build the plot using the ordered by less to more percent change
p_stacked <- ggplot(food_web_long, aes(x = dp, y = factor(ypos, levels = unique(ypos)), fill = scenario)) +
  geom_col() +
  # Replace y-axis tick labels with the category names
  scale_y_discrete(labels = food_web_ordered$bar_label) +
  # Set custom colors for the scenarios
  scale_fill_manual(values = c("Senario 1" = "black", 
                               "Senario 2" = "#E69F00", 
                               "Senario 3" = "#56B4E9")) +
  labs(x = expression("Percent change in biomass from baseline, (%)"),
       y = NULL,
       fill = "Scenario") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(10, 10, 10, 30)
  ) +
  ggtitle("Stacked Bar Chart: Biomass Percent Changes from Baseline")

p_stacked


ggsave("bmsc_diffbd.png", p, width = 6, height = 8)
ggsave("bmsc_diffbd.eps", p, width = 6, height = 8, device = "eps")
