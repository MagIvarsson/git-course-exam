# Libraries needed for script
library("dplyr")
library("tidyverse")

# Load data
df_barnulf_synth <- readr::read_csv("data/barnulf_synthetic.csv")

# Quick overview of data
skimr::skim(df_barnulf_synth)

# Visualize with scatter plots
# Define function that creates 12 bar plots at a time
overview_barplot_barnulf <- function(df, vars = names(df), start = 1, n = 12) {

  vars <- intersect(vars, names(df))
  vars <- vars[start:min(start + n - 1, length(vars))]

  df %>%
    dplyr::select(all_of(vars)) %>%
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = value)) +
    geom_bar() +
    facet_wrap(~variable, ncol = 3, scales = "free_x") +
    labs(x = "Response", y = "Frequency") +
    theme_minimal()
}

# Plot variables 12 at a time (except ID)
overview_barplot_barnulf(df_barnulf_synth, start = 2)
overview_barplot_barnulf(df_barnulf_synth, start = 14)
overview_barplot_barnulf(df_barnulf_synth, start = 26)
overview_barplot_barnulf(df_barnulf_synth, start = 38)
overview_barplot_barnulf(df_barnulf_synth, start = 50)
overview_barplot_barnulf(df_barnulf_synth, start = 62)
overview_barplot_barnulf(df_barnulf_synth, start = 74)
overview_barplot_barnulf(df_barnulf_synth, start = 86)

