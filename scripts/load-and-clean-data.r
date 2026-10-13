# Libraries needed for script
library("dplyr")
library("tidyverse")

# Load data
df_barnulf_synth <- readr::read_csv("data/barnulf_synthetic.csv")

# Quick overview
skimr::skim(df_barnulf_synth)
