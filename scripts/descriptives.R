library(dplyr)
library(knitr)
library(kableExtra)

# Load data
df_for_table <- readRDS("data/barnulf_synthetic_clean.rds")

# Create descriptive table
descriptive_table <- df_for_table %>%
  group_by(year) %>%
  summarise(
    N = n(),
    `Age, M` = mean(child_age, na.rm = TRUE),
    `Age, SD` = sd(child_age, na.rm = TRUE),
    `Boys, n` = sum(child_sex == 1, na.rm = TRUE),
    `Girls, n` = sum(child_sex == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = Value
  )

# Format table
table_html <- descriptive_table %>%
  kable(
    format = "html",
    digits = 2,
    align = c("l", rep("c", ncol(descriptive_table) - 1)),
    caption = "Descriptive Statistics by Year"
  ) %>%
  kable_styling(
    bootstrap_options = NULL,
    full_width = FALSE,
    position = "left"
  ) %>%
  row_spec(0, bold = TRUE) %>%
  add_footnote(
    label = "M = mean; SD = standard deviation.",
    notation = "none"
  )

# Save as HTML
save_kable(
  table_html,
  file = "output/descriptive_table.html"
)
