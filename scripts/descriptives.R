library("dplyr")
library("knitr")
library("kableExtra")

# Load data
df_for_table <- readRDS("data/barnulf_synthetic_clean.rds")

# Create descriptive table
table_html <- descriptive_table %>%
  kable(
    format = "html",
    align = c("l", rep("c", ncol(descriptive_table) - 1)),
    caption = "Descriptive Statistics by Year",
    escape = TRUE
  ) %>%
  kable_styling(
    bootstrap_options = NULL,
    full_width = FALSE,
    position = "left"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    extra_css = "border-top: 1px solid black; border-bottom: 1px solid black;"
  ) %>%
  row_spec(
    nrow(descriptive_table),
    extra_css = "border-bottom: 1px solid black;"
  ) %>%
  add_footnote(
    label = "M = mean; SD = standard deviation.",
    notation = "none"
  )

# Save as HTML
dir.create("output", showWarnings = FALSE, recursive = TRUE)

save_kable(
  table_html,
  file = "output/descriptive_table.html"
)
