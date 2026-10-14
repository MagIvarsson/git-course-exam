# Load quarto
library("quarto")

# Render the quarto file where aspects of data quality are investigated
quarto::quarto_render(
  input = "scripts/check-data-quality.qmd",
  output_format = "html"
)
