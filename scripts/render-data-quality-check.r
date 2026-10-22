# Load quarto
library("quarto")

# Render the quarto file where aspects of data quality are investigated
quarto::quarto_render(
  input = "check-data-quality.qmd",
  output_format = "html"
)

# Move output to outpit folder
source_file <- "check-data-quality.html"
target_file <- file.path("output", "check-data-quality.html")

if (!file.exists(source_file)) {
  stop("Rendered HTML file not found: ", source_file)
}

if (!dir.exists("output")) {
  dir.create("output")
}

if (!file.rename(source_file, target_file)) {
  stop("Could not move HTML file to: ", target_file)
}