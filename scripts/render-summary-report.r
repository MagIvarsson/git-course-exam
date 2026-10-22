# Load quarto
library("quarto")

# Render the quarto file where aspects of data quality are investigated
quarto::quarto_render(
  input = "summary-report.qmd",
  output_format = "html"
)

# Move output to outpit folder
source_file <- "summary-report.html"
target_file <- file.path("output", "summary-report.html")

if (!file.exists(source_file)) {
  stop("Rendered HTML file not found: ", source_file)
}

if (!dir.exists("output")) {
  dir.create("output")
}

if (!file.rename(source_file, target_file)) {
  stop("Could not move HTML file to: ", target_file)
}