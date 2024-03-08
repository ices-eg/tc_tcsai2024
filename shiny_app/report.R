## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(rmarkdown)

mkdir("report")


build_example <- function(filename) {
  rmarkdown::render(
    paste0("report_", filename, ".R"),
    output_format = github_document(html_preview = FALSE),
    output_file = paste0("shiny_", filename, ".md")
  )

  # change figure file path
  x <- readLines(paste0("shiny_", filename, ".md"))
  x <- gsub(paste0("shiny_", filename, "_files/figure-gfm"), filename, x)
  writeLines(x, paste0("shiny_", filename, ".md"))

  mkdir(paste0("report/", filename))
  cp(paste0("shiny_", filename, "_files/figure-gfm/*.png"), paste0("report/", filename))
  unlink(paste0("shiny_", filename, "_files"), recursive = TRUE)
  cp(paste0("shiny_", filename, ".md"), "report", move = TRUE)
}

filenames <- c("02_model_fitting", "03_biological_production")

for (filename in filenames) {
  build_example(filename)
}
