## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(rmarkdown)

mkdir("report")

rmarkdown::render(
  "report_02_model_fitting.R",
  output_format = github_document(html_preview = FALSE),
  output_file = "shiny_02_model_fitting.md"
)

x <- readLines("shiny_02_model_fitting.md")
x <- gsub("shiny_02_model_fitting_files/figure-gfm", "02_model_fitting", x)
writeLines(x, "shiny_02_model_fitting.md")

mkdir("report/02_model_fitting")
cp("shiny_02_model_fitting_files/figure-gfm/*.png", "report/02_model_fitting")
unlink("shiny_02_model_fitting_files", recursive = TRUE)
