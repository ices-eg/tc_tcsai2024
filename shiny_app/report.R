## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(rmarkdown)

mkdir("report")

if (FALSE) {
  rmarkdown::render(
    "report_02_model_fitting.R",
    output_format = github_document(html_preview = FALSE)
  )

  x <- readLines("report_02_model_fitting.md")
  x <- gsub("![](report_02_model_fitting_files/figure-gfm", "![](report/02_model_fitting", x)
  writeLines(x, "temp.md")
}
