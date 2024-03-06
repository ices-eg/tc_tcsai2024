## Before:
## After:

library(icesTAF)

# create all folders
mkdir("report")
mkdir("report/data")


# copy in required data
cp(file.path("data", "app_data.RData"), "report/data")

# copy in utilities
cp("utilities_models.R", "report")

# copy in server and ui scripts
cp("shiny_ui.R", "report/ui.R")
cp("shiny_server.R", "report/server.R")
cp("shiny_02_model_fitting.R", "report/02_model_fitting.R")

msg("Created shiny app. To run, use: \n\n\tlibrary(shiny)\n\trunApp('report')\n\n")
