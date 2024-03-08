## Before:
## After:

library(icesTAF)

# create all folders
mkdir("shiny")
mkdir("shiny/data")


# copy in required data
cp(file.path("data", "app_data.RData"), "shiny/data")

# copy in utilities
cp("utilities_models.R", "shiny")
cp("utilities_vpa.R", "shiny")

# copy in server and ui scripts
cp("shiny_ui.R", "shiny/ui.R")
cp("shiny_server.R", "shiny/server.R")
cp("shiny_mod_02_model_fitting.R", "shiny/mod_02_model_fitting.R")
cp("shiny_mod_03_biological_production.R", "shiny/mod_03_biological_production.R")
cp("shiny_mod_06_VPA.R", "shiny/mod_06_VPA.R")

# copy over examples
cp("shiny_mod_examples.R", "shiny/mod_examples.R")

filenames <- c("02_model_fitting", "03_biological_production")

for (filename in filenames) {
  cp(paste0("report/shiny_", filename, ".md"), paste0("shiny/", filename, ".md"))
  mkdir(paste0("shiny/", filename, ""))
  cp(paste0("report/", filename, "/"), "shiny")
}

msg("Created shiny app. To run, use: \n\n\tlibrary(shiny)\n\trunApp('shiny')\n\n")
