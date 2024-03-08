library(ggplot2)

set.seed(239874)

# load models
source("utilities_models.R")
source("utilities_vpa.R")

# the app logic
server <- function(input, output, session) {
  mod_02_model_fitting_server("model_fitting")
  mod_03_biological_production_server("biological_production")
  mod_06_VPA_server("vpa")
  mod_examples_server("examples")
}
