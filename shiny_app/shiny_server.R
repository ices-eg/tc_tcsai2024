library(ggplot2)

# load models
source("utilities_models.R")

# the app logic
server <- function(input, output, session) {
  mod_02_model_fitting_server("model_fitting")
}
