library(shiny)
library(htmltools)

source("mod_02_model_fitting.R")
source("mod_03_biological_production.R")
source("mod_06_VPA.R")

# shiny user interface
ui <- fluidPage(
  navbarPage(
    title = "ICES Intro to Stock Assessment",
    tabPanel(
      "Model fitting",
      mod_02_model_fitting_ui("model_fitting")
    ),
    tabPanel(
      "Biological production",
      mod_03_biological_production_ui("biological_production")
    ),
    tabPanel(
      "VPA",
      mod_06_VPA_ui("vpa")
    )
  )
)
