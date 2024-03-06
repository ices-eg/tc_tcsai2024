library(shiny)
library(htmltools)

source("02_model_fitting.R")
source("03_biological_production.R")

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
    )
  )
)
