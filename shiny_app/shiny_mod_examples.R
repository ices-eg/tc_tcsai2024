mod_examples_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, selectInput(ns("file"), "Select example", c("model fitting", "biological production", "biomass dynamics", "virtual population analysis")))
    ),
    fluidRow(
      uiOutput(ns("md"))
    ),
  )
}


mod_examples_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filename <- reactive({
      switch(input$file,
      `model fitting` = "02_model_fitting.md",
      `biological production` = "03_biological_production.md",
      `biomass dynamics` = "04_biomass_dynamics.md",
      `virtual population analysis` = "06_VPA.md",
      "not_found.md"
      )
    })

    output$md <- renderUI({
      includeMarkdown(path = filename())
    })
  })
}
