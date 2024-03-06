mod_03_biological_production_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Application title
    titlePanel("Biological production"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = ns("Amax"),
          "Maximum age", value = 15
        ),
        numericInput(
          inputId = ns("Ninit"),
          "Initial population size", value = 1000
        ),
        sliderInput(ns("Mage"), "M at age", value = 0.2, min = 0, max = 2, step = 0.05),
        sliderInput(ns("Fage"), "F at age", value = 0.3, min = 0, max = 2, step = 0.05),
        sliderInput(ns("alpha"), "SR alpha", value = 1000, min = 0, max = 2000),
        sliderInput(ns("beta"), "SR beta", value = 1000, min = 0, max = 2000),
        sliderInput(ns("v"), "Recruitment error", value = 0, min = 0, max = 1000, step = 1),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = ns("LinePlot")),
        plotOutput(outputId = ns("srPlot"))
      )
    )
  )
}


mod_03_biological_production_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pop <- reactive({
      ageAtF <- 3
      Tmax <- 25
      M <- rep(input$Mage, input$Amax)
      Fmort <- c(rep(0, ageAtF - 1), rep(input$Fage, input$Amax - ageAtF + 1))
      mat <- c(0, 0, rep(1, input$Amax - 2))

      w <- 10 * (1 - exp(-0.5 * ((1:input$Amax) + 0.1)))^3

      AgeModel(input$Ninit, M, Fmort, mat, w, input$Amax, Tmax, input$alpha, input$beta, v = input$v)
    })

    output$LinePlot <- renderPlot({
      pop_ <- pop()

      df <- rbind(
        data.frame(year = 1:length(pop_$SSB), val = pop_$SSB, what = "SSB"),
        data.frame(year = 1:length(pop_$SSB), val = pop_$Rec, what = "Recruitment")
      )

      ggplot(data = df, aes(year, val)) +
        geom_line() +
        labs(x = "Year") +
        facet_wrap(~what, scales = "free") +
        expand_limits(y = 0)
    })

    output$srPlot <- renderPlot({
      pop_ <- pop()

      df <- data.frame(R = pop_$Rec, S = pop_$SSB)
      sr <- data.frame(S = seq(0.1, max(pop_$S), length = 100))
      sr$R <- input$alpha * sr$S / (input$beta + sr$S)

      ggplot(data = df, aes(S, R)) +
        geom_point() +
        geom_line(data = sr, col = "blue") +
        scale_x_continuous("Spawners", limits = c(0, max(pop_$S)), expand = expansion(mult = c(0, .04))) +
        scale_y_continuous("Recruits", limits = c(0, max(pop_$R)), expand = expansion(mult = c(0, .04))) +
        ggtitle("stock and recruitment") +
        theme_minimal()
    })
  })
}
