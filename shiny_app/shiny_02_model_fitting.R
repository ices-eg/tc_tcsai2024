mod_02_model_fitting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Stock Recruitment Model Fiting"),
    fluidRow(
      column(3, selectInput(ns("data"), "Select dataset", c("haddock", "cod", "herring"))),
      column(3, selectInput(ns("model"), "Select model", c("ricker", "bevholt", "bevholt2"))),
      column(3, textOutput(ns("equation"))),
      column(3, textOutput(ns("ab")))
    ),
    fluidRow(
      column(6, plotOutput(ns("sr_plot"))),
      column(6, plotOutput(ns("ssq"), click = ns("plot_click")))
    )
  )
}


mod_02_model_fitting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # assign model
    model <- reactive({
      switch(input$model,
        ricker = ricker,
        bevholt = bevholt,
        bevholt2 = bevholt2
      )
    })

    output$equation <- renderText({
      as.character(body(model())[2])
    })

    # assign dataset
    data <- reactive({
      switch(input$data,
        haddock = haddock,
        cod = cod,
        herring = herring
      )
    })

    # get ssq data
    ssq <- reactive({
      ssqs[[input$data]][[input$model]]
    })

    best_fit <- reactive({
      # S > 0
      S <- seq(1e-9, max(data()$S), length = 100)
      a <- exp(ssq()$est["loga", "Estimate"])
      b <- exp(ssq()$est["logb", "Estimate"])
      data.frame(S = S, R = exp(model()(a, b, S)))
    })

    # process inputs
    fit <- reactive({
      # S > 0
      S <- seq(1e-9, max(data()$S), length = 100)
      a <- exp(input$plot_click$y)
      b <- exp(input$plot_click$x)
      out <- data.frame(S = S, R = exp(model()(a, b, S)))
      out[out$R < max(data()$R), ]
    })

    # make a plot
    output$sr_plot <- renderPlot({
      data_ <- data()
      p <- ggplot(data_, aes(S, R)) +
        geom_point() +
        geom_line(data = best_fit(), col = "blue") +
        scale_x_continuous("Spawners (kt)", limits = c(0, max(data_$S)), expand = expansion(mult = c(0, .04))) +
        scale_y_continuous("Recruits (millions age 0)", limits = c(0, max(data_$R)), expand = expansion(mult = c(0, .04))) +
        ggtitle("stock and recruitment") +
        theme_minimal()

      if (!is.null(input$plot_click) && nrow(fit()) > 4) {
        p +
          geom_line(data = fit(), col = "red", lwd = 1.5)
      } else {
        p
      }
    })

    output$ssq <- renderPlot({
      image(
        ssq()$logbs, ssq()$logas, ssq()$ssqs,
        xlab = "log b", ylab = "log a", main = "SSQ surface",
        col = c(heat.colors(20, rev = TRUE))
      )
      points(ssq()$est["logb", "Estimate"], ssq()$est["loga", "Estimate"], pch = 16, col = "blue")
    })

    output$ab <- renderText({
      if (!is.null(input$plot_click)) {
        paste0("Selected\n a:", round(exp(input$plot_click$y), 3), "  b:", round(exp(input$plot_click$x), 3))
      } else {
        "Click SSQ plot to select a value for a and b"
      }
    })
  })
}

# load data into model fitting function only
load("data/app_data.RData", envir = environment(mod_02_model_fitting_server))
