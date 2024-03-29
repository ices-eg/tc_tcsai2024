mod_06_VPA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Application title
    h3("Simple VPA"),
    fluidRow(
      column(
        3,
        selectInput(ns("data"), "Select dataset", c("haddock", "cod")),
        numericInput(ns("Fages"), "Number of ages to average over for oldest age F", value = 3, min = 2, max = 5),
        sliderInput(ns("Fterm"), "F in final year", value = 0.1, min = 0, max = 2, step = 0.05),
        uiOutput(outputId = ns("xmin_slider"))
      ),
      column(9, plotOutput(outputId = ns("LinePlot"), height = "800px"))
    )
  )
}


mod_06_VPA_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactiveVal(NULL)

    output$xmin_slider <- renderUI({
      sliderInput(ns("Xmin"), "Year to start plotting from", value = min(data()$Year), min = min(data()$Year), max = max(data()$Year) - 5, step = 1, sep = "")
    })

    # assign dataset
    data <- reactive({
      ## empty plot data on selection of dataset
      df(NULL)

      if (input$data == "cod") {
        list(
          catch = cod_catch,
          Year = cod_Year,
          wt = cod_wt,
          mat = cod_mat
        )
      } else if (input$data == "haddock") {
        list(
          catch = haddock_catch,
          Year = haddock_Year,
          wt = haddock_wt,
          mat = haddock_mat
        )
      }
    })

    observeEvent(list(input$Fages, input$Fterm, input$data), {
      # input variables
      Mvec <- c(rep(0.1, 2), rep(0.3, ncol(data()$catch) - 2))

      # fit VPA
      model <- vpa(data()$catch, Mvec = Mvec, Fterm = input$Fterm, Fages = input$Fages)

      # summarise output
      ssb <- rowSums(model$N * data()$wt * data()$mat) / 1000
      Fbar <- rowMeans(model$F)
      Sel <- colMeans(model$F) / max(colMeans(model$F))
      Rec <- model$N[, 1]
      Ns <- model$N

      df_one <- rbind(
        data.frame(year = data()$Year, age = 0, val = ssb, what = "SSB"),
        data.frame(year = data()$Year, age = 0, val = Fbar, what = "Fbar"),
        data.frame(year = data()$Year, age = 0, val = Rec, what = "Recruitment"),
        data.frame(year = data()$Year, age = rep(1:ncol(Ns), each = nrow(Ns)), val = log(c(Ns)), what = "log Numbers")
      )

      df_one$FinalF <- input$Fterm
      df_one$Fages <- input$Fages

      if (!input$Fages %in% df()$Fages || !input$Fterm %in% df()$FinalF) {
        df(rbind(df(), df_one))
      }
    })

    output$LinePlot <- renderPlot({
      df_ <- df()[df()$year >= input$Xmin, ]
      if (nrow(df_) > 0) {
        ggplot(
          data = df_,
          aes(year, val, colour = FinalF, linetype = factor(age), group = interaction(FinalF, age, Fages))
          ) +
          geom_line() +
          labs(x = "Year") +
          facet_wrap(~what, scales = "free", ncol = 2) +
          expand_limits(y = 0)
      }
    })
  })
}

# load data into vpa
load("data/app_data.RData", envir = environment(mod_06_VPA_server))
