library(shiny)
library(readxl)
library(DT)
library(writexl)

built_in_data <- list(
  mtcars = mtcars,
  iris = iris,
  ToothGrowth = ToothGrowth,
  airquality = airquality
)

ui <- fluidPage(
  titlePanel("Универсальная загрузка и обработка данных"),
  sidebarLayout(
    sidebarPanel(
      h4("Источник данных"),
      radioButtons(
        "data_source",
        "Выберите источник:",
        choices = c("Встроенные данные" = "builtin", "Файл Excel" = "excel"),
        selected = "builtin"
      ),
      conditionalPanel(
        condition = "input.data_source == 'builtin'",
        selectInput("builtin_name", "Набор данных:", choices = names(built_in_data))
      ),
      conditionalPanel(
        condition = "input.data_source == 'excel'",
        fileInput("excel_file", "Загрузите Excel (.xlsx / .xls)",
                  accept = c(".xlsx", ".xls")),
        uiOutput("sheet_ui")
      ),
      tags$hr(),
      h4("Обработка"),
      uiOutput("numeric_col_ui"),
      selectInput(
        "operation",
        "Операция:",
        choices = c("Без изменений" = "none", "Нормализация (0-1)" = "normalize", "Логарифм" = "log")
      ),
      tags$small("Операции применяются только к выбранному числовому столбцу."),
      tags$hr(),
      downloadButton("download_csv", "Скачать результат (CSV)"),
      downloadButton("download_xlsx", "Скачать результат (Excel)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Просмотр данных", DTOutput("data_preview")),
        tabPanel("Сводка", verbatimTextOutput("summary_out"))
      )
    )
  )
)

server <- function(input, output, session) {
  loaded_excel <- reactive({
    req(input$excel_file)
    sheets <- excel_sheets(input$excel_file$datapath)
    req(input$sheet_name)
    read_excel(input$excel_file$datapath, sheet = input$sheet_name)
  })

  output$sheet_ui <- renderUI({
    req(input$excel_file)
    selectInput("sheet_name", "Лист Excel:", choices = excel_sheets(input$excel_file$datapath))
  })

  raw_data <- reactive({
    if (input$data_source == "builtin") {
      as.data.frame(built_in_data[[input$builtin_name]])
    } else {
      as.data.frame(loaded_excel())
    }
  })

  output$numeric_col_ui <- renderUI({
    df <- raw_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    if (length(numeric_cols) == 0) {
      return(tags$em("В данных нет числовых столбцов для обработки."))
    }

    selectInput("numeric_col", "Числовой столбец:", choices = numeric_cols)
  })

  processed_data <- reactive({
    df <- raw_data()

    if (is.null(input$numeric_col) || !(input$numeric_col %in% names(df))) {
      return(df)
    }

    col_values <- df[[input$numeric_col]]

    if (input$operation == "normalize") {
      rng <- range(col_values, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] != rng[2]) {
        df[[input$numeric_col]] <- (col_values - rng[1]) / (rng[2] - rng[1])
      }
    } else if (input$operation == "log") {
      df[[input$numeric_col]] <- ifelse(col_values > 0, log(col_values), NA_real_)
    }

    df
  })

  output$data_preview <- renderDT({
    datatable(processed_data(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$summary_out <- renderPrint({
    summary(processed_data())
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("processed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("processed_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(processed_data(), path = file)
    }
  )
}

shinyApp(ui, server)
