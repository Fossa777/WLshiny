library(shiny)
library(readxl)
library(DT)
library(writexl)

options(shiny.maxRequestSize = 100 * 1024^2)  # до 100MB для Excel

# Демо-данные для показа приложения на shinyapps.io без загрузки файлов
built_in_data <- list(
  mtcars = mtcars,
  iris = iris,
  ToothGrowth = ToothGrowth,
  airquality = airquality
)

ui <- fluidPage(
  titlePanel("Импорт, обработка и выгрузка данных"),
  sidebarLayout(
    sidebarPanel(
      h4("1) Источник данных"),
      radioButtons(
        "data_source",
        label = NULL,
        choices = c("Встроенные данные" = "builtin", "Excel файл" = "excel"),
        selected = "builtin"
      ),
      conditionalPanel(
        "input.data_source === 'builtin'",
        selectInput("builtin_name", "Набор данных:", choices = names(built_in_data))
      ),
      conditionalPanel(
        "input.data_source === 'excel'",
        fileInput(
          "excel_file",
          "Загрузите Excel (.xlsx / .xls)",
          accept = c(".xlsx", ".xls")
        ),
        checkboxInput("col_names", "Первая строка содержит имена столбцов", value = TRUE),
        numericInput("skip_rows", "Пропустить строк сверху", value = 0, min = 0, step = 1),
        uiOutput("sheet_ui")
      ),
      tags$hr(),
      h4("2) Обработка"),
      uiOutput("numeric_col_ui"),
      selectInput(
        "operation",
        "Операция:",
        choices = c(
          "Без изменений" = "none",
          "Нормализация (0-1)" = "normalize",
          "Логарифм (только > 0)" = "log"
        )
      ),
      tags$small("Операция применяется только к выбранному числовому столбцу."),
      tags$hr(),
      h4("3) Выгрузка"),
      downloadButton("download_csv", "Скачать результат (CSV)"),
      br(), br(),
      downloadButton("download_xlsx", "Скачать результат (Excel)")
    ),
    mainPanel(
      textOutput("upload_hint"),
      tabsetPanel(
        tabPanel("Данные", DTOutput("data_preview")),
        tabPanel("Сводка", verbatimTextOutput("summary_out")),
        tabPanel("Инфо", verbatimTextOutput("data_info"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$sheet_ui <- renderUI({
    req(input$excel_file)

    sheets <- tryCatch(
      excel_sheets(input$excel_file$datapath),
      error = function(e) character(0)
    )

    if (length(sheets) == 0) {
      return(tags$em("Не удалось прочитать листы Excel. Проверьте файл."))
    }

    selectInput("sheet_name", "Лист Excel:", choices = sheets)
  })

  loaded_excel <- reactive({
    req(input$excel_file)
    req(input$sheet_name)

    tryCatch(
      as.data.frame(
        read_excel(
          path = input$excel_file$datapath,
          sheet = input$sheet_name,
          col_names = isTRUE(input$col_names),
          skip = input$skip_rows
        )
      ),
      error = function(e) {
        showNotification(
          paste("Ошибка чтения Excel:", e$message),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
  })

  raw_data <- reactive({
    if (input$data_source == "builtin") {
      return(as.data.frame(built_in_data[[input$builtin_name]]))
    }

    uploaded <- loaded_excel()
    validate(need(!is.null(uploaded), "Загрузите корректный Excel-файл для продолжения."))
    uploaded
  })

  output$upload_hint <- renderText({
    if (input$data_source == "excel") {
      "Можно загрузить практически любую табличную структуру из Excel (xls/xlsx)."
    } else {
      "Выбран демонстрационный встроенный набор данных."
    }
  })

  output$numeric_col_ui <- renderUI({
    df <- raw_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    if (length(numeric_cols) == 0) {
      return(tags$em("В наборе нет числовых столбцов для обработки."))
    }

    selectInput("numeric_col", "Числовой столбец:", choices = numeric_cols)
  })

  processed_data <- reactive({
    df <- raw_data()

    if (is.null(input$numeric_col) || !(input$numeric_col %in% names(df))) {
      return(df)
    }

    values <- df[[input$numeric_col]]

    if (input$operation == "normalize") {
      rng <- range(values, na.rm = TRUE)
      if (all(is.finite(rng)) && diff(rng) != 0) {
        df[[input$numeric_col]] <- (values - rng[1]) / diff(rng)
      }
    }

    if (input$operation == "log") {
      df[[input$numeric_col]] <- ifelse(values > 0, log(values), NA_real_)
    }

    df
  })

  output$data_preview <- renderDT({
    datatable(
      processed_data(),
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$summary_out <- renderPrint({
    summary(processed_data())
  })

  output$data_info <- renderPrint({
    df <- processed_data()
    list(
      rows = nrow(df),
      columns = ncol(df),
      column_names = names(df),
      data_source = input$data_source
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("processed_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(processed_data(), file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("processed_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write_xlsx(processed_data(), path = file)
    }
  )
}

shinyApp(ui, server)
