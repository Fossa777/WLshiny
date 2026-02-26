# main_app.R
`%||%` <- function(x, y) if (!is.null(x)) x else y

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(openxlsx)
library(shinyWidgets)
library(scales)
library(igraph)
library(shinycssloaders)
library(tidyr)
library(cowplot)
library(shinyAce)
library(broom) 
library(purrr)
library(grid)
library(shinyjs)

source("Robust.R")
source("compare_before_after.R")
source("db_export_module.R")

# Загружаем модули
source("statistics_module.R")
source("plotting_module.R")
source("cleaning_module.R")
source("ui_module.R")
source("grouping_module.R")
source("grouping_server_module.R")

source_grouping_functions()

# ------------- ЗАГРУЖАЕМ ДАННЫЕ -------------------------------------------

prepare_species_data <- function(df) {
  required_cols <- c("species", "species_name_ru", "length", "weight", "maxlength")
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    stop(paste0("Отсутствуют обязательные колонки: ", paste(missing_cols, collapse = ", ")))
  }

  if ("Family" %in% colnames(df)) df$Family <- as.factor(df$Family)
  if ("species" %in% colnames(df)) df$species <- as.factor(df$species)
  if ("Salt" %in% colnames(df)) df$Salt <- as.factor(df$Salt)

  df <- df %>%
    filter(
      !is.na(species),
      !is.na(weight),
      !is.na(length),
      !is.na(maxlength),
      weight > 0,
      length > 0,
      maxlength > 0
    )

  species_counts_local <- df %>%
    group_by(species) %>%
    summarise(n_points = n(), .groups = "drop")

  species_to_keep_local <- species_counts_local %>%
    filter(n_points >= 7) %>%
    pull(species)

  filtered_df <- df %>%
    filter(species %in% species_to_keep_local) %>%
    mutate(
      logW = log(weight),
      logL = log(length)
    )

  list(
    data = filtered_df,
    species_counts = species_counts_local,
    species_to_keep = species_to_keep_local
  )
}

initial_loaded <- prepare_species_data(read_excel("10year2026.xlsx"))
Species10 <- initial_loaded$data
species_counts <- initial_loaded$species_counts
species_to_keep <- initial_loaded$species_to_keep

cat("=== ФИЛЬТРАЦИЯ ВИДОВ ПО КОЛИЧЕСТВУ ТОЧКИ ===\n")
cat("Исходное количество видов:", length(unique(species_counts$species)), "\n")
cat("Видов после фильтрации (≥7 точек):", length(species_to_keep), "\n")

# Загружаем данные
data <- Species10

create_interactive_comparison <- function(data) {
  
  ui <- create_main_ui()
  

#--------------  SERVER  --------------------------------

server <- function(input, output, session) {
  
    # Реактивные значения
    clean_all_results <- reactiveValues(
      result = NULL,
      timestamp = NULL,
      gap_filtered = NULL,
      gap_diagnostics = NULL
    )
    
   grouping_state <- reactiveValues(
    models = NULL,
    grouped = NULL,
    current_group = 1,
    current_overall_group = NA_integer_,
    multi_groups = integer(0)
  )

  current_data <- reactiveVal(data)
  current_source_name <- reactiveVal("Встроенный файл: 10year2026.xlsx")

  i18n <- reactive({
    if (identical(input$ui_lang, "en")) {
      list(
        source_prefix = "Data source",
        built_in = "Built-in file: 10year2026.xlsx",
        uploaded_prefix = "User file",
        rows = "rows",
        species = "species",
        upload_ok = "✅ Data uploaded successfully. Using user Excel file.",
        upload_err = "❌ File upload error:",
        reset_msg = "ℹ️ Switched back to built-in data from 10year2026.xlsx.",
        labels = list(
          upload = "Upload Excel (.xlsx)",
          reset = "Use built-in data",
          download = "Download current source data",
          species = "Select species:",
          next_label = "➡️Next species",
          prev_label = "⬅️Previous species",
          highlight = "Highlight outliers",
          point_size = "Point size:",
          line_size = "Line width:",
          alpha = "Point alpha:",
          clean_final2 = "Final threshold:",
          min_final_n = "Min points after cleaning:",
          clean_model2 = "Cleaning model:",
          clean_all2 = "Run cleaning",
          ribbon_percent = "Confidence band width (%):",
          ribbon_alpha = "Band transparency:",
          ribbon_model = "Model for band:",
          show_ribbon = "Show confidence band",
          show_points = "Show points",
          show_power = "Show power model",
          show_exp = "Show exponential model"
        )
      )
    } else {
      list(
        source_prefix = "Источник данных",
        built_in = "Встроенный файл: 10year2026.xlsx",
        uploaded_prefix = "Пользовательский файл",
        rows = "строк",
        species = "видов",
        upload_ok = "✅ Данные успешно загружены. Используется пользовательский Excel.",
        upload_err = "❌ Ошибка загрузки файла:",
        reset_msg = "ℹ️ Возвращены встроенные данные из 10year2026.xlsx.",
        labels = list(
          upload = "Загрузите Excel (.xlsx)",
          reset = "Использовать встроенные данные",
          download = "Скачать текущие исходные данные",
          species = "Выберите вид:",
          next_label = "➡️Следующий вид",
          prev_label = "⬅️Предыдущий вид",
          highlight = "Подсветить выбросы",
          point_size = "Размер точек:",
          line_size = "Толщина линий:",
          alpha = "Прозрачность точек:",
          clean_final2 = "Финальный порог:",
          min_final_n = "Мин. точек после очистки:",
          clean_model2 = "Модель для очистки:",
          clean_all2 = "Запустить очистку",
          ribbon_percent = "Ширина доверительной полосы (%):",
          ribbon_alpha = "Прозрачность полосы:",
          ribbon_model = "Для какой модели:",
          show_ribbon = "Показывать доверительную полосу",
          show_points = "Показывать точки",
          show_power = "Показывать степенную модель",
          show_exp = "Показывать экспоненциальную модель"
        )
      )
    }
  })
  observe({
    tr <- i18n()

    # Основные контролы анализа
    updateSelectInput(session, "species", label = tr$labels$species)
    updateActionButton(session, "reset_default_data", label = tr$labels$reset)
    updateActionButton(session, "next_species", label = tr$labels$next_label)
    updateActionButton(session, "prev_species", label = tr$labels$prev_label)
    updateActionButton(session, "highlight_outliers", label = tr$labels$highlight)

    # Анализ видов
    updateSliderInput(session, "point_size", label = tr$labels$point_size)
    updateSliderInput(session, "line_size", label = tr$labels$line_size)
    updateSliderInput(session, "alpha", label = tr$labels$alpha)
    updateSliderInput(session, "ribbon_percent", label = tr$labels$ribbon_percent)
    updateSliderInput(session, "ribbon_alpha", label = tr$labels$ribbon_alpha)
    updateSelectInput(session, "ribbon_model", label = tr$labels$ribbon_model)
    updateCheckboxInput(session, "show_ribbon", label = tr$labels$show_ribbon)
    updateCheckboxInput(session, "show_points", label = tr$labels$show_points)
    updateCheckboxInput(session, "show_power", label = tr$labels$show_power)
    updateCheckboxInput(session, "show_exp", label = tr$labels$show_exp)

    # Очистка
    updateSelectInput(session, "clean_model2", label = tr$labels$clean_model2)
    updateSliderInput(session, "clean_final2", label = tr$labels$clean_final2)
    updateSliderInput(session, "min_final_n", label = tr$labels$min_final_n)
    updateActionButton(session, "clean_all2", label = tr$labels$clean_all2)

    # Прочие вкладки (основные элементы)
    updateSliderInput(session, "compare_ribbon_percent", label = if (identical(input$ui_lang, "en")) "Confidence band width (%)" else "Ширина доверительной полосы (%)")
    updateActionButton(session, "run_grouping", label = if (identical(input$ui_lang, "en")) "Run grouping" else "Запустить группировку")
    updateActionButton(session, "group_prev", label = if (identical(input$ui_lang, "en")) "◀ Previous" else "◀ Предыдущая")
    updateActionButton(session, "group_next", label = if (identical(input$ui_lang, "en")) "Next ▶" else "Следующая ▶")
    updateActionButton(session, "export_groups", label = if (identical(input$ui_lang, "en")) "Export to Excel" else "Экспорт в Excel")
    updateActionButton(session, "export_grid", label = if (identical(input$ui_lang, "en")) "📁 Export grid" else "📁 Экспортировать сетку")
    updateActionButton(session, "export_individual", label = if (identical(input$ui_lang, "en")) "📁 Export separately" else "📁 Экспортировать отдельно")
    updateActionButton(session, "preview_grid", label = if (identical(input$ui_lang, "en")) "👁 Refresh preview" else "👁 Обновить предпросмотр")
    updateDownloadButton(session, "download_export_plot", label = if (identical(input$ui_lang, "en")) "Download current preview" else "Скачать текущий предпросмотр")
    updateActionButton(session, "main_overall_select_all", label = if (identical(input$ui_lang, "en")) "Select all" else "Выбрать все")
    updateActionButton(session, "main_overall_clear_all", label = if (identical(input$ui_lang, "en")) "Clear" else "Очистить")
    updateActionButton(session, "main_overall_export", label = if (identical(input$ui_lang, "en")) "📁 Export plot" else "📁 Экспортировать график")

    # Перевод заголовков вкладок/блоков через JS
    if (identical(input$ui_lang, "en")) {
      shinyjs::runjs("$(\"a[data-value=\'Анализ видов\']\").text(\'Species analysis\');")
      shinyjs::runjs("$(\"a[data-value=\'Очистка данных\']\").text(\'Data cleaning\');")
      shinyjs::runjs("$(\"a[data-value=\'Сравнение до/после\']\").text(\'Before/after comparison\');")
      shinyjs::runjs("$(\"a[data-value=\'Группировка видов\']\").text(\'Species grouping\');")
      shinyjs::runjs("$(\"a[data-value=\'Экспорт графиков\']\").text(\'Plot export\');")
      shinyjs::runjs("$(\"a[data-value=\'Общий график\']\").text(\'Overall plot\');")
    } else {
      shinyjs::runjs("$(\"a[data-value=\'Species analysis\']\").text(\'Анализ видов\');")
      shinyjs::runjs("$(\"a[data-value=\'Data cleaning\']\").text(\'Очистка данных\');")
      shinyjs::runjs("$(\"a[data-value=\'Before/after comparison\']\").text(\'Сравнение до/после\');")
      shinyjs::runjs("$(\"a[data-value=\'Species grouping\']\").text(\'Группировка видов\');")
      shinyjs::runjs("$(\"a[data-value=\'Plot export\']\").text(\'Экспорт графиков\');")
      shinyjs::runjs("$(\"a[data-value=\'Overall plot\']\").text(\'Общий график\');")
    }
  })

  output$readme_content <- renderUI({
    if (identical(input$ui_lang, "en")) {
      tagList(
        tags$p("The app can work with built-in demo data (default) or your own Excel file (.xlsx)."),
        tags$p("For successful upload and processing, your table must include these required columns:"),
        tags$ul(
          tags$li(tags$b("species"), " — species latin name (text)."),
          tags$li(tags$b("species_name_ru"), " — local/common species name (text)."),
          tags$li(tags$b("length"), " — fish length (numeric > 0)."),
          tags$li(tags$b("weight"), " — fish weight (numeric > 0)."),
          tags$li(tags$b("maxlength"), " — species maximum SL length (numeric > 0).")
        ),
        tags$hr(),
        tags$h4("Recommended optional columns"),
        tags$ul(
          tags$li(tags$b("Family"), " — family (text)."),
          tags$li(tags$b("Salt"), " — salinity/water type (text).")
        ),
        tags$hr(),
        tags$h4("Important processing rules"),
        tags$ul(
          tags$li("The file must be in .xlsx format."),
          tags$li("Column names must match exactly: species, species_name_ru, length, weight, maxlength."),
          tags$li("Rows with missing or non-positive length/weight/maxlength are removed automatically."),
          tags$li("After filtering, each species must have at least 7 observations, otherwise it is excluded.")
        )
      )
    } else {
      tagList(
        tags$p("Приложение может работать со встроенными данными (по умолчанию) или с вашим Excel-файлом (.xlsx)."),
        tags$p("Чтобы загрузка и обработка прошли успешно, в таблице должны быть следующие обязательные столбцы:"),
        tags$ul(
          tags$li(tags$b("species"), " — название вида (текст/строка)."),
          tags$li(tags$b("species_name_ru"), " — местное/русское название вида (текст/строка)."),
          tags$li(tags$b("length"), " — длина рыбы (число > 0)."),
          tags$li(tags$b("weight"), " — масса рыбы (число > 0)."),
          tags$li(tags$b("maxlength"), " — максимальная длина SL для вида (число > 0).")
        ),
        tags$hr(),
        tags$h4("Рекомендуемые дополнительные столбцы"),
        tags$ul(
          tags$li(tags$b("Family"), " — семейство (текст)."),
          tags$li(tags$b("Salt"), " — солёность/тип воды (текст, категориальный признак).")
        ),
        tags$hr(),
        tags$h4("Важные условия обработки"),
        tags$ul(
          tags$li("Файл должен быть в формате .xlsx."),
          tags$li("Столбцы species, species_name_ru, length, weight, maxlength должны называться точно так же."),
          tags$li("Строки с пустыми значениями и нечисловыми/неположительными length, weight или maxlength удаляются автоматически."),
          tags$li("Для каждого вида после фильтрации должно остаться не менее 7 наблюдений, иначе вид исключается из анализа.")
        )
      )
    }
  })

  observeEvent(input$upload_data_file, {
    req(input$upload_data_file)

    tryCatch({
      uploaded_raw <- read_excel(input$upload_data_file$datapath)
      prepared_uploaded <- prepare_species_data(uploaded_raw)

      if (nrow(prepared_uploaded$data) == 0) {
        stop("После фильтрации не осталось данных (нужно минимум 7 наблюдений на вид).")
      }

      current_data(prepared_uploaded$data)
      current_source_name(paste0(i18n()$uploaded_prefix, ": ", input$upload_data_file$name))

      clean_all_results$result <- NULL
      clean_all_results$timestamp <- NULL
      clean_all_results$gap_filtered <- NULL
      clean_all_results$gap_diagnostics <- NULL

      species_list <- sort(unique(prepared_uploaded$data$species))
      updateSelectInput(session, "species", choices = species_list, selected = species_list[1])

      showNotification(i18n()$upload_ok, type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste(i18n()$upload_err, e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$reset_default_data, {
    current_data(data)
    current_source_name(i18n()$built_in)

    clean_all_results$result <- NULL
    clean_all_results$timestamp <- NULL
    clean_all_results$gap_filtered <- NULL
    clean_all_results$gap_diagnostics <- NULL

    species_list <- sort(unique(current_data()$species))
    updateSelectInput(session, "species", choices = species_list, selected = species_list[1])

    showNotification(i18n()$reset_msg, type = "message", duration = 4)
  })

  output$data_source_info <- renderText({
    tr <- i18n()
    paste0(tr$source_prefix, ": ", current_source_name(), " | ", tr$rows, ": ", nrow(current_data()), " | ", tr$species, ": ", length(unique(current_data()$species)))
  })

  output$download_active_data <- downloadHandler(
    filename = function() {
      paste0("исходные_данные_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Данные")
      writeData(wb, "Данные", current_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

output$download_cleaned_btn1 <- downloadHandler(
  filename = function() {
    paste0("очищенные_данные_", Sys.Date(), ".xlsx")
  },
  
  content = function(file) {
    
    showNotification(
      "📊 Подготовка файла для скачивания...",
      type = "message",
      duration = 3
    )
    
    tryCatch({
      
      # Получаем данные для экспорта
      export_result <- create_cleaned_data_export(clean_all_results)
      
      if (is.null(export_result)) {
        showNotification(
          "❌ Нет данных для экспорта. Сначала выполните очистку.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      # Создаем Excel файл
      wb <- createWorkbook()
      
      # ---- Лист 1: Очищенные данные ----
      addWorksheet(wb, "Очищенные данные")
      writeData(wb, "Очищенные данные", export_result$data)
      
      # ---- Лист 2: Статистика ----
      addWorksheet(wb, "Статистика")
      writeData(wb, "Статистика", export_result$stats)
      
      # ---- Лист 3: Фильтрация по разрывам ----
      if (!is.null(clean_all_results$gap_diagnostics)) {
        
        gap_info <- clean_all_results$gap_diagnostics %>%
          mutate(
            Прошел_фильтр = ifelse(pass_gap_filter, "Да", "Нет"),
            Разрыв_проценты = round(gap_percent, 1)
          ) %>%
          select(
            Вид = species,
            `Макс. разрыв (см)` = max_gap_cm,
            `Разрыв (% диапазона)` = Разрыв_проценты,
            `Прошел фильтр` = Прошел_фильтр
          )
        
        addWorksheet(wb, "Фильтрация по разрывам")
        writeData(wb, "Фильтрация по разрывам", gap_info)
      }
      
      # Сохраняем файл
      saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification(
        paste("✅ Файл готов к скачиванию:", basename(file)),
        type = "success",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste("❌ Ошибка при экспорте:", e$message),
        type = "error",
        duration = 5
      )
    })
  }
)

output$gap_calculation_example <- renderPrint({
  req(input$gap_Lmin, input$gap_threshold)
  
  Lmin <- input$gap_Lmin
  gap_threshold <- input$gap_threshold
  
  cat("📐 ФОРМУЛА РАСЧЕТА РАЗРЫВОВ\n")
  cat("════════════════════════════════════════════════════\n\n")
  
  # Примерный расчет
  maxlength_example <- 30
  if (Lmin >= maxlength_example - 1) {
    maxlength_example <- Lmin + 15
  }
  
  total_range <- maxlength_example - Lmin
  max_allowed_gap <- total_range * (gap_threshold / 100)
  
  cat(sprintf("Lmin (начало диапазона) = %.1f см\n", Lmin))
  cat(sprintf("maxlength (конец диапазона) = %.1f см\n", maxlength_example))
  cat(sprintf("Общий диапазон = %.1f см\n\n", total_range))
  
  cat(sprintf("ДОПУСТИМЫЙ РАЗРЫВ = %.1f%% × %.1f см = %.1f см\n", 
              gap_threshold, total_range, max_allowed_gap))
  cat("\n")
 
})


  
 # Пример расчета разрывов
output$gap_visualization_plot <- renderPlot({
  req(input$gap_Lmin, input$gap_threshold)
  
  tryCatch({
    Lmin <- as.numeric(input$gap_Lmin)
    gap_threshold <- as.numeric(input$gap_threshold)
    
    # Автоматически подбираем maxlength для хорошей визуализации
    maxlength <- max(30, Lmin + 20)  # Минимум 20 см диапазона
    
    gap_visualization_plot(
      Lmin = Lmin,
      gap_threshold = gap_threshold,
      maxlength = maxlength
    )
    
  }, error = function(e) {
    # Простой график с сообщением об ошибке
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Ошибка построения графика", 
               size = 5, color = "red") +
      theme_void() +
      labs(title = "Ошибка")
  })
})
  
  
  observe({
  
  has_clean_data <- !is.null(clean_all_results$result) ||
                    !is.null(clean_all_results$gap_filtered)
  
  if (has_clean_data) {
    shinyjs::enable("download_cleaned_btn1")
  } else {
    shinyjs::disable("download_cleaned_btn1")
  }
})
  
  
  
#----------   ГРУППИРОВКА --------------------------------------------------------------------
  # 1. Запуск группировки
  observeEvent(input$run_grouping, {
    req(cleaned_data_for_comparison())
    
    showModal(modalDialog(
      title = "Запуск группировки...",
      "Пожалуйста, подождите. Это может занять несколько секунд.",
      footer = NULL,
      size = "l"
    ))
    
    tryCatch({
      data_for_grouping <- cleaned_data_for_comparison()$clean_data
      
      if(is.null(data_for_grouping)) {
        showNotification("Сначала выполните очистку данных", 
                         type = "warning", duration = 5)
        return()
      }
      
  output$groups_summary_table <- renderDT({
  req(grouping_state$grouped)
  
  summary_table <- create_groups_summary_table(grouping_state$grouped)
  
  datatable(
    summary_table,
    rownames = FALSE,
    options = list(
      pageLength = 20,
      dom = 'Bfrtip',
      scrollX = TRUE,
      buttons = list('copy', 'csv', 'excel')
    ),
    extensions = 'Buttons',
    caption = "Сводная таблица всех групп"
  ) %>%
    formatStyle('Тип группы',
                backgroundColor = styleEqual(
                  c("Мульти-видовая", "Одиночная"), 
                  c('#d4edda', '#f8d7da')
                ))
})
      
      
      
      
      # Строим модели
      grouping_state$models <- build_species_models(
        data = data_for_grouping,
        min_n = input$grouping_min_n
      )
      
      # Группируем
      grouping_state$grouped <- group_by_curve_similarity_combined(
        tbl = grouping_state$models,
        max_diff = input$grouping_max_diff / 100,
        max_growth = input$grouping_max_growth,
        points_per_cm = 10
      )
      
      grouping_state$current_group <- 1

      # Обновляем доступные мульти-видовые группы (>1 вид)
      multi_group_ids <- grouping_state$grouped$table %>%
        count(group, name = "n_species") %>%
        filter(n_species > 1) %>%
        pull(group) %>%
        sort()
      grouping_state$multi_groups <- multi_group_ids
      grouping_state$current_group <- if (length(multi_group_ids) > 0) multi_group_ids[1] else NA_integer_

      # Выбор групп для главной вкладки "Общий график" (только мульти-видовые)
      group_choices <- as.character(multi_group_ids)
      names(group_choices) <- paste("Группа", group_choices)
      updateCheckboxGroupInput(
        session,
        "main_overall_species_select",
        choices = group_choices,
        selected = group_choices
      )

      singleton_tbl <- grouping_state$grouped$table %>%
        group_by(group) %>%
        filter(n() == 1) %>%
        ungroup() %>%
        arrange(species)
      species_choices <- singleton_tbl$species
      species_labels <- singleton_tbl$species
      names(species_choices) <- species_labels
      updateCheckboxGroupInput(
        session,
        "single_species_select",
        choices = species_choices,
        selected = species_choices
      )
      
      showNotification("✅ Группировка завершена", 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("❌ Ошибка при группировке:", e$message), 
                       type = "error", duration = 10)
    }, finally = {
      removeModal()
    })
  })
  
  # 2. Навигация по группам
  observeEvent(input$group_next, {
    groups <- grouping_state$multi_groups
    if (length(groups) > 0 && !is.na(grouping_state$current_group)) {
      idx <- match(grouping_state$current_group, groups)
      idx <- ifelse(is.na(idx), 1, idx)
      grouping_state$current_group <- groups[ifelse(idx == length(groups), 1, idx + 1)]
    }
  })
  
  observeEvent(input$group_prev, {
    groups <- grouping_state$multi_groups
    if (length(groups) > 0 && !is.na(grouping_state$current_group)) {
      idx <- match(grouping_state$current_group, groups)
      idx <- ifelse(is.na(idx), 1, idx)
      grouping_state$current_group <- groups[ifelse(idx == 1, length(groups), idx - 1)]
    }
  })
  
  # 3. Информация о группировке
  output$grouping_info <- renderPrint({
    if(is.null(grouping_state$grouped)) {
      cat("Группировка не выполнена.\n")
      cat("1. Сначала выполните очистку данных\n")
      cat("2. Затем нажмите 'Запустить группировку'\n")
      return()
    }
    
    tbl <- grouping_state$grouped$table
    groups <- grouping_state$multi_groups

    if (length(groups) == 0 || is.na(grouping_state$current_group)) {
      cat("Нет групп с количеством видов > 1.\n")
      return()
    }

    current <- grouping_state$current_group
    current_idx <- match(current, groups)
    cat(sprintf("Группа %d из %d (только мульти-видовые)\n", current_idx, length(groups)))

    group_data <- tbl %>% filter(group == current)
    cat(sprintf("ID группы: %d\n", current))
    cat(sprintf("Видов в группе: %d\n", nrow(group_data)))
    cat(sprintf("Наблюдений: %d\n", sum(group_data$n)))

    species_names <- unique(group_data$species)
    cat("Виды в группе:\n")
    cat(paste0(" - ", species_names), sep = "\n")
  })
  
  # 4. График текущей группы
output$grouping_plot <- renderPlot({
  req(grouping_state$grouped, grouping_state$current_group)
  
  create_group_plot(
    grouped_data = grouping_state$grouped,
    current_group = grouping_state$current_group,
    show_mean = input$show_group_mean,
    show_ci = input$show_group_ci,
    ci_width = input$group_ci_width,
    power = 3  # Можно сделать настройкой
  )
})
  
  # 5. Таблица текущей группы
  output$grouping_table <- renderDT({
    req(grouping_state$grouped, grouping_state$current_group)
    
    tbl_grp <- grouping_state$grouped$table %>% 
      filter(group == grouping_state$current_group) %>%
      mutate(
        a = round(a, 6),
        b = round(b, 3),
        maxlength = round(maxlength, 1)
      ) %>%
      select(
        Вид = species,
        Наблюдения = n,
        `maxlength` = maxlength,
        a = a,
        b = b
      )
    
    datatable(tbl_grp, rownames = FALSE, options = list(pageLength = 10))
  })
  
  # 6. Статистика группировки
  output$grouping_stats <- renderPrint({
  if(is.null(grouping_state$grouped)) {
    cat("Группировка не выполнена.\n")
    cat("1. Сначала выполните очистку данных\n")
    cat("2. Затем нажмите 'Запустить группировку'\n")
    return()
  }
  
  # Собираем параметры группировки
  grouping_params <- list(
    min_n = input$grouping_min_n,
    max_diff = input$grouping_max_diff,
    max_growth = input$grouping_max_growth
  )
  
  stats_text <- format_grouping_stats(
    grouped_data = grouping_state$grouped,
    grouping_params = grouping_params
  )
  cat(stats_text)
})
  
  # 7. График размеров групп
  output$group_sizes_plot <- renderPlot({
    req(grouping_state$grouped)
    
    create_group_sizes_plot(grouping_state$grouped)
  })

  # Новые вкладки из Definite_3.R
  observeEvent(input$main_overall_select_all, {
    req(grouping_state$grouped)
    selected_groups <- grouping_state$grouped$table %>%
      count(group, name = "n_species") %>%
      filter(n_species > 1) %>%
      pull(group) %>%
      as.character()
    updateCheckboxGroupInput(session, "main_overall_species_select", selected = selected_groups)
  })

  observeEvent(input$main_overall_clear_all, {
    updateCheckboxGroupInput(session, "main_overall_species_select", selected = character(0))
  })

  observeEvent(input$single_select_all, {
    req(grouping_state$grouped)
    singleton_species <- grouping_state$grouped$table %>%
      group_by(group) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      arrange(species) %>%
      pull(species)
    updateCheckboxGroupInput(session, "single_species_select", selected = singleton_species)
  })

  observeEvent(input$single_clear_all, {
    updateCheckboxGroupInput(session, "single_species_select", selected = character(0))
  })

  output$clustering_stats <- renderPrint({
    cat(create_clustering_stats_text(grouping_state$grouped))
  })

  output$summary_table <- renderDT({
    req(grouping_state$grouped)
    datatable(create_groups_summary_table(grouping_state$grouped), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$param_distribution_plot <- renderPlot({
    create_group_parameter_distribution_plot(grouping_state$grouped)
  })

  output$cluster_size_plot <- renderPlot({
    req(grouping_state$grouped)
    create_group_sizes_plot(grouping_state$grouped)
  })

  output$single_plot <- renderPlot({
    create_single_species_plot(
      grouped_data = grouping_state$grouped,
      selected_species = input$single_species_select,
      show_groups = isTRUE(input$single_show_groups),
      log_space = isTRUE(input$single_log_space)
    )
  })

  observe({
    groups <- grouping_state$multi_groups
    if (length(groups) == 0) {
      grouping_state$current_group <- NA_integer_
    } else if (is.na(grouping_state$current_group) || !(grouping_state$current_group %in% groups)) {
      grouping_state$current_group <- groups[1]
    }
  })

  output$overall_plot <- renderPlot({
    req(grouping_state$grouped)

    current_group <- grouping_state$current_group
    if (is.na(current_group)) {
      return(create_empty_plot("Нет групп с количеством видов > 1"))
    }

    group_tbl <- grouping_state$grouped$table %>%
      filter(group == current_group)

    if (nrow(group_tbl) <= 1) {
      return(create_empty_plot("Текущая группа не является мульти-видовой"))
    }

    safe_x <- suppressWarnings(max(group_tbl$maxlength, na.rm = TRUE) * 1.05)
    safe_y <- suppressWarnings(max(group_tbl$a * (group_tbl$maxlength ^ group_tbl$b), na.rm = TRUE) * 1.1)
    if (!is.finite(safe_x) || is.na(safe_x) || safe_x <= 0) safe_x <- 80
    if (!is.finite(safe_y) || is.na(safe_y) || safe_y <= 0) safe_y <- 5000

    create_overall_groups_plot(
      grouped_data = grouping_state$grouped,
      selected_groups = current_group,
      show_mean = isTRUE(input$show_group_mean),
      show_ci = isTRUE(input$show_group_ci),
      ci_width = input$group_ci_width %||% 10,
      alpha_funnel = 0.18,
      x_max = safe_x,
      y_max = safe_y,
      mean_size = 1.6,
      show_bounds = TRUE,
      show_legend = TRUE,
      style = "color",
      font_size = 13,
      color_mode = "species"
    ) + labs(title = paste0("Группа ", current_group))
  })

  output$overall_formulas_table <- renderDT({
    req(grouping_state$grouped)

    current_group <- grouping_state$current_group
    if (is.na(current_group)) {
      return(datatable(data.frame(Сообщение = "Нет мульти-видовых групп"), rownames = FALSE))
    }

    group_tbl <- grouping_state$grouped$table %>%
      filter(group == current_group)

    if (nrow(group_tbl) == 0) {
      return(datatable(data.frame(Сообщение = "Нет данных по текущей группе"), rownames = FALSE))
    }

    overall_a <- weighted.mean(group_tbl$a, w = group_tbl$n, na.rm = TRUE)
    overall_b <- weighted.mean(group_tbl$b, w = group_tbl$n, na.rm = TRUE)

    overall_row <- tibble(
      Тип = "Общая регрессия группы",
      Вид = paste0("Группа ", current_group),
      Наблюдения = sum(group_tbl$n, na.rm = TRUE),
      a = round(overall_a, 6),
      b = round(overall_b, 3),
      Формула = sprintf("W = %.5f × L^%.3f", overall_a, overall_b)
    )

    species_rows <- group_tbl %>%
      transmute(
        Тип = "Регрессия вида",
        Вид = species,
        Наблюдения = n,
        a = round(a, 6),
        b = round(b, 3),
        Формула = sprintf("W = %.5f × L^%.3f", a, b)
      )

    table_data <- bind_rows(overall_row, species_rows)

    datatable(
      table_data,
      rownames = FALSE,
      options = list(pageLength = 12, ordering = FALSE),
      caption = paste0("Формулы регрессий для группы ", current_group)
    ) %>%
      formatStyle(
        "Тип",
        target = "row",
        fontWeight = styleEqual("Общая регрессия группы", "bold"),
        backgroundColor = styleEqual("Общая регрессия группы", "#eef7ff")
      )
  })

  main_overall_plot_object <- reactive({
    req(grouping_state$grouped)

    selected_groups <- suppressWarnings(as.numeric(input$main_overall_species_select))
    selected_groups <- selected_groups[!is.na(selected_groups)]

    if (length(selected_groups) == 0) {
      selected_groups <- grouping_state$grouped$table %>%
        count(group, name = "n_species") %>%
        filter(n_species > 1) %>%
        pull(group)
    }

    create_overall_groups_plot(
      grouped_data = grouping_state$grouped,
      selected_groups = selected_groups,
      show_mean = isTRUE(input$main_overall_show_mean),
      show_ci = isTRUE(input$main_overall_show_bounds),
      ci_width = 10,
      alpha_funnel = input$main_overall_funnel_alpha %||% 0.2,
      x_max = input$main_overall_x_max %||% 80,
      y_max = input$main_overall_y_max %||% 5000,
      mean_size = input$main_overall_mean_size %||% 1.5,
      show_bounds = isTRUE(input$main_overall_show_bounds),
      show_legend = isTRUE(input$main_overall_show_legend),
      style = input$main_overall_style %||% "color",
      font_size = input$main_overall_font_size %||% 12,
      color_mode = "group"
    )
  })

  output$main_overall_plot <- renderPlot({
    main_overall_plot_object()
  })

  output$main_overall_formulas_table <- renderDT({
    selected_groups <- suppressWarnings(as.numeric(input$main_overall_species_select))
    selected_groups <- selected_groups[!is.na(selected_groups)]

    datatable(
      create_overall_formulas_table(grouping_state$grouped, selected_groups = selected_groups),
      rownames = FALSE,
      options = list(pageLength = 8)
    )
  })


  observeEvent(input$main_overall_export, {
    req(grouping_state$grouped)

    export_folder <- input$export_folder %||% "plotsResult"
    if (!dir.exists(export_folder)) dir.create(export_folder, recursive = TRUE)

    export_format <- input$export_format %||% "png"
    export_dpi <- input$export_dpi %||% 300
    export_name <- paste0(input$export_filename %||% "overall_plot", "_main_overall_", Sys.Date(), ".", export_format)
    out_file <- file.path(export_folder, export_name)

    ggsave(
      filename = out_file,
      plot = main_overall_plot_object(),
      width = input$main_overall_export_width %||% 20,
      height = input$main_overall_export_height %||% 15,
      units = "cm",
      dpi = export_dpi
    )

    showNotification(paste("✅ Экспорт выполнен:", out_file), type = "message", duration = 8)
  })

  output$export_info_simple <- renderUI({
    if (is.null(grouping_state$grouped) || is.null(grouping_state$grouped$table)) {
      return(tags$p("Нет данных для экспорта"))
    }

    group_stats <- grouping_state$grouped$table %>%
      group_by(group) %>%
      summarise(n_species = n(), .groups = "drop")

    n_groups_multi <- sum(group_stats$n_species > 1)
    n_species <- nrow(grouping_state$grouped$table)

    tags$div(
      tags$p(tags$b("Групп для экспорта (n > 1):"), n_groups_multi),
      tags$p(tags$b("Видов:"), n_species)
    )
  })

  output$export_groups_list <- renderUI({
    req(grouping_state$grouped)

    group_stats <- grouping_state$grouped$table %>%
      group_by(group) %>%
      summarise(
        n_species = n(),
        species_names = paste(species, collapse = ", "),
        .groups = "drop"
      ) %>%
      filter(n_species > 1) %>%
      arrange(group)

    if (nrow(group_stats) == 0) {
      return(tags$p("Нет групп с количеством видов > 1"))
    }

    tagList(
      tags$p(tags$strong("Группы для экспорта:")),
      lapply(seq_len(nrow(group_stats)), function(i) {
        tags$div(
          style = "margin-bottom: 5px;",
          tags$strong(sprintf("Группа %d (%d видов):", group_stats$group[i], group_stats$n_species[i])),
          tags$br(),
          tags$span(style = "font-size: 0.9em; color: #666;", group_stats$species_names[i])
        )
      })
    )
  })

  output$export_grid_preview <- renderPlot({
    req(grouping_state$grouped)

    create_export_preview_plot(
      grouped_data = grouping_state$grouped,
      ncol = input$export_ncol %||% 2,
      style = input$export_style %||% "color",
      font_size = input$export_font_size %||% 10,
      title_size_mult = (input$export_title_size %||% 90) / 100,
      axis_size_mult = (input$export_axis_size %||% 80) / 100,
      show_titles = isTRUE(input$export_show_titles),
      preview_only = TRUE
    )
  })

  output$preview_export_plot <- renderPlot({
    req(grouping_state$grouped)
    create_export_preview_plot(
      grouped_data = grouping_state$grouped,
      ncol = input$export_ncol %||% 2,
      style = input$export_style %||% "color",
      font_size = input$export_font_size %||% 10,
      title_size_mult = (input$export_title_size %||% 90) / 100,
      axis_size_mult = (input$export_axis_size %||% 80) / 100,
      show_titles = isTRUE(input$export_show_titles),
      preview_only = TRUE
    )
  })

  observeEvent(input$preview_grid, {
    output$export_grid_preview <- renderPlot({
      req(grouping_state$grouped)
      create_export_preview_plot(
        grouped_data = grouping_state$grouped,
        ncol = input$export_ncol %||% 2,
        style = input$export_style %||% "color",
        font_size = input$export_font_size %||% 10,
        title_size_mult = (input$export_title_size %||% 90) / 100,
        axis_size_mult = (input$export_axis_size %||% 80) / 100,
        show_titles = isTRUE(input$export_show_titles),
        preview_only = TRUE
      )
    })
  })

  output$download_export_plot <- downloadHandler(
    filename = function() {
      fmt <- input$export_format %||% "png"
      paste0(input$export_filename %||% "all_groups", "_preview_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      plot_obj <- create_export_preview_plot(
        grouped_data = grouping_state$grouped,
        ncol = input$export_ncol %||% 2,
        style = input$export_style %||% "color",
        font_size = input$export_font_size %||% 10,
        title_size_mult = (input$export_title_size %||% 90) / 100,
        axis_size_mult = (input$export_axis_size %||% 80) / 100,
        show_titles = isTRUE(input$export_show_titles),
        preview_only = TRUE
      )

      ggsave(
        filename = file,
        plot = plot_obj,
        width = input$export_width %||% 21,
        height = input$export_height %||% 29.7,
        units = "cm",
        dpi = input$export_dpi %||% 300
      )
    }
  )

  observeEvent(input$export_grid, {
    req(grouping_state$grouped)

    export_folder <- input$export_folder %||% "plotsResult"
    if (!dir.exists(export_folder)) dir.create(export_folder, recursive = TRUE)

    full_plot <- create_export_preview_plot(
      grouped_data = grouping_state$grouped,
      ncol = input$export_ncol %||% 2,
      style = input$export_style %||% "color",
      font_size = input$export_font_size %||% 10,
      title_size_mult = (input$export_title_size %||% 90) / 100,
      axis_size_mult = (input$export_axis_size %||% 80) / 100,
      show_titles = isTRUE(input$export_show_titles),
      preview_only = FALSE
    )

    out_file <- file.path(
      export_folder,
      paste0(input$export_filename %||% "all_groups", "_", Sys.Date(), ".", input$export_format %||% "png")
    )

    ggsave(
      filename = out_file,
      plot = full_plot,
      width = input$export_width %||% 21,
      height = input$export_height %||% 29.7,
      units = "cm",
      dpi = input$export_dpi %||% 300
    )

    showNotification(paste("✅ Сетка графиков сохранена:", out_file), type = "message", duration = 8)
  })

  observeEvent(input$export_individual, {
    req(grouping_state$grouped)

    export_folder <- input$export_folder %||% "plotsResult"
    if (!dir.exists(export_folder)) dir.create(export_folder, recursive = TRUE)

    group_stats <- grouping_state$grouped$table %>%
      group_by(group) %>%
      summarise(n_species = n(), .groups = "drop") %>%
      filter(n_species > 1) %>%
      arrange(group)

    if (nrow(group_stats) == 0) {
      showNotification("Нет групп с количеством видов > 1 для экспорта", type = "warning", duration = 6)
      return()
    }

    for (i in seq_len(nrow(group_stats))) {
      group_id <- group_stats$group[i]
      group_data <- grouping_state$grouped$table %>% filter(group == group_id)

      p_group <- create_group_plot_for_grid(
        group_data = group_data,
        group_id = group_id,
        style = input$export_style %||% "color",
        font_size = input$export_font_size %||% 10,
        title_size_mult = (input$export_title_size %||% 90) / 100,
        axis_size_mult = (input$export_axis_size %||% 80) / 100,
        show_title = isTRUE(input$export_show_titles)
      )

      out_file <- file.path(
        export_folder,
        sprintf("group_%02d_%d_species.%s", group_id, group_stats$n_species[i], input$export_format %||% "png")
      )

      ggsave(
        filename = out_file,
        plot = p_group,
        width = (input$export_width %||% 21) / max(1, input$export_ncol %||% 2),
        height = (input$export_height %||% 29.7) / max(1, if (isTRUE(input$export_auto_height)) ceiling(nrow(group_stats)/(input$export_ncol %||% 2)) else (input$export_nrow %||% 2)),
        units = "cm",
        dpi = input$export_dpi %||% 300
      )
    }

    showNotification("✅ Отдельные графики групп экспортированы", type = "message", duration = 8)
  })

# 8. Экспорт результатов группировки (НОВАЯ ВЕРСИЯ)
observeEvent(input$export_groups, {
  req(grouping_state$grouped)
  req(cleaned_data_for_comparison())
  
  showModal(modalDialog(
    title = "Экспорт данных...",
    "Формируется таблица для базы данных и Excel-файл.",
    footer = NULL,
    size = "m"
  ))
  
  tryCatch({
    cat("\n=== НАЧАЛО ЭКСПОРТА (NEW) ===\n")
    
    # 1. Получаем данные
    clean_data <- cleaned_data_for_comparison()$clean_data
    grouped_models <- grouping_state$grouped$table
    
    # 2. Строим каноническую таблицу для БД
    cat("Сборка big_db_table...\n")
    
    big_db_table <- build_big_db_table(
      cleaned_data   = clean_data,
      grouped_models = grouped_models
    )
    
    cat("big_db_table: строк =", nrow(big_db_table), "\n")
    
    # 3. Создаем Excel файл
    filename <- paste0("LW_big_table_for_DB2_", Sys.Date(), ".xlsx")
    cat("Имя файла:", filename, "\n")
    
    wb <- createWorkbook()
    
    # Лист 1: Каноническая таблица для БД
    addWorksheet(wb, "LW_big_table")
    writeData(wb, "LW_big_table", big_db_table)
    
    # 4. Стили и форматирование
    if(nrow(big_db_table) > 0) {
      setColWidths(wb, "LW_big_table", cols = 1:ncol(big_db_table), widths = "auto")
      
      headerStyle <- createStyle(
        textDecoration = "bold",
        halign = "center",
        border = "Bottom"
      )
      addStyle(wb, "LW_big_table", headerStyle, rows = 1, cols = 1:ncol(big_db_table))
    }
    
    # 5. Сохраняем файл
    saveWorkbook(wb, filename, overwrite = TRUE)
    
    cat("Файл успешно сохранен!\n")
    cat("=== КОНЕЦ ЭКСПОРТА (NEW) ===\n\n")
    
    removeModal()
    
    showNotification(
      HTML(paste(
        "✅ <b>Файл для БД и расчётов кормов сохранён:</b><br>",
        filename, "<br><br>",
        "📋 <b>Содержит каноническую таблицу:</b><br>",
        "&nbsp;&nbsp;• group_id<br>",
        "&nbsp;&nbsp;• species_latin / species_ru<br>",
        "&nbsp;&nbsp;• a_group / b_group<br>",
        "&nbsp;&nbsp;• a_species / b_species<br>",
        "&nbsp;&nbsp;• диапазоны измерений<br>",
        "&nbsp;&nbsp;• численность в группе и по виду<br><br>",
        "Всего строк: ", nrow(big_db_table)
      )),
      type = "message",
      duration = 15
    )
    
  }, error = function(e) {
    removeModal()
    cat("ОШИБКА ЭКСПОРТА:", e$message, "\n")
    print(traceback())
    
    showNotification(
      paste("❌ Ошибка при экспорте:", e$message),
      type = "error",
      duration = 10
    )
  })
})
  
  # 9. Предпросмотр данных группировки
  output$groups_preview <- renderDT({
    req(grouping_state$grouped)
    
    preview_data <- grouping_state$grouped$table %>%
      select(
        Группа = group,
        Вид = species,
        Наблюдения = n,
        `maxlength` = maxlength,
        a = a,
        b = b
      ) %>%
      head(50)
    
    datatable(preview_data, rownames = FALSE, options = list(pageLength = 10))
  })

  
#--------------------  КОНЕЦ ГРУППИРОВКА ------------------------------------------------------

    passed_compare_species <- reactive({
      req(cleaned_data_for_comparison())
      compare_data <- cleaned_data_for_comparison()$clean_data

      if (is.null(compare_data) || nrow(compare_data) == 0) {
        return(character(0))
      }

      compare_filtered <- if ("was_cleaned" %in% colnames(compare_data)) {
        compare_data %>% filter(was_cleaned == TRUE)
      } else {
        compare_data
      }

      compare_filtered %>%
        group_by(species) %>%
        summarise(n_points = n(), .groups = "drop") %>%
        filter(n_points >= 3) %>%
        pull(species) %>%
        as.character() %>%
        sort()
    })

    output$compare_cleaning_info <- renderUI({
      all_species <- sort(unique(as.character(current_data()$species)))

      if (is.null(clean_all_results$result)) {
        return(tags$div(
          class = "alert alert-warning",
          "Очистка ещё не запускалась. Список видов появится после выполнения очистки."
        ))
      }

      passed_species <- passed_compare_species()
      failed_species <- setdiff(all_species, passed_species)

      tags$div(
        class = "alert alert-info",
        tags$p(tags$b("Видов для сравнения (прошли очистку): "), length(passed_species)),
        tags$p(tags$b("Видов не прошли очистку: "), length(failed_species)),
        if (length(failed_species) > 0) {
          tags$div(
            tags$b("Список исключённых видов:"),
            tags$br(),
            paste(failed_species, collapse = ", ")
          )
        } else {
          tags$div("Все виды прошли очистку.")
        }
      )
    })

    observe({
      if (!identical(input$main_navbar, "Сравнение до/после")) {
        return()
      }

      passed_species <- passed_compare_species()
      if (length(passed_species) == 0) {
        return()
      }

      if (is.null(input$species) || !(input$species %in% passed_species)) {
        updateSelectInput(session, "species", selected = passed_species[1])
      }
    })
  
    species_data <- reactive({
      req(input$species)
      current_data() %>% 
        filter(species == input$species) %>%
        arrange(length)
    })
    
    raw_data_reactive <- reactive({
      species_data()
    })
    
cleaned_data_reactive <- reactive({
  if(!is.null(clean_all_results$gap_filtered)) {
    clean_all_results$gap_filtered
  } else if(!is.null(clean_all_results$result)) {
    clean_all_results$result$clean_data
  } else {
    NULL
  }
})
    

  
# Функция для модуля сравнения
cleaned_data_for_comparison <- reactive({
  
  # 1. Если есть данные после фильтрации разрывов - используем их
  if(!is.null(clean_all_results$gap_filtered)) {
    cat("\n=== COMPARE: Используем данные ПОСЛЕ фильтрации разрывов ===\n")
    cat("Видов после разрывов:", length(unique(clean_all_results$gap_filtered$species)), "\n")
    
    return(list(
      clean_data = clean_all_results$gap_filtered,  # ← ПОСЛЕ разрывов
      outliers = if(!is.null(clean_all_results$result$outliers)) {
        clean_all_results$result$outliers
      } else {
        NULL
      }
    ))
  }
  
  # 2. Если нет данных после разрывов, но есть после очистки выбросов
  else if(!is.null(clean_all_results$result)) {
    cat("\n=== COMPARE: Используем данные ДО фильтрации разрывов ===\n")
    cat("Видов до разрывов:", length(unique(clean_all_results$result$clean_data$species)), "\n")
    
    return(list(
      clean_data = clean_all_results$result$clean_data,  # ← ДО разрывов
      outliers = clean_all_results$result$outliers
    ))
  }
  
  # 3. Если очистка не выполнялась
  else {
    cat("\n=== COMPARE: Очистка не выполнялась ===\n")
    return(list(
      clean_data = NULL,
      outliers = NULL
    ))
  }
})




    model_type_reactive <- reactive({
      if (input$clean_model2 == "Экспоненциальная") {
        return("exp")
      } else {
        return("power")
      }
    })
    
    # Статистика модели
    model_stats_reactive <- reactive({
      sp_data <- species_data()
      calculate_model_stats(sp_data)
    })
    
    comparison_result <- reactive({
      stats <- model_stats_reactive()
      compare_models(stats)
    })
    
    outliers_data <- reactive({
      sp_data <- species_data()
      stats <- model_stats_reactive()
      detect_outliers(sp_data, stats, input$ribbon_percent)
    })
    
    predictions_with_ribbon <- reactive({
      sp_data <- species_data()
      stats <- model_stats_reactive()
      create_predictions_with_ribbon(sp_data, stats, input$ribbon_percent)
    })
    
    # Настройки графика
    plot_settings <- reactive({
      list(
        show_ribbon = input$show_ribbon,
        ribbon_model = input$ribbon_model,
        ribbon_alpha = input$ribbon_alpha,
        ribbon_percent = input$ribbon_percent,
        show_points = input$show_points,
        show_power = input$show_power,
        show_exp = input$show_exp,
        point_size = input$point_size,
        alpha = input$alpha,
        line_size = input$line_size,
        log_x = input$log_x,
        log_y = input$log_y
      )
    })
    
    # Построение графика с помощью модульной функции
    output$regression_plot <- renderPlot({
      sp_data <- species_data()
      pred <- predictions_with_ribbon()
      outliers <- outliers_data()
      
      create_regression_plot(
        sp_data = sp_data,
        pred_data = pred,
        outliers = outliers,
        highlight_outliers = highlight_outliers_flag(),
        plot_settings = plot_settings()
      )
    })
    
    # Флаг для подсветки выбросов
    highlight_outliers_flag <- reactiveVal(FALSE)
    
    # Обработчик кнопки подсветки выбросов
    observeEvent(input$highlight_outliers, {
      highlight_outliers_flag(!highlight_outliers_flag())
    })
    
    # Вывод результата сравнения под графиком
    output$model_comparison_result <- renderUI({
      result <- comparison_result()
      if(is.null(result)) return(NULL)
      
      # Определяем цвет и иконку
      if(result$best_model == "Модели примерно одинакового качества") {
        color <- "warning"
        icon <- "🟡"
        message <- "Модели примерно одинакового качества"
      } else if(result$best_model == "Экспоненциальная") {
        color <- "message"
        icon <- "🏆"
        message <- paste(icon, "Экспоненциальная модель", result$strength, "ЛУЧШЕ")
      } else {
        color <- "info"
        icon <- "🏆"
        message <- paste(icon, "Степенная модель", result$strength, "ЛУЧШЕ")
      }
      
      tags$div(
        class = paste("panel panel-", color),
        style = "margin-top: 20px; padding: 15px; border-radius: 10px;",
        tags$div(
          class = "panel-heading",
          style = "font-weight: bold; font-size: 16px;",
          "СРАВНЕНИЕ МОДЕЛЕЙ"
        ),
        tags$div(
          class = "panel-body",
          tags$p(
            style = "font-size: 14px;",
            tags$strong("AIC степенной модели: "), result$aic_power, tags$br(),
            tags$strong("AIC экспоненциальной модели: "), result$aic_exp, tags$br(),
            tags$strong("AIC (Exp - Power) = "), 
            tags$span(style = paste("color:", ifelse(result$delta_aic < 0, "green", "red"), ";"), 
                      result$delta_aic)
          ),
          tags$hr(),
          tags$div(
            style = "font-size: 16px; font-weight: bold; text-align: center; padding: 10px;",
            class = paste("alert alert-", color),
            message,
            if(result$best_model != "Модели примерно одинакового качества") {
              tags$span(style = "font-size: 14px; font-weight: normal; display: block; margin-top: 5px;",
                        paste("(AIC =", result$abs_delta, ")"))
            }
          )
        )
      )
    })
    
    # Легенда с помощью модульной функции
    output$legend_html <- renderUI({
      create_legend_html(
        plot_settings = plot_settings(),
        highlight_outliers = highlight_outliers_flag(),
        ribbon_percent = input$ribbon_percent
      )
    })
    
    # Информация о выбросах с помощью модульной функции
    output$outliers_info <- renderUI({
      outliers <- outliers_data()
      create_outliers_info_html(outliers, input$ribbon_percent)
    })
    
    # Подробная статистика
    output$model_stats <- renderPrint({
      stats <- model_stats_reactive()
      outliers <- if(input$show_ribbon) outliers_data() else NULL
      
      formatted_stats <- format_model_stats(
        stats = stats,
        species_name = input$species,
        ribbon_percent = if(input$show_ribbon) input$ribbon_percent else NULL,
        outliers_data = outliers
      )
      
      cat(formatted_stats)
    })
    
    # Таблица сравнения моделей
    output$model_comparison_table <- renderTable({
      stats <- model_stats_reactive()
      create_model_comparison_table(stats)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
    
    # Обработчик очистки с автоматической фильтрацией разрывов
    observeEvent(input$clean_all2, {
      showModal(modalDialog(
        title = "Запуск очистки с фильтрацией разрывов...",
        "Пожалуйста, подождите. Это может занять несколько минут.",
        footer = NULL,
        size = "l"
      ))
      
      tryCatch({
        # 1. Очистка выбросов
        result <- clean_all_species_correct(  
          data = current_data(),
          final_threshold = input$clean_final2,
          min_final_n = input$min_final_n,
          verbose = TRUE
        )
        
        # Сохраняем очищенные данные
        clean_all_results$result <- result
        clean_all_results$timestamp <- Sys.time()
        
        # 2. Автоматическая фильтрация по разрывам
        gap_filter_result <- filter_by_gaps(
          data = result$clean_data,
          Lmin = input$gap_Lmin,
          gap_threshold = input$gap_threshold / 100
        )
        
        # Сохраняем результаты фильтрации
        clean_all_results$gap_filtered <- gap_filter_result$filtered_data
        clean_all_results$gap_diagnostics <- gap_filter_result$gap_diagnostics
        



        # Уведомление
        showNotification(
          HTML(paste(
            "✅ <b>Очистка завершена!</b><br>",
            "📊 <b>Статистика:</b><br>",
            "&nbsp;&nbsp;• Исходно:", result$stats$data_counts$total_initial, "наблюдений<br>",
            "&nbsp;&nbsp;• После очистки:", nrow(result$clean_data), "<br>",
            "&nbsp;&nbsp;• После фильтра разрывов:", nrow(gap_filter_result$filtered_data), "<br>",
            "&nbsp;&nbsp;• Видов удалено по разрывам:", gap_filter_result$stats$removed_species 
            
          )),
          type = "message",
          duration = 15
        )
        
output$gap_calculation_RESULT <- renderPrint({

  cat("📐 Статисктика очистки:\n")
  cat("════════════════════════════════════════════════════\n\n")

      if (!is.null(result$stats$data_counts$total_initial)) {
        cat("Исходное количество наблюдений:", result$stats$data_counts$total_initial, "\n")
      }
      
      if (!is.null(result$stats$data_counts$total_final)) {
        cat("После очистки:", result$stats$data_counts$total_final, "\n")
      }
      
      if (!is.null(result$stats$data_counts$total_outliers)) {
        cat("Удалено выбросов:", result$stats$data_counts$total_outliers, "\n")
      }
      
      if (!is.null(result$stats$data_counts$total_removed)) {
        cat("Всего удалено:", result$stats$data_counts$total_removed, "\n")
      }


    if(!is.null(gap_filter_result)) {
      cat("\n=== ФИЛЬТРАЦИЯ ПО РАЗРЫВАМ ===\n")
      cat(sprintf("Удалено видов по разрывам: %d\n", 
                  gap_filter_result$stats$removed_species))
      cat(sprintf("Осталось видов: %d\n", 
                  length(species_to_keep) - gap_filter_result$stats$removed_species))
                  
                  
    }


})





      }, error = function(e) {
        showNotification(paste("❌ Ошибка при очистке:", e$message), 
                         type = "error", duration = 10)
      }, finally = {
        removeModal()
      })
    })
    
    # ВЫВОД СТАТИСТИКИ
    
    output$cleaning_stats <- renderPrint({
      if(is.null(clean_all_results$result)) {
        cat("Очистка еще не выполнялась.\n")
        cat("Нажмите кнопку 'Запустить очистку' для начала процесса.\n")
        return()
      }
      
      gap_results <- if(!is.null(clean_all_results$gap_diagnostics)) {
        list(
          stats = list(
            passed_species = length(unique(clean_all_results$gap_filtered$species)),
            removed_species = length(unique(clean_all_results$result$clean_data$species)) - 
                              length(unique(clean_all_results$gap_filtered$species))
          )
        )
      } else {
        NULL
      }
      
      formatted_stats <- format_cleaning_stats(
        cleaning_result = clean_all_results$result,
        gap_results = gap_results,
        gap_Lmin = input$gap_Lmin,
        gap_threshold = input$gap_threshold
      )
      
      cat(formatted_stats)
    })
    
    output$gap_filter_stats <- renderPrint({
      if(is.null(clean_all_results$gap_diagnostics)) {
        cat("Фильтрация по разрывам не применялась.\n")
        return()
      }
      
      gap_results <- list(
        filtered_data = clean_all_results$gap_filtered,
        gap_diagnostics = clean_all_results$gap_diagnostics,
        stats = list(
          total_species = length(unique(clean_all_results$result$clean_data$species)),
          passed_species = length(unique(clean_all_results$gap_filtered$species)),
          removed_species = length(unique(clean_all_results$result$clean_data$species)) - 
                            length(unique(clean_all_results$gap_filtered$species)),
          removed_percent = round(100 * 
            (length(unique(clean_all_results$result$clean_data$species)) - 
             length(unique(clean_all_results$gap_filtered$species))) / 
            length(unique(clean_all_results$result$clean_data$species)), 1)
        )
      )
      
      formatted_stats <- format_gap_filter_stats(
        gap_results = gap_results,
        Lmin = input$gap_Lmin,
        gap_threshold = input$gap_threshold
      )
      
      cat(formatted_stats)
    })

    # Предпросмотр очищенных данных
    output$cleaned_preview <- renderDT({
      if(is.null(clean_all_results$result)) {
        return(datatable(
          data.frame(Сообщение = "Данные еще не очищены. Нажмите кнопку 'Запустить очистку' для начала процесса."),
          options = list(pageLength = 5, dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Используем отфильтрованные данные, если они есть
      clean_data <- if(!is.null(clean_all_results$gap_filtered)) {
        clean_all_results$gap_filtered
      } else {
        clean_all_results$result$clean_data
      }
      
      available_cols <- colnames(clean_data)
      base_cols <- c("species", "length", "weight", "was_cleaned")
      optional_cols <- c("was_thinned", "iterations", "initial_n", 
                         "outliers_removed", "percent_removed")
      
      cols_to_select <- c(base_cols, 
                          intersect(optional_cols, available_cols))
      
      preview_data <- clean_data %>%
        select(any_of(cols_to_select)) %>%
        head(4000) %>%
        mutate(
          length = round(as.numeric(length), 1),
          weight = round(as.numeric(weight), 1)
        )
      
      if("percent_removed" %in% colnames(preview_data)) {
        preview_data$percent_removed <- round(preview_data$percent_removed, 1)
      }
      
      datatable(
        preview_data,
        options = list(
          pageLength = 300,
          dom = 'Bfrtip',
          buttons = list('copy', 'csv', 'excel'),
          scrollX = TRUE
        ),
        extensions = 'Buttons',
        caption = "Предпросмотр очищенных данных (первые 300 строк)",
        rownames = FALSE
      ) %>%
        formatStyle('was_cleaned',
                  backgroundColor = styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da')))
    })
    
    # Навигация по видам
    observeEvent(input$next_species, {
      species_list <- if (identical(input$main_navbar, "Сравнение до/после") && length(passed_compare_species()) > 0) {
        passed_compare_species()
      } else {
        sort(unique(as.character(current_data()$species)))
      }

      current <- which(species_list == input$species)
      if (length(current) == 0) current <- 1
      next_idx <- ifelse(current < length(species_list), current + 1, 1)
      updateSelectInput(session, "species", selected = species_list[next_idx])
    })
    
    observeEvent(input$prev_species, {
      species_list <- if (identical(input$main_navbar, "Сравнение до/после") && length(passed_compare_species()) > 0) {
        passed_compare_species()
      } else {
        sort(unique(as.character(current_data()$species)))
      }

      current <- which(species_list == input$species)
      if (length(current) == 0) current <- 1
      prev_idx <- ifelse(current > 1, current - 1, length(species_list))
      updateSelectInput(session, "species", selected = species_list[prev_idx])
    })
    
    # Подключение модуля сравнения до/после
   compare_before_after_server(input, output, session,
                            raw_data = raw_data_reactive,
                            clean_data = cleaned_data_for_comparison,
                            model_type = model_type_reactive)

  

  




    
}
  
  shinyApp(ui, server)
}

app <- create_interactive_comparison(data)
app
