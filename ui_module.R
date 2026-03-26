# ui_module.R

# Основной UI
create_main_ui <- function(species_counts, species_to_keep, data) {
  fluidPage(
    titlePanel(
      paste0(
        "Анализ длина-вес рыб. Исходное количество видов: ",
        length(unique(species_counts$species)),
        ". Видов с n ≥ 7: ",
        length(species_to_keep)
      )
    ),
    navbarPage(
      title = "Меню",
      id = "main_navbar",
      collapsible = TRUE,

      tabPanel(
        "Read me",
        icon = icon("book"),
        fluidRow(
          column(
            10,
            offset = 1,
            tags$div(
              class = "panel panel-default",
              style = "margin-top: 20px;",
              tags$div(
                class = "panel-heading",
                tags$h3("📘 Инструкция по формату входного файла", style = "margin: 0;")
              ),
              tags$div(
                class = "panel-body",
                selectInput(
                  "ui_lang",
                  "Language / Язык",
                  choices = c("Русский" = "ru", "English" = "en"),
                  selected = "ru"
                ),
                uiOutput("readme_content")
              )
            )
          )
        )
      ),
      
      # ============ ВКЛАДКА 1: АНАЛИЗ ОТДЕЛЬНЫХ ВИДОВ ============
     tabPanel(
  "Анализ видов",
  icon = icon("fish"),
  sidebarLayout(
    sidebarPanel = create_sidebar_panel(data),
    mainPanel = create_analysis_main_panel()
  )
),
      
      # ============ ВКЛАДКА 2: ОЧИСТКА ДАННЫХ ============
      tabPanel(
        "Очистка данных",
        icon = icon("broom"),
        sidebarLayout(
    sidebarPanel = create_cleaning_settings(),
    mainPanel = create_cleaning_main_panel()
        )
      ),
      tabPanel(
        "Сравнение до/после",
        sidebarLayout(
          sidebarPanel(
            width = 3,
        h4("Настройки сравнения:"),
        sliderInput(
          "compare_ribbon_percent",
          "Ширина доверительной полосы (%)",
          min = 0, max = 50, value = 40
        ),
        hr(),
        h4("Информация по очистке"),
        uiOutput("compare_cleaning_info"),
        hr(),
    create_navigation_buttons()),
     mainPanel(
        width = 9,
        
        plotOutput("compare_plot", height = "650px")
      ))),
      
      # ============ ВКЛАДКА 3: ГРУППИРОВКА ВИДОВ ============
      tabPanel(
        "Группировка видов",
        icon = icon("project-diagram"),
        sidebarLayout(
          sidebarPanel = create_grouping_sidebar(),
          mainPanel = create_grouping_main_panel()
        )
      ),

      tabPanel(
        "Экспорт графиков",
        icon = icon("file-export"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("⚙ Настройки экспорта"),
            htmlOutput("export_info_simple"),
            hr(),
            h4("📊 Настройки сетки"),
            sliderInput("export_ncol", "Графиков по ширине:", min = 1, max = 5, value = 2, step = 1),
            checkboxInput("export_auto_height", "Автоматическая высота", value = TRUE),
            conditionalPanel(
              condition = "!input.export_auto_height",
              numericInput("export_nrow", "Количество строк:", value = 2, min = 1, max = 10, step = 1)
            ),
            sliderInput("export_spacing", "Отступ между графиками:", min = 0.1, max = 2, value = 0.5, step = 0.1),
            hr(),
            h4("🔤 Настройки шрифтов"),
            sliderInput("export_font_size", "Основной размер шрифта:", min = 6, max = 16, value = 10, step = 0.5),
            sliderInput("export_title_size", "Размер заголовка (%):", min = 70, max = 130, value = 90, step = 5, post = "%"),
            sliderInput("export_axis_size", "Размер подписей осей (%):", min = 70, max = 120, value = 80, step = 5, post = "%"),
            checkboxInput("export_show_titles", "Показывать заголовки графиков", value = TRUE),
            hr(),
            h4("🎨 Настройки стиля"),
            radioButtons("export_style", "Стиль графика:",
                         choices = c("Цветной" = "color", "Черно-белый (линии)" = "bw", "Черно-белый (точки)" = "bw_dots", "Минималистичный" = "minimal"),
                         selected = "color"),
            hr(),
            h4("📐 Настройки размеров"),
            numericInput("export_width", "Ширина итогового файла (см):", value = 21, min = 10, max = 50, step = 1),
            numericInput("export_height", "Высота итогового файла (см):", value = 29.7, min = 10, max = 50, step = 1),
            numericInput("export_dpi", "Разрешение (DPI):", value = 300, min = 150, max = 600, step = 50),
            radioButtons("export_format", "Формат файла:", choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff"), selected = "png"),
            hr(),
h4("💾 Экспорт"),
textInput("export_filename", "Имя файла (без расширения):", value = "all_groups"),
downloadButton("download_export_grid", "📥 Скачать итоговую сетку", class = "btn-success"),
tags$div(style = "margin-bottom: 10px;"),
actionButton("export_individual", "📁 Экспортировать отдельно", class = "btn-primary", style = "width: 100%; font-weight: bold; margin-bottom: 10px;"),
actionButton("preview_grid", "👁 Обновить предпросмотр", class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
downloadButton("download_export_plot", "Скачать текущий предпросмотр", class = "btn-secondary")
          ),
          mainPanel(
            width = 9,
            withSpinner(plotOutput("export_grid_preview", height = "700px"), type = 6, color = "#0d6efd"),
            br(),
            h5("ℹ Информация о группах:"),
            htmlOutput("export_groups_list")
          )
        )
      ),

      tabPanel(
        "Общий график",
        icon = icon("chart-area"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("🎯 Выбор групп"),
            checkboxGroupInput("main_overall_species_select", "Выберите группы:", choices = NULL, selected = NULL, inline = TRUE),
            tags$style(HTML("#main_overall_species_select .shiny-options-group{column-count:2; column-gap:12px;} @media (max-width: 1400px){#main_overall_species_select .shiny-options-group{column-count:1;}} #main_overall_species_select .checkbox{margin-top:2px; margin-bottom:2px;}")),
            actionButton("main_overall_select_all", "Выбрать все", class = "btn-sm btn-primary"),
            actionButton("main_overall_clear_all", "Очистить", class = "btn-sm btn-danger"),
            hr(),
            h4("📊 Настройки воронок"),
            sliderInput("main_overall_funnel_alpha", "Прозрачность воронок:", min = 0.1, max = 0.5, value = 0.2, step = 0.05),
            checkboxInput("main_overall_show_mean", "Показывать средние линии", value = TRUE),
            conditionalPanel(
              condition = "input.main_overall_show_mean",
              sliderInput("main_overall_mean_size", "Толщина средних линий:", min = 1, max = 3, value = 1.5, step = 0.1),
              checkboxInput("main_overall_show_legend", "Показывать легенду", value = TRUE)
            ),
            checkboxInput("main_overall_show_bounds", "Показывать границы воронок", value = TRUE),
            hr(),
            h4("🎨 Настройки отображения"),
            radioButtons("main_overall_style", "Стиль графика:", choices = c("Цветной" = "color", "Черно-белый" = "bw", "Только средние линии" = "mean_only"), selected = "color"),
            sliderInput("main_overall_font_size", "Размер шрифта:", min = 10, max = 18, value = 12, step = 0.5),
            hr(),
            h4("📐 Настройки осей"),
            numericInput("main_overall_x_max", "Макс. длина (см):", value = 50, min = 10, max = 200, step = 5),
            numericInput("main_overall_y_max", "Макс. вес (г):", value = 2000, min = 100, max = 10000, step = 100),
            hr(),
            h4("💾 Экспорт"),
            numericInput("main_overall_export_width", "Ширина (см):", value = 20, min = 10, max = 30, step = 0.5),
            numericInput("main_overall_export_height", "Высота (см):", value = 15, min = 10, max = 30, step = 0.5),
            actionButton("main_overall_export", "📁 Экспортировать график", class = "btn-success", style = "width: 100%; font-weight: bold;")
          ),
          mainPanel(
            width = 9,
            withSpinner(plotOutput("main_overall_plot", height = "650px"), type = 6, color = "#0d6efd"),
            br(),
            h4("📋 Формулы групп"),
            DTOutput("main_overall_formulas_table")
          )
        )
      )
    )
  )
}


create_grouping_sidebar <- function() {
  sidebarPanel(
    width = 3,
    
    # Информационное сообщение
    tags$div(
      class = "alert alert-info",
      style = "margin-bottom: 15px;",
      icon("info-circle"),
      tags$strong("Важно:"),
      " Сначала выполните очистку данных во вкладке",
      tags$strong(" 'Очистка данных'"),
      ", затем нажмите кнопку ниже."
    ),
    
    h4("Параметры группировки"),
    
    numericInput("grouping_min_n", "Мин. наблюдений на вид:", 
                 value = 7, min = 5, max = 30, step = 1),
    
    sliderInput("grouping_max_diff", "Макс. расхождение кривых (%):", 
                min = 1, max = 50, value = 10, step = 0.5),
    
    sliderInput("grouping_max_growth", "Макс. отношение роста:", 
                min = 0.5, max = 5, value = 1.5, step = 0.1),
    
    actionButton("run_grouping", "Запустить группировку", 
                 class = "btn-primary",
                 style = "width: 100%; margin-top: 15px; font-weight: bold;"),
    
    hr(),
    
    h4("Навигация по группам"),
    fluidRow(
      column(6, actionButton("group_prev", "◀ Предыдущая", 
                            style = "width: 100%;")),
      column(6, actionButton("group_next", "Следующая ▶", 
                            style = "width: 100%;"))
    ),

    hr(),
    h4("💾 Экспорт данных"),
    actionButton("export_groups", "Экспорт в Excel",
                 class = "btn-success",
                 style = "width: 100%; font-weight: bold;"),
 fluidRow(
  column(3,
         checkboxInput("show_group_mean", "Показать среднюю линию", value = FALSE),
         checkboxInput("show_group_ci", "Показать доверительные интервалы", value = FALSE)
  ),
  column(3,
         conditionalPanel(
           condition = "input.show_group_ci",
           sliderInput("group_ci_width", "Ширина доверительного интервала (%)",
                       min = 5, max = 30, value = 10, step = 5)
         )
  )
),
    
    verbatimTextOutput("grouping_info")
  )
}


create_grouping_main_panel <- function() {
  mainPanel(
    width = 9,
    tabsetPanel(
      id = "grouping_tabs",
      tabPanel(
        "Общий график",
        fluidRow(
          column(
            12,
            tags$div(
              class = "alert alert-info",
              "Показана текущая группа (только группы с количеством видов > 1). Переключение — кнопками 'Предыдущая' и 'Следующая' слева."
            ),
            withSpinner(plotOutput("overall_plot", height = "600px"), type = 6, color = "#0d6efd"),
            br(),
            DTOutput("overall_formulas_table")
          )
        )
      ),
      tabPanel(
        "Одиночные виды",
        sidebarLayout(
          sidebarPanel(
            width = 4,
            checkboxGroupInput("single_species_select", "Выберите виды:", choices = NULL),
            fluidRow(
              column(6, actionButton("single_select_all", "Выбрать все", class = "btn-sm btn-primary")),
              column(6, actionButton("single_clear_all", "Очистить", class = "btn-sm btn-danger"))
            ),
            hr(),
            checkboxInput("single_show_groups", "Показывать фон", value = TRUE),
            checkboxInput("single_log_space", "Log-масштаб", value = FALSE)
          ),
          mainPanel(
            width = 8,
            withSpinner(plotOutput("single_plot", height = "600px"), type = 6, color = "#0d6efd")
          )
        )
      ),
      tabPanel(
        "Статистика",
        fluidRow(
          column(
            6,
            h4("Статистика кластеризации"),
            verbatimTextOutput("clustering_stats"),
            br(),
            plotOutput("cluster_size_plot", height = "350px")
          ),
          column(
            6,
            h4("Сводная таблица групп"),
            DTOutput("summary_table"),
            br(),
            plotOutput("param_distribution_plot", height = "300px")
          )
        )
      )
    )
  )
}



create_analysis_main_panel <- function() {
  mainPanel(
    width = 9,
    tabsetPanel(
      id = "analysis_tabs",
      tabPanel("График и результаты", 
               plotOutput("regression_plot", height = "500px"),
               uiOutput("model_comparison_result"),
               br(),
               uiOutput("legend_html")
      ),
      tabPanel("Детальная статистика", 
               verbatimTextOutput("model_stats"),
               br(),
               h4("Сравнение моделей:"),
               tableOutput("model_comparison_table")
      )
    )
  )
}

# Боковая панель
create_sidebar_panel <- function(data) {
  sidebarPanel(
    width = 3,
    h4("📥 Импорт данных"),
    fileInput(
      "upload_data_file",
      "Загрузите Excel (.xlsx)",
      accept = c(".xlsx")
    ),
    actionButton("reset_default_data", "Использовать встроенные данные", class = "btn-default", style = "width: 100%; margin-bottom: 8px;"),
    downloadButton("download_active_data", "Скачать текущие исходные данные", class = "btn-info", style = "width: 100%;"),
    br(), br(),
    textOutput("data_source_info"),

    hr(),
     selectInput(
      "species",
      "Выберите вид:",
      choices = sort(unique(data$species)),
      selected = unique(data$species)[1]
    ),
    
    hr(),
    create_navigation_buttons(),
    br(), br(),
    actionButton("highlight_outliers", "Подсветить выбросы", class = "btn-warning"),
    hr(),
    create_axis_settings(),
    create_display_settings(),
    hr(),
    create_model_settings(),
    hr(),
    create_ribbon_settings()
  )
}

# Навигационные кнопки
create_navigation_buttons <- function() {
  tagList(
    actionButton("next_species", "➡️Следующий вид", class = "btn-primary"),
    actionButton("prev_species", "⬅️Предыдущий вид", class = "btn-default")
  )
}

# Настройки осей
create_axis_settings <- function() {
  tagList(
    h4("Оси:"),
    fluidRow(
      column(6,
             prettyCheckbox(
               inputId = "log_x",
               label = "log ось X", 
               value = FALSE,
               status = "info",
               icon = icon("check"),
               bigger = TRUE,
               animation = "pulse"
             )
      ),
      column(6,
             prettyCheckbox(
               inputId = "log_y",
               label = "log ось Y", 
               value = FALSE,
               status = "info",
               icon = icon("check"),
               bigger = TRUE,
               animation = "pulse"
             )
      )
    )
  )
}

# Настройки отображения
create_display_settings <- function() {
  tagList(
    h4("Настройки отображения:"),
    sliderInput("point_size", "Размер точек:",
                min = 1, max = 10, value = 4),
    sliderInput("line_size", "Толщина линий:",
                min = 0.5, max = 3, value = 1.5, step = 0.1),
    sliderInput("alpha", "Прозрачность точек:",
                min = 0.1, max = 1, value = 0.7, step = 0.1)
  )
}

# Настройки моделей
create_model_settings <- function() {
  tagList(
    h4("Модели:"),
    fluidRow(
      
             prettyCheckbox(
               "show_power",
               label = tagList(icon("chart-line"), "Степенная"),
               value = TRUE,
               status = "danger",
               bigger = TRUE,
               animation = "pulse"
             
      ),
      
             prettyCheckbox(
               "show_exp",
               label = tagList(icon("chart-line"), "Экспоненц."),
               value = FALSE,
               status = "success",
               bigger = TRUE,
               animation = "pulse"
             
      ),
      
             prettyCheckbox(
               "show_points",
               label = tagList(icon("circle"), "Точки"),
               value = TRUE,
               status = "primary",
               bigger = TRUE,
               animation = "pulse"
             
      )
    )
  )
}

# Настройки доверительной полосы
create_ribbon_settings <- function() {
  tagList(
    h4("Доверительная полоса (ribbon):"),
    checkboxInput("show_ribbon", "Показать доверительную полосу", TRUE),
    sliderInput("ribbon_percent", "Ширина полосы (% от линии):",
                min = 0, max = 100, value = 50, step = 1,
                post = "%"),
    selectInput("ribbon_model", "Для какой модели:",
                choices = c("Степенная", "Экспоненциальная", "Обе"),
                selected = "Степенная"),
    sliderInput("ribbon_alpha", "Прозрачность полосы:",
                min = 0.1, max = 0.5, value = 0.3, step = 0.05)
  )
}

# Настройки очистки
create_cleaning_settings <- function() {
  sidebarPanel(
    width = 3,
    
    # 1. Параметры очистки
    h4("Параметры очистки"),
    
    selectInput("clean_model2", "Модель для очистки:",
                choices = c("Степенная", "Экспоненциальная"),
                selected = "Степенная"),
    
    sliderInput("clean_final2", "Порог отклонения от модели (%):",
                min = 0, max = 100, value = 30, step = 1,
                post = "%",
                helpText("Максимальное отклонение от модели")),
    
    sliderInput("min_final_n", "Мин. точек после очистки:",
                min = 3, max = 20, value = 10, step = 1,
                helpText("Минимум точек после очистки")),
    
    hr(),
    
    # 2. Фильтрация по разрывам
    h4("Фильтрация по разрывам"),
    
    numericInput(
      "gap_Lmin",
      "Минимальная длина, Lmin (см):",
      value = 3,
      min = 0,
      step = 0.5,
      width = "100%"
    ),
    helpText("Начало диапазона для анализа разрывов"),
    
    sliderInput(
      "gap_threshold",
      HTML("Максимальный разрыв (% от Lmin до maxlength):"),
      min = 5,
      max = 95,
      value = 45,
      step = 1,
      post = "%"
    ),
    
    hr(),
    
    # 3. Кнопки управления (теперь в sidebar!)
    create_cleaning_buttons_sidebar(),
    
    # 4. Описание процесса
    h5("Процесс очистки:"),
    tags$ul(
      tags$li("1. Очистка выбросов по модели"),
      tags$li("2. Фильтрация видов по разрывам"),
    )
  )
}

# Кнопки очистки данных (для sidebar)
create_cleaning_buttons_sidebar <- function() {
  tagList(
    hr(),
    
    h4("Управление очисткой"),
    
    # Кнопка запуска очистки
    actionButton("clean_all2", "Запустить очистку и фильтрацию", 
                 class = "btn-danger btn-block", 
                 icon = icon("broom"),
                 style = "width: 100%; height: 50px; font-size: 16px; font-weight: bold; margin-bottom: 15px;"),
    useShinyjs(),
    # Кнопка экспорта
    downloadButton("download_cleaned_btn1", "Скачать очищенные данные (Excel)", 
                   class = "btn-success btn-block",
                   style = "width: 100%; height: 50px; font-size: 16px; margin-bottom: 15px;"),
    
    # Пояснение
    tags$div(
      style = "padding: 10px; background-color: #d4edda; border-radius: 5px; margin-top: 10px;",
      icon("info-circle", style = "color: #28a745;"),
      tags$span(style = "color: #28a745; font-size: 13px;",
                "Фильтрация по разрывам применяется автоматически после очистки выбросов.")
    ),
    
    hr()
  )
}


# График Разрывов
create_cleaning_main_panel <- function() {
  mainPanel(
    fluidRow(
    column(6,
    withSpinner(
      plotOutput("gap_visualization_plot", height = "400px", width = "500"),
      type = 4, color = "#0d6efd"
    )),column(6,
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 5px solid #6c757d; padding: 15px;",
      verbatimTextOutput("gap_calculation_RESULT")
    ))),
# График визуализации разрывов
    h4("Визуализация разрывов в размерном ряду"),
    p(HTML("График показывает <b>ширину допустимого разрыва</b> между точками данных.<br>
          Изменяйте параметр 'Максимальный разрыв' слева, чтобы увидеть как меняется допустимая зона.")),
    br(),
    
    # Пример расчета
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 5px solid #6c757d; padding: 15px;",
      h4("Формула расчета разрывов"),
      verbatimTextOutput("gap_calculation_example")
    ),
    
    br(),
    
    # Статистика очистки (появляется после выполнения)
    conditionalPanel(
      condition = "output.cleaning_stats",
      h4("Результаты очистки"),
      verbatimTextOutput("cleaning_stats"),
      br(),
      h4("Статистика фильтрации по разрывам"),
      verbatimTextOutput("gap_filter_stats")
    )
  )
}
