library(shiny)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(readxl)
library(DT)
library(openxlsx)
library(scales)
library(igraph)
library(shinycssloaders)
library(tidyr)
library(cowplot)
library("shinyAce")

# ======================================================
# 1. ДАННЫЕ
# ======================================================
data_raw <- read_excel("Clear_2026-01-27.xlsx")

# Функция для проверки разрывов в размерном ряду
check_length_gaps <- function(lengths, Lmin = 1, maxl) {
  
  # оставляем только конечные значения >= Lmin
  L <- sort(lengths[is.finite(lengths) & lengths >= Lmin])
  
  # если данных недостаточно — сразу бракуем
  if (length(L) < 2) {
    return(tibble(
      min_L_observed = ifelse(length(L) == 0, NA, min(L)),
      max_L_observed = ifelse(length(L) == 0, NA, max(L)),
      max_gap_cm = NA,
      gap_ratio = Inf,
      pass_gap_filter = FALSE
    ))
  }
  
  # Создаем полный массив точек от Lmin до maxl, включая все промежуточные точки данных
  all_points <- c(Lmin, L, maxl)
  
  # Рассчитываем разрывы между всеми соседними точками
  gaps <- diff(all_points)
  
  max_gap <- max(gaps, na.rm = TRUE)
  range_total <- maxl - Lmin
  
  gap_ratio <- max_gap / range_total
  
  tibble(
    min_L_observed = min(L),
    max_L_observed = max(L),
    max_gap_cm = max_gap,
    gap_ratio = gap_ratio,
    pass_gap_filter = is.finite(gap_ratio)
  )
}

# Функция для расчета диагностики разрывов
calc_gap_diagnostics <- function(data, Lmin) {
  
  data %>%
    filter(
      was_cleaned,
      length > 0,
      is.finite(length)
    ) %>%
    group_by(species, secies_name_ru) %>%
    summarise(
      maxlength = first(maxlength),  # получаем maxlength
      gap_info = list(
        check_length_gaps(
          lengths = pick(length)[[1]],
          Lmin = Lmin,
          maxl = first(maxlength)  # передаем maxlength
        )
      ),
      .groups = "drop"
    ) %>%
    tidyr::unnest(gap_info)
}

# ======================================================
# 2. МОДЕЛИ ПО ВИДАМ (С ИСПОЛЬЗОВАНИЕМ maxlength)
# ======================================================
build_species_models <- function(data, min_n = 7) {
  
  cat("\n=== НАЧАЛО build_species_models ===\n")
  cat("Входные данные: строк =", nrow(data), ", видов =", length(unique(data$species)), "\n")
  
  # Упрощенный подход - извлекаем коэффициенты напрямую
  models <- data %>%
    filter(
      was_cleaned == TRUE,
      length > 0,
      weight > 0,
      is.finite(length),
      is.finite(weight)
    ) %>%
    group_by(species, secies_name_ru) %>%
    filter(n() >= min_n) %>%
    summarise(
      n = n(),
      maxlength = first(maxlength, na_rm = TRUE),
      min_length = min(length, na.rm = TRUE),
      max_length = max(length, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Видов с достаточным количеством точек:", nrow(models), "\n")
  
  # Для каждого вида строим модель
  results <- list()
  
  for(i in 1:nrow(models)) {
    sp <- models$species[i]
    sp_name <- models$secies_name_ru[i]
    
    # Получаем данные для этого вида
    sp_data <- data %>%
      filter(
        species == sp,
        was_cleaned == TRUE,
        length > 0, weight > 0,
        is.finite(length), is.finite(weight)
      )
    
    if(nrow(sp_data) >= min_n) {
      tryCatch({
        # Строим модель
        model <- lm(log(weight) ~ log(length), data = sp_data)
        coefs <- coef(model)
        
        # Извлекаем коэффициенты
        a <- exp(coefs[1])
        b <- coefs[2]
        
        # Проверяем корректность
        if(is.finite(a) && is.finite(b)) {
          results[[sp]] <- tibble(
            species = sp,
            secies_name_ru = sp_name,
            n = nrow(sp_data),
            maxlength = models$maxlength[i],
            min_length = min(sp_data$length),
            max_length = max(sp_data$length),
            a = a,
            b = b,
            log_a = log(a)
          )
          cat("Успешно:", sp_name, "a =", round(a, 4), "b =", round(b, 3), "\n")
        }
      }, error = function(e) {
        cat("Ошибка для вида", sp_name, ":", e$message, "\n")
      })
    }
  }
  
  if(length(results) == 0) {
    cat("Нет успешных моделей\n")
    return(tibble())
  }
  
  final_result <- bind_rows(results)
  cat("Успешно построено моделей:", nrow(final_result), "\n")
  cat("=== КОНЕЦ build_species_models ===\n\n")
  
  return(final_result)
}

# ======================================================
# 3. ФУНКЦИИ СХОДСТВА КРИВЫХ (ИСПОЛЬЗУЕМ maxlength)
# ======================================================
curve_distance_combined <- function(a1, b1, a2, b2, Lmax, points_per_cm = 10, Lmin = 1) {
  # Используем maxlength как Lmax
  if (Lmax <= Lmin) return(list(rms = Inf, growth_ratio = Inf, worst_L = NA, worst_diff = NA))
  n_points <- max(50, min(round((Lmax - Lmin) * points_per_cm), 5000))
  L <- seq(Lmin, Lmax, length.out = n_points)
  W1 <- a1 * L^b1
  W2 <- a2 * L^b2
  diff_rel <- abs(W1 - W2) / ((W1 + W2)/2)
  worst_idx <- which.max(diff_rel)
  list(
    rms = sqrt(mean(diff_rel^2, na.rm = TRUE)),
    growth_ratio = diff_rel[length(diff_rel)] / median(diff_rel, na.rm = TRUE),
    worst_L = L[worst_idx],
    worst_diff = diff_rel[worst_idx]
  )
}

group_by_curve_similarity_combined <- function(
    tbl,
    max_diff = 0.05,
    max_growth = 1,
    points_per_cm = 10
) {
  n <- nrow(tbl)
  if (n == 0) return(list(table = tbl, diagnostics = tibble()))
  
  adj <- matrix(FALSE, n, n)
  diagnostics <- list()
  idx <- 1
  
  for (i in 1:n) {
    for (j in i:n) {
      
      # Используем min из двух maxlength
      Lmax <- min(tbl$maxlength[i], tbl$maxlength[j])
      
      res <- curve_distance_combined(
        tbl$a[i], tbl$b[i],
        tbl$a[j], tbl$b[j],
        Lmax = Lmax,
        points_per_cm = points_per_cm
      )
      
      # Создаём диагностическую запись
      diagnostics[[idx]] <- tibble(
        i = i,
        j = j,
        species_i = tbl$secies_name_ru[i],
        species_j = tbl$secies_name_ru[j],
        rms = res$rms,
        growth_ratio = res$growth_ratio,
        worst_L = res$worst_L,
        worst_diff = res$worst_diff
      )
      
      # Только если оба значения конечные и не NA
      if (is.finite(res$rms) && is.finite(res$growth_ratio) &&
          res$rms <= max_diff && res$growth_ratio <= max_growth) {
        adj[i, j] <- TRUE
        adj[j, i] <- TRUE
      }
      
      idx <- idx + 1
    }
  }
  
  # Построение графа и определение групп
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  tbl$group <- igraph::components(g)$membership
  
  # Нумерация групп по размеру
  group_sizes <- tbl %>%
    count(group, name = "size") %>%
    arrange(desc(size))
  
  group_mapping <- setNames(seq_len(nrow(group_sizes)), group_sizes$group)
  tbl$group <- group_mapping[as.character(tbl$group)]
  
  list(
    table = tbl,
    diagnostics = bind_rows(diagnostics)
  )
}

# ======================================================
# 4. СРЕДНЯЯ КРИВАЯ (ИСПОЛЬЗУЕМ maxlength)
# ======================================================
calculate_mean_curve <- function(tbl, n_points = 300, Lmin = 1, power = 3) {
  if (nrow(tbl) < 2) return(NULL)
  Lmax <- max(tbl$maxlength, na.rm = TRUE)
  L <- seq(Lmin, Lmax, length.out = n_points)
  w_species <- (tbl$maxlength / max(tbl$maxlength))^power
  w_species <- w_species / sum(w_species)
  Wmat <- sapply(seq_len(nrow(tbl)), function(i) tbl$a[i]*L^tbl$b[i])
  Wmean <- Wmat %*% w_species
  data.frame(length = L, weight = as.numeric(Wmean), type = "mean_curve")
}

# ======================================================
# 6. UI
# ======================================================
ui <- navbarPage(
  "Группировка степенных регрессий",
  
  # ====================================================
  # ВКЛАДКА 4 — ОЧИСТКА ДАННЫХ
  # ====================================================
  tabPanel(
    "Очистка данных",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4("✂ Очистка размерного ряда"),
        
        numericInput(
          "gap_Lmin",
          "Минимальная длина (см)",
          value = 3,
          min = 0
        ),
        
        sliderInput(
          "gap_threshold",
          "Допустимый относительный разрыв",
          min = 0.05,
          max = 0.5,
          value = 0.45,
          step = 0.01
        ),
        
        actionButton(
          "apply_gap_filter",
          "Применить фильтр",
          class = "btn-danger",
          style = "width: 100%; font-weight: bold;"
        ),
        
        hr(),
        
        h4("ℹ Статистика фильтрации"),
        verbatimTextOutput("gap_filter_stats")
      ),
      
      mainPanel(
        width = 9,
        DTOutput("gap_table")
      )
    )
  ),
  # ====================================================
  # ВКЛАДКА 1 — ГРУППЫ
  # ====================================================
  tabPanel(
    "Группы",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "overflow-y: auto; max-height: 95vh;",
        
        # Кнопка пересчета
        actionButton("recalc", "🔄 Пересчитать группы", 
                    class = "btn-primary", 
                    style = "width: 100%; margin-bottom: 15px; font-weight: bold;"),
        
        hr(),
        h4("📊 Параметры модели"),
        sliderInput("min_n", "Мин. наблюдений на вид", 
                   min = 5, max = 30, value = 7, step = 1),
        
        hr(),
        h4("🎯 Критерии группировки"),
        sliderInput("max_diff", "Макс. расхождение кривых (%)", 
                   min = 1, max = 50, value = 10, step = 0.5),
        sliderInput("max_growth", "Макс. отношение роста", 
                   min = 0.5, max = 5, value = 1.5, step = 0.1),
        
        hr(),
        h4("📈 Настройки графика"),
        checkboxInput("compare_log_space", "Log-масштаб", value = FALSE),
        checkboxInput("compare_show_ci", "Показывать доверительные интервалы", value = TRUE),
        conditionalPanel(
          condition = "input.compare_show_ci",
          sliderInput("compare_ci_width", "Ширина CI (%)", 
                     min = 10, max = 80, value = 30, step = 1)
        ),
        
        hr(),
        h4("🔧 Настройки осей"),
        checkboxInput("auto_scale", "Автоматический масштаб", value = TRUE),
        conditionalPanel(
          condition = "!input.auto_scale",
          sliderInput("x_min", "Мин. X (Длина, см)", 
                     min = 0, max = 100, value = 0, step = 1),
          sliderInput("x_max", "Макс. X (Длина, см)", 
                     min = 10, max = 200, value = 50, step = 5),
          sliderInput("y_min", "Мин. Y (Вес, г)", 
                     min = 0, max = 1000, value = 0, step = 10),
          sliderInput("y_max", "Макс. Y (Вес, г)", 
                     min = 10, max = 10000, value = 1000, step = 50)
        ),
        
        hr(),
        h4("🚀 Навигация по группам"),
        fluidRow(
          column(6, actionButton("prev_group", "◀ Предыдущая", 
                                style = "width: 100%;")),
          column(6, actionButton("next_group", "Следующая ▶", 
                                style = "width: 100%;"))
        ),
        
        hr(),
        h4("✨ Дополнительные опции"),
        checkboxInput("show_group_median", "Показывать среднюю кривую группы", value = FALSE),
        checkboxInput("show_group_points", "Показывать точки данных", value = FALSE),
        conditionalPanel(
          condition = "input.show_group_points",
          sliderInput("point_size", "Размер точек", min = 0.5, max = 5, value = 2, step = 0.5),
          sliderInput("point_alpha", "Прозрачность точек", min = 0.1, max = 1, value = 0.5, step = 0.1)
        ),
      
        hr(),
        h4("💾 Экспорт данных"),
        actionButton("downloadData", "Экспорт в Excel", 
                    class = "btn-success", 
                    style = "width: 100%; margin-top: 10px; font-weight: bold;"),
        
        hr(),
        verbatimTextOutput("group_label")
      ),
      mainPanel(
        width = 9,
        withSpinner(
          plotOutput("group_plot", height = "650px"),
          type = 6,
          color = "#0d6efd"
        ),
        br(),
        fluidRow(
          column(6,
            h4("📋 Состав текущей группы"),
            DTOutput("group_table")
          ),
          column(6,
            h4("📊 Статистика группы"),
            verbatimTextOutput("group_stats")
          )
        )
      )
    )
  ),
  
# ====================================================
# ВКЛАДКА 2 — ОДИНОЧНЫЕ ВИДЫ
# ====================================================
tabPanel(
  "Одиночные виды",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("🎯 Выбор видов"),
      checkboxGroupInput(
        "single_species_select",
        "Выберите виды:",
        choices = NULL,
        selected = NULL
      ),
      actionButton("single_select_all", "Выбрать все", 
                  class = "btn-sm btn-primary"),
      actionButton("single_clear_all", "Очистить", 
                  class = "btn-sm btn-danger"),
      
      hr(),
      h4("📈 Настройки графика"),
      checkboxInput("single_show_groups", "Показывать фон групп", value = TRUE),
      conditionalPanel(
        condition = "input.single_show_groups",
        radioButtons(
          "single_background_type",
          "Тип фона:",
          choices = c(
            "Все группы" = "all",
            "Только сгруппированные" = "grouped",
            "Только одиночные" = "single"
          ),
          selected = "all",
          inline = FALSE
        )
      ),
      checkboxInput("single_log_space", "Log-масштаб", value = FALSE),
      
      hr(),
      h4("🔧 Настройки осей"),
      sliderInput("single_x_max", "Макс. длина (см)", 
                 min = 10, max = 200, value = 50, step = 5),
      sliderInput("single_y_max", "Макс. вес (г)", 
                 min = 10, max = 5000, value = 1000, step = 50),
      
      hr(),
      h4("🎨 Настройки отображения"),
      sliderInput("single_line_size", "Толщина линий", 
                 min = 0.5, max = 3, value = 1.2, step = 0.1),
      sliderInput("single_alpha", "Прозрачность линий", 
                 min = 0.3, max = 1, value = 1, step = 0.1)
    ),
    mainPanel(
      width = 9,
      withSpinner(
        plotOutput("single_plot", height = "700px"),
        type = 6,
        color = "#0d6efd"
      )
    )
  )
),
  
  # ====================================================
  # ВКЛАДКА 3 — СТАТИСТИКА
  # ====================================================
  tabPanel(
    "Статистика",
    fluidRow(
      column(6,
        h4("📊 Статистика кластеризации"),
        verbatimTextOutput("clustering_stats"),
        br(),
        h4("📈 Размеры групп"),
        plotOutput("cluster_size_plot", height = "400px")
      ),
      column(6,
        h4("📋 Сводная таблица групп"),
        DTOutput("summary_table"),
        br(),
        h4("📊 Распределение параметров"),
        plotOutput("param_distribution_plot", height = "300px")
      )
    )
  ),
  # ======================================================
# UI - ВКЛАДКА ЭКСПОРТА ГРАФИКОВ
# ======================================================
tabPanel(
  "Экспорт графиков",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("⚙ Настройки экспорта"),
      
      # Информация о группах
      htmlOutput("export_info_simple"),
      
      hr(),
      h4("📊 Настройки сетки"),
      
      # Количество графиков по ширине
      sliderInput("export_ncol", "Графиков по ширине:", 
                 min = 1, max = 5, value = 2, step = 1),
      
      # Автоматический расчет высоты
      checkboxInput("export_auto_height", "Автоматическая высота", value = TRUE),
      
      conditionalPanel(
        condition = "!input.export_auto_height",
        numericInput("export_nrow", "Количество строк:", 
                    value = 2, min = 1, max = 10, step = 1)
      ),
      
      # Отступы между графиками
      sliderInput("export_spacing", "Отступ между графиками:", 
                 min = 0.1, max = 2, value = 0.5, step = 0.1),
      
      hr(),
      h4("🔤 Настройки шрифтов"),
      
      # Размер основного шрифта
      sliderInput("export_font_size", "Основной размер шрифта:", 
                 min = 6, max = 16, value = 10, step = 0.5),
      
      # Размер заголовка
      sliderInput("export_title_size", "Размер заголовка (% от основного):", 
                 min = 70, max = 130, value = 90, step = 5, post = "%"),
      
      # Размер подписей осей
      sliderInput("export_axis_size", "Размер подписей осей (% от основного):", 
                 min = 70, max = 120, value = 80, step = 5, post = "%"),
      
      # Показывать заголовки графиков
      checkboxInput("export_show_titles", "Показывать заголовки графиков", 
                   value = TRUE),
      
      hr(),
      h4("🎨 Настройки стиля"),
      
      # Стиль графика
      radioButtons("export_style", "Стиль графика:",
                  choices = c("Цветной" = "color",
                             "Черно-белый (линии)" = "bw",
                             "Черно-белый (точки)" = "bw_dots",
                             "Минималистичный" = "minimal"),
                  selected = "color"),
      
      hr(),
      h4("📐 Настройки размеров"),
      
      # Размеры итогового файла
      numericInput("export_width", "Ширина итогового файла (см):", 
                  value = 21, min = 10, max = 50, step = 1),
      
      numericInput("export_height", "Высота итогового файла (см):", 
                  value = 29.7, min = 10, max = 50, step = 1),
      
      numericInput("export_dpi", "Разрешение (DPI):", 
                  value = 300, min = 150, max = 600, step = 50),
      
      # Формат файла
      radioButtons("export_format", "Формат файла:",
                  choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
      
      hr(),
      h4("💾 Экспорт"),
      
      # Папка для сохранения
      textInput("export_folder", "Папка для сохранения:", 
               value = "plotsResult"),
      
      # Имя файла
      textInput("export_filename", "Имя файла (без расширения):", 
               value = "all_groups"),
      
      # Кнопки экспорта
      actionButton("export_grid", "📁 Экспортировать сетку", 
                  class = "btn-success",
                  style = "width: 100%; font-weight: bold; margin-bottom: 10px;"),
      
      actionButton("export_individual", "📁 Экспортировать отдельно", 
                  class = "btn-primary",
                  style = "width: 100%; font-weight: bold; margin-bottom: 10px;"),
      
      # Предпросмотр
      actionButton("preview_grid", "👁 Обновить предпросмотр", 
                  class = "btn-info",
                  style = "width: 100%;")
    ),
    
    mainPanel(
      width = 9,
      withSpinner(
        plotOutput("export_grid_preview", height = "700px"),
        type = 6,
        color = "#0d6efd"
      ),
      br(),
      h5("ℹ Информация о группах:"),
      htmlOutput("export_groups_list")
    )
  )
),
# ======================================================
# UI - ДОБАВЛЯЕМ НОВУЮ ВКЛАДКУ "ОБЩИЙ ГРАФИК"
# ======================================================
tabPanel(
  "Общий график",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("🎯 Выбор групп"),
      
      # Выбор групп для отображения
      checkboxGroupInput("overall_groups_select", "Выберите группы:",
                        choices = NULL,
                        selected = NULL),
      
      actionButton("overall_select_all", "Выбрать все", 
                  class = "btn-sm btn-primary"),
      actionButton("overall_clear_all", "Очистить", 
                  class = "btn-sm btn-danger"),
      
      hr(),
      h4("📊 Настройки воронок"),
      
      # Прозрачность воронок
      sliderInput("overall_funnel_alpha", "Прозрачность воронок:", 
                 min = 0.1, max = 0.5, value = 0.2, step = 0.05),
      
      # Показывать средние линии
      checkboxInput("overall_show_mean", "Показывать средние линии", 
                   value = TRUE),
      
      conditionalPanel(
        condition = "input.overall_show_mean",
        sliderInput("overall_mean_size", "Толщина средних линий:", 
                   min = 1, max = 3, value = 1.5, step = 0.1),
        checkboxInput("overall_show_legend", "Показывать легенду", 
                     value = TRUE)
      ),
      
      # Показывать границы
      checkboxInput("overall_show_bounds", "Показывать границы воронок", 
                   value = TRUE),
      
      hr(),
      h4("🎨 Настройки отображения"),
      
      # Стиль графика
      radioButtons("overall_style", "Стиль графика:",
                  choices = c("Цветной" = "color",
                             "Черно-белый" = "bw",
                             "Только средние линии" = "mean_only"),
                  selected = "color"),
      
      # Размер шрифта
      sliderInput("overall_font_size", "Размер шрифта:", 
                 min = 10, max = 18, value = 12, step = 0.5),
      
      hr(),
      h4("📐 Настройки осей"),
      
      # Лимиты осей
      numericInput("overall_x_max", "Макс. длина (см):", 
                  value = 50, min = 10, max = 200, step = 5),
      numericInput("overall_y_max", "Макс. вес (г):", 
                  value = 2000, min = 100, max = 10000, step = 100),
      
      hr(),
      h4("💾 Экспорт"),
      
      # Размеры для экспорта
      numericInput("overall_export_width", "Ширина (см):", 
                  value = 20, min = 10, max = 30, step = 0.5),
      numericInput("overall_export_height", "Высота (см):", 
                  value = 15, min = 10, max = 30, step = 0.5),
      
      actionButton("overall_export", "📁 Экспортировать график", 
                  class = "btn-success",
                  style = "width: 100%; font-weight: bold;")
    ),
    
    mainPanel(
      width = 9,
      withSpinner(
        plotOutput("overall_plot", height = "650px"),
        type = 6,
        color = "#0d6efd"
      ),
      br(),
      h4("📋 Формулы групп"),
      DTOutput("overall_formulas_table")
    )
  )
),
# ======================================================
# UI - ДОБАВЛЯЕМ ВКЛАДКУ РЕДАКТОРА КОДА
# ======================================================
tabPanel(
  "Редактор кода",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("📝 Редактор графика"),
      
      # Выбор группы для редактирования
      selectInput("edit_group", "Выберите группу для редактирования:",
                  choices = NULL),
      
      # Выбор стиля
      selectInput("edit_style", "Стиль графика:",
                  choices = c("Основной" = "main",
                             "Минималистичный" = "minimal",
                             "Публикационный" = "publication",
                             "Черно-белый" = "bw")),
      
      # Настройки
      sliderInput("edit_font_size", "Размер шрифта:", 
                 min = 8, max = 16, value = 10, step = 0.5),
      checkboxInput("edit_show_points", "Показывать точки данных", 
                   value = FALSE),
      checkboxInput("edit_show_ci", "Показывать доверительные интервалы", 
                   value = FALSE),
      
      hr(),
      h4("⚡ Быстрые правки"),
      
      # Быстрые правки через интерфейс
      textInput("edit_title", "Заголовок графика:", 
               value = "Группа регрессий длина-вес"),
      textInput("edit_xlab", "Подпись оси X:", value = "Длина, см"),
      textInput("edit_ylab", "Подпись оси Y:", value = "Вес, г"),
      
      actionButton("apply_quick_edit", "Применить быстрые правки", 
                  class = "btn-primary",
                  style = "width: 100%; margin-bottom: 10px;"),
      
      hr(),
      h4("💾 Сохранение кода"),
      
      # Сохранение кода
      textInput("code_filename", "Имя файла для сохранения:", 
               value = "custom_plot_code.R"),
      actionButton("save_code", "💾 Сохранить код", 
                  class = "btn-success",
                  style = "width: 100%;"),
      
      # Загрузка кода
      fileInput("load_code", "📂 Загрузить код",
                accept = c(".R", ".r", ".txt")),
      
      br(),
      actionButton("reset_code", "🔄 Сбросить к исходному", 
                  class = "btn-warning",
                  style = "width: 100%;")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel(
          "📊 Предпросмотр",
          withSpinner(
            plotOutput("edit_preview_plot", height = "500px"),
            type = 6, color = "#0d6efd"
          ),
          br(),
          actionButton("update_preview", "🔄 Обновить предпросмотр", 
                      class = "btn-info",
                      style = "width: 100%;")
        ),
        tabPanel(
          "📝 Редактор кода",
          br(),
          h5("Редактируйте код графика ниже:"),
          aceEditor("code_editor", 
                   mode = "r",
                   theme = "chrome",
                   height = "500px",
                   fontSize = 14,
                   value = "# Код графика будет загружен здесь\n# Редактируйте и нажмите 'Обновить предпросмотр'"),
          br(),
          actionButton("execute_code", "▶ Выполнить код", 
                      class = "btn-success",
                      style = "width: 100%; font-weight: bold;")
        ),
        tabPanel(
          "📋 Примеры кода",
          br(),
          h5("Примеры для вставки:"),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
            tags$pre(
              style = "font-size: 12px;",
'# Добавить легенду в правом верхнем углу
theme(legend.position = c(0.95, 0.95),
      legend.justification = c(1, 1))
# Изменить цвета линий
scale_color_manual(values = c("red", "blue", "green"))
# Добавить сетку
theme(panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.2))+

# Логарифмическая шкала
scale_x_log10() + scale_y_log10()

# Добавить аннотацию
annotate("text", x = 20, y = 100, 
         label = "Текст аннотации", size = 4, color = "red")'
            )
          )
        )
      )
    )
  )
)
)

# ======================================================
# 7. SERVER
# ======================================================
server <- function(input, output, session) {
  
  # ======================================================
  # ФИЛЬТРАЦИЯ ПО РАЗРЫВАМ
  # ======================================================
  gap_diagnostics <- reactive({
    req(input$gap_Lmin, input$gap_threshold)
    
    calc_gap_diagnostics(
      data = data_raw,
      Lmin = input$gap_Lmin
    ) %>%
      mutate(
        gap_threshold = input$gap_threshold,
        pass_gap_filter = gap_ratio <= gap_threshold
      ) %>%
      arrange(desc(gap_ratio))
  })
  
  # Фильтрованные данные
  data_gap_filtered <- reactive({
    req(gap_diagnostics())
    
    passed_species <- gap_diagnostics() %>%
      filter(pass_gap_filter) %>%
      pull(species)
    
    data_raw %>%
      filter(species %in% passed_species)
  })
  
  # Статистика фильтрации
  output$gap_filter_stats <- renderPrint({
    req(gap_diagnostics())
    
    total_species <- nrow(gap_diagnostics())
    passed_species <- gap_diagnostics() %>%
      filter(pass_gap_filter) %>%
      nrow()
    
    cat("=== СТАТИСТИКА ФИЛЬТРАЦИИ ===\n\n")
    cat(sprintf("Всего видов: %d\n", total_species))
    cat(sprintf("Прошло фильтр: %d (%.1f%%)\n", 
                passed_species, passed_species/total_species*100))
    cat(sprintf("Отбраковано: %d (%.1f%%)\n", 
                total_species - passed_species, 
                (total_species - passed_species)/total_species*100))
    cat(sprintf("\nПараметры фильтра:\n"))
    cat(sprintf("• Минимальная длина: %.0f см\n", input$gap_Lmin))
    cat(sprintf("• Допустимый разрыв: %.0f%%\n", input$gap_threshold*100))
  })
  
  # Таблица диагностики разрывов
  output$gap_table <- renderDT({
    req(gap_diagnostics())
    
    datatable(
      gap_diagnostics() %>%
        select(
          species, secies_name_ru, maxlength,
          min_L_observed, max_L_observed, max_gap_cm,
          gap_ratio, gap_threshold, pass_gap_filter
        ),
      options = list(
        pageLength = 150,
        columnDefs = list(
          list(targets = 8, visible = FALSE)  # скрываем pass_gap_filter
        )
      )
    )
  })
  
  # ======================================================
  # КЭШ МОДЕЛЕЙ (НА ФИЛЬТРОВАННЫХ ДАННЫХ)
  # ======================================================
  models_cached <- reactive({
    req(data_gap_filtered())
    build_species_models(data_gap_filtered(), input$min_n)
  })
  
  # ======================================================
  # ГРУППИРОВКА
  # ======================================================
  grouped <- eventReactive(input$recalc, {
    req(models_cached())
    
    withProgress(message = 'Группировка видов...', value = 0.5, {
      result <- group_by_curve_similarity_combined(
        tbl = models_cached(),
        max_diff   = input$max_diff / 100,
        max_growth = input$max_growth,
        points_per_cm = 10
      )
      
      incProgress(0.5, detail = "Завершение...")
      return(result)
    })
  })
  
  # ======================================================
  # СТАТИСТИКА ГРУППИРОВКИ
  # ======================================================
  output$clustering_stats <- renderPrint({
    req(grouped())
    
    tbl <- grouped()$table
    
    total_species <- nrow(tbl)
    total_groups <- length(unique(tbl$group))
    
    singletons <- tbl %>%
      group_by(group) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      nrow()
    
    clusters <- total_groups - singletons
    
    avg_cluster_size <- if (clusters > 0) {
      cluster_sizes <- tbl %>%
        group_by(group) %>%
        summarise(size = n(), .groups = "drop") %>%
        filter(size > 1)
      round(mean(cluster_sizes$size), 1)
    } else {
      0
    }
    
    cat("=== СТАТИСТИКА КЛАСТЕРИЗАЦИИ ===\n\n")
    cat(sprintf("Всего видов: %d\n", total_species))
    cat(sprintf("Всего групп: %d\n", total_groups))
    cat(sprintf("  • Групп с >1 вида: %d\n", clusters))
    cat(sprintf("  • Одиночных видов: %d (%.1f%%)\n", 
                singletons, singletons/total_species*100))
    cat(sprintf("Средний размер группы: %.1f\n", avg_cluster_size))
    cat(sprintf("Максимальный размер группы: %d\n", max(table(tbl$group))))
    cat(sprintf("Минимальный размер группы: %d\n", min(table(tbl$group))))
    cat(sprintf("\nПараметры группировки:\n"))
    cat(sprintf("  • Мин. наблюдений на вид: %d\n", input$min_n))
    cat(sprintf("  • Допуск расхождения: %.1f%%\n", input$max_diff))
    cat(sprintf("  • Макс. отношение роста: %.1f\n", input$max_growth))
  })
  
  # График распределения параметров
  output$param_distribution_plot <- renderPlot({
    req(grouped())
    
    grouped()$table %>%
      ggplot(aes(x = b, fill = factor(group))) +
      geom_histogram(bins = 20, alpha = 0.7) +
      labs(title = "Распределение параметра b по группам",
           x = "Параметр b",
           y = "Количество видов") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # ======================================================
  # ГРАФИК РАЗМЕРОВ ГРУПП
  # ======================================================
  output$cluster_size_plot <- renderPlot({
    req(grouped())
    
    group_stats <- grouped()$table %>%
      group_by(group) %>%
      summarise(
        n_species = n(),
        group_type = ifelse(n() == 1, "Одиночка", "Группа"),
        .groups = "drop"
      ) %>%
      arrange(desc(n_species))
    
    ggplot(group_stats, aes(x = reorder(factor(group), n_species), 
                           y = n_species, fill = group_type)) +
      geom_col() +
      geom_text(aes(label = n_species), vjust = -0.5, size = 4) +
      scale_fill_manual(values = c("Группа" = "#3498db", "Одиночка" = "#e74c3c")) +
      labs(
        title = "Размеры групп",
        x = "Группа",
        y = "Количество видов",
        fill = "Тип"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ======================================================
  # СВОДНАЯ ТАБЛИЦА ГРУПП
  # ======================================================
 output$summary_table <- renderDT({
  req(grouped())
  
  summary <- grouped()$table %>%
    group_by(group) %>%
    summarise(
      `Кол-во видов` = n(),
      `Наблюдений всего` = sum(n),
      `Мин. длина (см)` = round(min(min_length), 1),
      `Макс. промер (см)` = round(max(max_length), 1),
      `Предельная длина модели (см)` = round(max(maxlength), 1),
      `Средний a` = round(mean(a), 5),
      `SD a` = ifelse(n() > 1, round(sd(a), 5), NA),
      `Средний b` = round(mean(b), 3),
      `SD b` = ifelse(n() > 1, round(sd(b), 3), NA),
      `Виды` = paste(species, collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(`Кол-во видов`))
  
  datatable(
    summary,
    rownames = FALSE,
    options = list(
      pageLength = 150,
      dom = 'Bfrtip',
      scrollX = TRUE
    )
  )
})
  
  # ======================================================
  # ОДИНОЧНЫЕ ВИДЫ
  # ======================================================
  single_species <- reactive({
    req(grouped())
    grouped()$table %>%
      group_by(group) %>%
      filter(n() == 1) %>%
      ungroup()
  })
  
  # Обновление списка одиночных видов
  observeEvent(single_species(), {
    req(single_species())
    
    ss <- single_species() %>%
      arrange(secies_name_ru)
    
    updateCheckboxGroupInput(
      session,
      "single_species_select",
      choices = setNames(ss$species, ss$secies_name_ru),
      selected = head(ss$species, 1)
    )
  })
  
  # Кнопки для выбора/очистки одиночных видов
  observeEvent(input$single_select_all, {
    req(single_species())
    
    updateCheckboxGroupInput(
      session,
      "single_species_select",
      selected = single_species()$species
    )
  })
  
  observeEvent(input$single_clear_all, {
    updateCheckboxGroupInput(
      session,
      "single_species_select",
      selected = character(0)
    )
  })
  


# ======================================================
# ГРАФИК ОДИНОЧНЫХ ВИДОВ
# ======================================================

# Функция для получения линий фона групп
group_background_lines <- reactive({
  req(grouped())
  
  # Получаем статистику по группам
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(
      n_species_in_group = n(),  # переименовываем
      a = median(a),
      b = median(b),
      Lmax = max(maxlength),
      .groups = "drop"
    )
  
  # Фильтруем группы в зависимости от выбора пользователя
  if (input$single_background_type == "single") {
    # Только одиночные виды (группы с 1 видом)
    group_stats <- group_stats %>% filter(n_species_in_group == 1)
    label_prefix <- "Одиночный вид"
  } else if (input$single_background_type == "grouped") {
    # Только сгруппированные виды (группы с >1 вида)
    group_stats <- group_stats %>% filter(n_species_in_group > 1)
    label_prefix <- "Группа"
  } else {
    # Все группы (по умолчанию)
    label_prefix <- "Группа"
  }
  
  if (nrow(group_stats) == 0) {
    return(tibble())
  }
  
  # Создаем данные для линий
  background_data <- group_stats %>%
    rowwise() %>%
    mutate(
      line_data = list({  # переименовываем вложенный список
        L <- seq(1, Lmax, length.out = 200)
        tibble(
          group_label = paste(label_prefix, group),
          group_id = group,
          length = L,
          weight = a * L^b
        )
      })
    )
  
  # Разворачиваем данные
  background_data %>%
    select(group, n_species_in_group, line_data) %>%
    unnest(cols = line_data)
})

output$single_plot <- renderPlot({
  req(input$single_species_select, single_species())
  
  selected_species <- single_species() %>%
    filter(species %in% input$single_species_select)
  
  if (nrow(selected_species) == 0) {
    return(
      ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = "Выберите виды для отображения",
          size = 6
        ) +
        theme_void()
    )
  }
  
  # Линии выбранных одиночных видов
  species_lines <- purrr::map_dfr(1:nrow(selected_species), function(i) {
    L <- seq(1, selected_species$maxlength[i], length.out = 200)
    tibble(
      species = selected_species$secies_name_ru[i],
      species_id = selected_species$species[i],
      length = L,
      weight = selected_species$a[i] * L^selected_species$b[i]
    )
  })
  
  # Линии фона (если выбрано)
  background_lines <- group_background_lines()
  
  p <- ggplot()
  
  # ===== ФОН ГРУПП =====
  if (input$single_show_groups && nrow(background_lines) > 0) {
    # Определяем цвет в зависимости от типа фона
    if (input$single_background_type == "single") {
      line_color <- "#e74c3c"  # красный для одиночных
    } else if (input$single_background_type == "grouped") {
      line_color <- "#3498db"  # синий для сгруппированных
    } else {
      line_color <- "grey70"   # серый для всех
    }
    
    p <- p +
      geom_line(
        data = background_lines,
        aes(x = length, y = weight, group = group_id),
        colour = line_color,
        linewidth = 0.6,
        alpha = 0.3
      )
    
    # Добавляем легенду для фона
    if (input$single_background_type == "single") {
      p <- p + 
        annotate(
          "segment", 
          x = max(background_lines$length) * 0.85, 
          xend = max(background_lines$length) * 0.95,
          y = max(background_lines$weight) * 0.95,
          yend = max(background_lines$weight) * 0.95,
          color = "#e74c3c",
          linewidth = 1,
          alpha = 0.5
        ) +
        annotate(
          "text",
          x = max(background_lines$length) * 0.85,
          y = max(background_lines$weight) * 0.95,
          label = "Одиночные виды",
          hjust = 1,
          color = "#e74c3c",
          size = 3.5,
          alpha = 0.7
        )
    } else if (input$single_background_type == "grouped") {
      p <- p + 
        annotate(
          "segment", 
          x = max(background_lines$length) * 0.85, 
          xend = max(background_lines$length) * 0.95,
          y = max(background_lines$weight) * 0.95,
          yend = max(background_lines$weight) * 0.95,
          color = "#3498db",
          linewidth = 1,
          alpha = 0.5
        ) +
        annotate(
          "text",
          x = max(background_lines$length) * 0.85,
          y = max(background_lines$weight) * 0.95,
          label = "Сгруппированные виды",
          hjust = 1,
          color = "#3498db",
          size = 3.5,
          alpha = 0.7
        )
    }
  }
  
  # ===== ЛИНИИ ВЫБРАННЫХ ВИДОВ =====
  p <- p +
    geom_line(
      data = species_lines,
      aes(x = length, y = weight, colour = species),
      linewidth = input$single_line_size,
      alpha = input$single_alpha
    ) +
    theme_minimal(base_size = 14) +
    labs(
      x = "Длина, см",
      y = "Вес, г",
      colour = "Выбранные виды"
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin()
    ) +
    coord_cartesian(
      xlim = c(0, input$single_x_max),
      ylim = c(0, input$single_y_max)
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
  
  # Логарифмический масштаб
  if (input$single_log_space) {
    p <- p + scale_x_log10() + scale_y_log10()
  }
  
  # Если нет выбранных видов, но есть фон
  if (nrow(selected_species) == 0 && nrow(background_lines) > 0) {
    p <- p + labs(colour = NULL) + theme(legend.position = "none")
  }
  
  p
})




  # ======================================================
  # НАВИГАЦИЯ ПО ГРУППАМ
  # ======================================================
  current_group <- reactiveVal(1)
  
  observeEvent(grouped(), {
    current_group(1)
  })
  
  observeEvent(input$prev_group, {
    req(grouped())
    g <- current_group() - 1
    if (g < 1) g <- max(grouped()$table$group)
    current_group(g)
  })
  
  observeEvent(input$next_group, {
    req(grouped())
    g <- current_group() + 1
    if (g > max(grouped()$table$group)) g <- 1
    current_group(g)
  })
  
  # Показываем номер текущей группы 
  output$group_label <- renderText({ 
    req(grouped()) 
    tbl <- grouped()$table 
    
    # Количество групп, где больше одного вида 
    groups_summary <- tbl %>% 
      group_by(group) %>% 
      summarise(n_species = n()) 
    
    n_useful_groups <- groups_summary %>% 
      filter(n_species > 1) %>% 
      nrow() 
    
    n_species_useful <- groups_summary %>% 
      filter(n_species > 1) %>% 
      summarise(total = sum(n_species)) %>% 
      pull(total) 
    
    # Текущая группа / общее число групп 
    grp <- current_group() 
    total <- length(unique(tbl$group)) 
    
    paste0( 
      "Группа ", grp, " из ", total, 
      " — ", n_useful_groups, " групп с >1 вида", 
      ", всего: ", n_species_useful 
    ) 
  })
  
  # ======================================================
  # ЛИНИИ ТЕКУЩЕЙ ГРУППЫ (ИСПОЛЬЗУЕМ maxlength)
  # ======================================================
  group_lines <- reactive({
    req(grouped(), current_group())
    
    tbl_grp <- grouped()$table %>% filter(group == current_group())
    req(nrow(tbl_grp) > 0)
    
    ci <- if (input$compare_show_ci) input$compare_ci_width / 100 else 0
    
    species_lines <- purrr::map_dfr(seq_len(nrow(tbl_grp)), function(i) {
      # Используем maxlength для построения кривой
      L <- seq(1, tbl_grp$maxlength[i], length.out = 300)
      W <- tbl_grp$a[i] * L^tbl_grp$b[i]
      data.frame(
        species = tbl_grp$secies_name_ru[i],
        species_latin = tbl_grp$species[i],
        length = L,
        weight = W,
        low  = W * (1 - ci),
        high = W * (1 + ci),
        type = "species"
      )
    })
    
    mean_line <- NULL
    if (input$show_group_median && nrow(tbl_grp) > 1) {
      mean_line <- calculate_mean_curve(tbl_grp, n_points = 300, Lmin = 1, power = 3)
      if (!is.null(mean_line)) {
        mean_line$type <- "mean_curve"
        mean_line <- as.data.frame(mean_line)
      }
    }
    
    if (!is.null(mean_line)) {
      bind_rows(species_lines, mean_line)
    } else {
      species_lines
    }
  })
  
  # ======================================================
  # ГРАФИК ГРУППЫ
  # ======================================================
  output$group_plot <- renderPlot({
    req(group_lines())
    
    df <- group_lines()
    species_lines <- df %>% filter(type == "species")
    mean_line     <- df %>% filter(type == "mean_curve")
    
    # Пределы осей
    if (input$auto_scale) {
      x_lim <- c(min(species_lines$length, na.rm = TRUE), 
                 max(species_lines$length, na.rm = TRUE))
      y_lim <- c(min(species_lines$weight, na.rm = TRUE), 
                 max(species_lines$weight, na.rm = TRUE))
    } else {
      x_lim <- c(input$x_min, input$x_max)
      y_lim <- c(input$y_min, input$y_max)
    }
    
    # Базовый график
    p <- ggplot(species_lines, aes(length, weight, colour = species)) +
      geom_line(linewidth = 1.2) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      labs(x = "Длина, см", y = "Вес, г", colour = "Вид") +
      coord_cartesian(xlim = x_lim, ylim = y_lim)
    
    # Доверительные интервалы
    if (input$compare_show_ci) {
      p <- p +
        geom_ribbon(aes(ymin = low, ymax = high, fill = species),
                   alpha = 0.15, colour = NA, show.legend = FALSE)
    }
    
    # Средняя кривая группы
    if (nrow(mean_line) > 0) {
      p <- p +
        geom_line(data = mean_line,
                 aes(x = length, y = weight),
                 colour = "black",
                 linewidth = 1.5,
                 linetype = "dashed")
    }
    
    # Исходные точки
    if (input$show_group_points) {
      grp_species <- unique(species_lines$species_latin)
      
      pts <- data_gap_filtered() %>%
        filter(species %in% grp_species) %>%
        filter(length > 0, weight > 0)
      
      if (nrow(pts) > 0) {
        p <- p +
          geom_point(data = pts,
                    aes(length, weight, colour = secies_name_ru),
                    size = input$point_size,
                    alpha = input$point_alpha)
      }
    }
    
    # Логарифмический масштаб
    if (input$compare_log_space) {
      p <- p + scale_x_log10() + scale_y_log10()
    }
    
    p
  })
  
  # ======================================================
  # СТАТИСТИКА ТЕКУЩЕЙ ГРУППЫ
  # ======================================================
  output$group_stats <- renderPrint({
    req(grouped(), current_group())
    
    tbl_grp <- grouped()$table %>% filter(group == current_group())
    
    cat("=== СТАТИСТИКА ГРУППЫ ===\n\n")
    cat(sprintf("Группа: %d\n", current_group()))
    cat(sprintf("Количество видов: %d\n", nrow(tbl_grp)))
    cat(sprintf("Наблюдений всего: %d\n", sum(tbl_grp$n)))
    cat(sprintf("Диапазон промеров: %.1f - %.1f см\n", 
                min(tbl_grp$min_length), max(tbl_grp$max_length)))
    cat(sprintf("Диапазон maxlength: %.1f - %.1f см\n", 
                min(tbl_grp$maxlength), max(tbl_grp$maxlength)))
    cat(sprintf("Средний параметр b: %.3f\n", mean(tbl_grp$b)))
    if (nrow(tbl_grp) > 1) {
      cat(sprintf("SD параметра b: %.3f\n", sd(tbl_grp$b)))
    }
    cat("\nВиды в группе:\n")
    for (i in 1:nrow(tbl_grp)) {
      cat(sprintf("%d. %s (n=%d, maxlength=%.1f см)\n", 
                  i, tbl_grp$secies_name_ru[i], tbl_grp$n[i], tbl_grp$maxlength[i]))
    }
  })
  
  # ======================================================
  # ТАБЛИЦА ТЕКУЩЕЙ ГРУППЫ
  # ======================================================
  output$group_table <- renderDT({
    req(grouped(), current_group())
    
    tbl_grp <- grouped()$table %>% filter(group == current_group())
    
    datatable(
      tbl_grp %>%
        mutate(
          a = round(a, 6),
          b = round(b, 3),
          maxlength = round(maxlength, 1),
          min_length = round(min_length, 1),
          max_length = round(max_length, 1)
        ) %>%
        select(
          Вид = secies_name_ru,
          Наблюдения = n,
          `Мин. промер` = min_length,
          `Макс. промер` = max_length,
          `maxlength` = maxlength,
          a = a,
          b = b
        ),
      rownames = FALSE,
      options = list(pageLength = 150, dom = 't')
    )
  })
  
# ======================================================
#----------------- ЭКСПОРТ В EXCEL -----------------------------
# ======================================================

export_to_excel_final <- function() {
  # Проверяем, что данные доступны
  if (is.null(grouped())) {
    stop("Данные не загружены")
  }
  
  tbl <- grouped()$table
  
  # Функция для расчета групповых коэффициентов через взвешенную среднюю
  calculate_group_coefficients <- function(group_data) {
    if (nrow(group_data) < 2) {
      # Для одиночных видов берем их собственные коэффициенты
      return(data.frame(
        a_group = group_data$a[1],
        b_group = group_data$b[1]
      ))
    }
    
    # Используем функцию calculate_mean_curve для получения средней кривой
    mean_curve <- calculate_mean_curve(group_data, n_points = 100, Lmin = 1, power = 3)
    
    if(is.null(mean_curve) || nrow(mean_curve) < 2) {
      # Если не удалось вычислить среднюю кривую, используем среднее арифметическое
      return(data.frame(
        a_group = mean(group_data$a, na.rm = TRUE),
        b_group = mean(group_data$b, na.rm = TRUE)
      ))
    }
    
    # Подбираем коэффициенты a и b к средней кривой
    # Линеаризация: log(weight) = log(a) + b * log(length)
    mean_curve$logL <- log(mean_curve$length)
    mean_curve$logW <- log(mean_curve$weight)
    
    # Убираем бесконечные значения
    mean_curve <- mean_curve[is.finite(mean_curve$logL) & is.finite(mean_curve$logW), ]
    
    if(nrow(mean_curve) < 2) {
      return(data.frame(
        a_group = mean(group_data$a, na.rm = TRUE),
        b_group = mean(group_data$b, na.rm = TRUE)
      ))
    }
    
    # Линейная регрессия для получения коэффициентов
    fit <- lm(logW ~ logL, data = mean_curve)
    
    return(data.frame(
      a_group = exp(coef(fit)[1]),  # exp(intercept)
      b_group = coef(fit)[2]        # slope
    ))
  }
  
  # Рассчитываем групповые коэффициенты для каждой группы
  group_coefs_list <- list()
  
  for(g in unique(tbl$group)) {
    group_data <- tbl %>% filter(group == g)
    coefs <- calculate_group_coefficients(group_data)
    group_coefs_list[[as.character(g)]] <- data.frame(
      group = g,
      a_group = coefs$a_group,
      b_group = coefs$b_group
    )
  }
  
  group_coefs <- bind_rows(group_coefs_list)
  
  # Теперь группируем остальную статистику
  group_stats <- tbl %>%
    group_by(group) %>%
    summarise(
      n_species_in_group = n(),
      n_measurements_group = sum(n),
      .groups = "drop"
    ) %>%
    left_join(group_coefs, by = "group")
  
  # Получаем статистику по длине и весу из исходных данных
  weight_length_stats <- data_gap_filtered() %>%
    filter(
      was_cleaned,
      length > 0,
      weight > 0,
      is.finite(length),
      is.finite(weight)
    ) %>%
    group_by(species) %>%
    summarise(
      min_L_observed = min(length, na.rm = TRUE),
      max_L_observed = max(length, na.rm = TRUE),
      min_W = min(weight, na.rm = TRUE),
      max_W = max(weight, na.rm = TRUE),
      n_measurements_species = n(),  # Добавляем подсчет измерений по виду
      .groups = "drop"
    )
  
  # Создаем итоговую таблицу
  export_table <- tbl %>%
    left_join(group_stats, by = "group") %>%
    left_join(weight_length_stats, by = "species") %>%
    mutate(
      # Округляем коэффициенты
      a_group = round(a_group, 6),
      b_group = round(b_group, 3),
      a_species = round(a, 6),
      b_species = round(b, 3),
      
      # Округляем промеры
      min_L_observed = round(min_L_observed, 1),
      max_L_observed = round(max_L_observed, 1),
      min_W = round(min_W, 1),
      max_W = round(max_W, 1)
    ) %>%
    select(
      group_id = group,
      species_latin = species,
      species_ru = secies_name_ru,
      maxlength,
      a_group,
      b_group,
      a_species,
      b_species,
      n_species_in_group,
      n_measurements_species,
      n_measurements_group,
      min_L_observed,
      max_L_observed,
      min_W,
      max_W
    ) %>%
    arrange(group_id, species_latin)
  
  # Добавляем семейство и соленость если есть
  if (!is.null(data_gap_filtered())) {
    # Проверяем наличие Family (с большой или маленькой буквы)
    if ("Family" %in% names(data_gap_filtered())) {
      family_info <- data_gap_filtered() %>%
        select(species, Family) %>%
        distinct()
    } else if ("family" %in% names(data_gap_filtered())) {
      family_info <- data_gap_filtered() %>%
        select(species, family) %>%
        distinct() %>%
        rename(Family = family)
    } else {
      family_info <- NULL
    }
    
    # Проверяем наличие Salt
    if ("Salt" %in% names(data_gap_filtered())) {
      salt_info <- data_gap_filtered() %>%
        select(species, Salt) %>%
        distinct()
    } else {
      salt_info <- NULL
    }
    
    # Добавляем Family
    if (!is.null(family_info)) {
      export_table <- export_table %>%
        left_join(family_info, by = c("species_latin" = "species"))
    } else {
      export_table$Family <- NA_character_
    }
    
    # Добавляем Salt
    if (!is.null(salt_info)) {
      export_table <- export_table %>%
        left_join(salt_info, by = c("species_latin" = "species"))
    } else {
      export_table$Salt <- NA_character_
    }
    
    # Переупорядочиваем колонки
    export_table <- export_table %>%
      select(
        group_id, species_latin, species_ru, 
        Family, Salt,
        everything()
      )
  } else {
    export_table$Family <- NA_character_
    export_table$Salt <- NA_character_
  }
  
  # Рассчитываем R² для каждой группы (дополнительная информация)
  cat("\n=== РАСЧЕТ КАЧЕСТВА ГРУППОВЫХ ФОРМУЛ ===\n")
  
  for(g in unique(export_table$group_id)) {
    group_rows <- export_table %>% filter(group_id == g)
    
    if(nrow(group_rows) > 1) {
      cat(sprintf("\nГруппа %d (%d видов):\n", g, nrow(group_rows)))
      cat(sprintf("  Формула: W = %.6f × L^%.3f\n", 
                  unique(group_rows$a_group), 
                  unique(group_rows$b_group)))
      
      # Расчет R² для каждого вида в группе
      r2_values <- numeric()
      
      for(i in 1:nrow(group_rows)) {
        # Общий диапазон длин для сравнения
        L_range <- seq(
          group_rows$min_L_observed[i],
          group_rows$max_L_observed[i],
          length.out = 50
        )
        
        # Предсказания групповой и индивидуальной моделей
        W_group <- group_rows$a_group[i] * L_range^group_rows$b_group[i]
        W_species <- group_rows$a_species[i] * L_range^group_rows$b_species[i]
        
        # R²
        SSE <- sum((W_species - W_group)^2)
        SST <- sum((W_species - mean(W_species))^2)
        
        r2 <- ifelse(SST > 0, 1 - SSE/SST, NA_real_)
        r2_values <- c(r2_values, r2)
        
        if(!is.na(r2)) {
          cat(sprintf("    %s: R² = %.3f\n", 
                      group_rows$species_ru[i], r2))
        }
      }
      
      mean_r2 <- mean(r2_values, na.rm = TRUE)
      cat(sprintf("  Средний R² по группе: %.3f\n", mean_r2))
    }
  }
  
  # Сохраняем
  filename <- paste0("LW_groups_final_", Sys.Date(), ".xlsx")
  
  # Создаем Excel файл с несколькими листами
  wb <- createWorkbook()
  
  # Лист 1: Основные данные
  addWorksheet(wb, "Группы_видов")
  writeData(wb, "Группы_видов", export_table)
  
  # Лист 2: Сводная информация по группам
  group_summary <- export_table %>%
    group_by(group_id) %>%
    summarise(
      `Количество видов` = n(),
      `Общее количество измерений` = sum(n_measurements_species),
      `Формула группы` = sprintf("W = %.6f × L^%.3f", 
                                 first(a_group), 
                                 first(b_group)),
      `Средний maxlength` = round(mean(maxlength), 1),
      `Диапазон длин (общий)` = sprintf("%.1f - %.1f см", 
                                        min(min_L_observed), 
                                        max(max_L_observed)),
      .groups = "drop"
    )
  
  addWorksheet(wb, "Сводная_информация")
  writeData(wb, "Сводная_информация", group_summary)
  
  saveWorkbook(wb, filename, overwrite = TRUE)
  
  cat("\n=== ФАЙЛ УСПЕШНО СОХРАНЕН ===\n")
  cat("Имя файла:", filename, "\n")
  cat("Количество строк:", nrow(export_table), "\n")
  cat("Количество групп:", length(unique(export_table$group_id)), "\n")
  
  return(filename)
}
  
  # И вызываем в observeEvent
  observeEvent(input$downloadData, {
    filename <- export_to_excel_final()
    showNotification(paste("Файл создан:", filename), type = "message", duration = 5)
  })
  


# ======================================================
#---------- SERVER - ФУНКЦИИ ДЛЯ ЭКСПОРТА В СЕТКУ ---------------
# ======================================================

# Простая информация о группах
output$export_info_simple <- renderPrint({
  req(grouped())
  
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1)
  
  cat("=== ИНФОРМАЦИЯ ===\n\n")
  cat("Всего групп с >1 видом:", nrow(group_stats), "\n")
  if (nrow(group_stats) > 0) {
    cat("Количество видов:\n")
    for (i in 1:min(5, nrow(group_stats))) {
      cat(sprintf("  Группа %d: %d видов\n", group_stats$group[i], group_stats$n_species[i]))
    }
    if (nrow(group_stats) > 5) {
      cat(sprintf("  ... и еще %d групп\n", nrow(group_stats) - 5))
    }
  }
})

# Список групп для экспорта
output$export_groups_list <- renderUI({
  req(grouped())
  
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(
      n_species = n(),
      species_names = paste(secies_name_ru, collapse = ", "),
      .groups = "drop"
    ) %>%
    filter(n_species > 1) %>%
    arrange(group)
  
  if (nrow(group_stats) == 0) {
    return(tags$p("Нет групп для экспорта"))
  }
  
  tagList(
    tags$p(tags$strong("Группы для экспорта:")),
    lapply(1:nrow(group_stats), function(i) {
      tags$div(
        style = "margin-bottom: 5px;",
        tags$strong(sprintf("Группа %d (%d видов):", 
                           group_stats$group[i], 
                           group_stats$n_species[i])),
        tags$br(),
        tags$span(style = "font-size: 0.9em; color: #666;",
                 group_stats$species_names[i])
      )
    })
  )
})

# Функция для создания графика группы (оптимизированная для сетки)
# Улучшенная версия с управлением размерами шрифтов
create_group_plot_for_grid <- function(group_data, group_id, style = "color", 
                                       font_size = 10, 
                                       title_size_mult = 0.9,
                                       axis_size_mult = 0.8,
                                       show_title = TRUE) {
  
  if (nrow(group_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Подготовка данных
  plot_list <- list()
  colors <- scales::hue_pal()(nrow(group_data))
  line_types <- 1:nrow(group_data)
  
  # Создаем базовый график
  p <- ggplot()
  
  # Добавляем линии для каждого вида
  for (i in 1:nrow(group_data)) {
    L <- seq(1, group_data$maxlength[i], length.out = 50)
    W <- group_data$a[i] * L^group_data$b[i]
    
    line_data <- data.frame(length = L, weight = W, species = i)
    
    if (style == "color") {
      p <- p + geom_line(data = line_data, 
                        aes(x = length, y = weight), 
                        color = colors[i],
                        linewidth = 0.7)
    } else if (style == "bw") {
      p <- p + geom_line(data = line_data, 
                        aes(x = length, y = weight), 
                        color = "black",
                        linetype = line_types[i],
                        linewidth = 0.6)
    } else if (style == "bw_dots") {
      p <- p + geom_line(data = line_data, 
                        aes(x = length, y = weight), 
                        color = "black",
                        linetype = "dashed",
                        linewidth = 0.3) +
        geom_point(data = line_data,
                  aes(x = length, y = weight),
                  color = "black",
                  size = 0.5,
                  shape = line_types[i])
    } else {
      p <- p + geom_line(data = line_data, 
                        aes(x = length, y = weight), 
                        color = "black",
                        linewidth = 0.5,
                        alpha = 0.7)
    }
  }
  
# --------- Подготовка заголовка -----------


  title_text <- if (show_title) {
    sprintf("Группа %d (n=%d)", group_id, nrow(group_data))
  } else {
    NULL
  }
  
  # Базовая настройка с управлением размерами шрифтов
  p <- p +
    labs(x = "Длина, см", 
         y = "Вес, г",
         title = title_text) +
    theme_minimal(base_size = font_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.2, color = "grey90"),
      panel.border = element_rect(fill = NA, color = "grey70", linewidth = 0.3),
      plot.title = if (show_title) {
        element_text(hjust = 0.5, 
                    size = font_size * title_size_mult,
                    face = "bold",
                    margin = margin(b = 5))
      } else {
        element_blank()
      },
      axis.title = element_text(size = font_size * axis_size_mult),
      axis.text = element_text(size = font_size * 0.7),
      plot.margin = unit(c(3, 3, 3, 3), "mm"),
      legend.position = "none"
    )
  
  # Масштабирование осей
  max_length <- max(sapply(1:nrow(group_data), function(i) group_data$maxlength[i]))
  max_weight <- max(sapply(1:nrow(group_data), function(i) {
    group_data$a[i] * group_data$maxlength[i]^group_data$b[i]
  }))
  
  p <- p +
    coord_cartesian(
      xlim = c(0, max_length * 1.05),
      ylim = c(0, max_weight * 1.1)
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4))
  
  return(p)
}

# Предпросмотр сетки
grid_preview_data <- reactive({
  req(grouped(), input$export_ncol)
  
  # Находим группы с >1 видом
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1)
  
  if (nrow(group_stats) == 0) return(NULL)
  
  # Берем первые N групп для предпросмотра
  n_groups_preview <- min(4, nrow(group_stats))
  preview_groups <- head(group_stats$group, n_groups_preview)
  
  list(
    groups = preview_groups,
    n_total = nrow(group_stats)
  )
})

# График предпросмотра сетки
output$export_grid_preview <- renderPlot({
  req(grid_preview_data(), grouped())
  
  data_list <- grid_preview_data()
  ncol <- input$export_ncol
  
  # Рассчитываем количество строк
  n_groups <- length(data_list$groups)
  nrow <- ceiling(n_groups / ncol)
  
  # Создаем графики для каждой группы
  plot_list <- list()
  for (i in seq_along(data_list$groups)) {
    group_id <- data_list$groups[i]
    group_data <- grouped()$table %>% filter(group == group_id)
    
    plot_list[[i]] <- create_group_plot_for_grid(
      group_data = group_data,
      group_id = group_id,
      style = input$export_style,
      font_size = input$export_font_size
    )
  }
  
  # Создаем сетку
  grid_plot <- cowplot::plot_grid(
    plotlist = plot_list,
    ncol = ncol,
    nrow = nrow,
    align = "hv",
    axis = "lb"
  )
  
  # Добавляем заголовок
  title <- ggdraw() + 
    draw_label(
      sprintf("Группы регрессий длина-вес (показано %d из %d групп)", 
              n_groups, data_list$n_total),
      fontface = 'bold',
      size = input$export_font_size * 1.2
    )
  
  cowplot::plot_grid(
    title,
    grid_plot,
    ncol = 1,
    rel_heights = c(0.05, 0.95)
  )
})

# Экспорт всех групп в одну сетку
observeEvent(input$export_grid, {
  req(grouped(), input$export_ncol)
  
  # Создаем папку если не существует
  export_folder <- input$export_folder
  if (!dir.exists(export_folder)) {
    dir.create(export_folder, recursive = TRUE)
  }
  
  # Находим все группы с >1 видом
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1) %>%
    arrange(group)
  
  if (nrow(group_stats) == 0) {
    showNotification("Нет групп для экспорта", type = "warning", duration = 5)
    return()
  }
  
  # Прогресс бар
  withProgress(message = 'Создание сетки графиков...', value = 0, {
    
    # Создаем графики для всех групп
    plot_list <- list()
    for (i in seq_len(nrow(group_stats))) {
      incProgress(1/nrow(group_stats), 
                 detail = paste("Группа", group_stats$group[i]))
      
      group_data <- grouped()$table %>% filter(group == group_stats$group[i])
      
      plot_list[[i]] <- create_group_plot_for_grid(
        group_data = group_data,
        group_id = group_stats$group[i],
        style = input$export_style,
        font_size = input$export_font_size
      )
    }
    
    # Рассчитываем количество строк
    ncol <- input$export_ncol
    nrow <- if (input$export_auto_height) {
      ceiling(nrow(group_stats) / ncol)
    } else {
      input$export_nrow
    }
    
    # Создаем сетку
    grid_plot <- cowplot::plot_grid(
      plotlist = plot_list,
      ncol = ncol,
      nrow = nrow,
      align = "hv",
      axis = "lb"
    )
    
    # Добавляем заголовок
    title <- ggdraw() + 
      draw_label(
        "Группы регрессий длина-вес",
        fontface = 'bold',
        size = 14
      )
    
    final_plot <- cowplot::plot_grid(
      title,
      grid_plot,
      ncol = 1,
      rel_heights = c(0.03, 0.97)
    )
    
    # Формируем имя файла
    filename <- sprintf("%s/%s_%d_групп.%s",
                       export_folder,
                       input$export_filename,
                       nrow(group_stats),
                       input$export_format)
    
    # Сохраняем
    ggsave(filename, final_plot,
           width = input$export_width,
           height = input$export_height,
           units = "cm",
           dpi = input$export_dpi)
    
  })
  
  # Уведомление об успехе
  showNotification(
    paste("Сетка из", nrow(group_stats), "графиков сохранена в", filename),
    type = "message",
    duration = 10
  )
})

# Экспорт каждого графика отдельно
observeEvent(input$export_individual, {
  req(grouped())
  
  export_folder <- input$export_folder
  if (!dir.exists(export_folder)) {
    dir.create(export_folder, recursive = TRUE)
  }
  
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1) %>%
    arrange(group)
  
  if (nrow(group_stats) == 0) {
    showNotification("Нет групп для экспорта", type = "warning", duration = 5)
    return()
  }
  
  withProgress(message = 'Экспорт отдельных графиков...', value = 0, {
    
    for (i in seq_len(nrow(group_stats))) {
      incProgress(1/nrow(group_stats), 
                 detail = paste("Группа", group_stats$group[i]))
      
      group_data <- grouped()$table %>% filter(group == group_stats$group[i])
      
      p <- create_group_plot_for_grid(
        group_data = group_data,
        group_id = group_stats$group[i],
        style = input$export_style,
        font_size = input$export_font_size
      )
      
      filename <- sprintf("%s/группа_%02d_%d_видов.%s",
                         export_folder,
                         group_stats$group[i],
                         group_stats$n_species[i],
                         input$export_format)
      
      ggsave(filename, p,
             width = input$export_width / 2,
             height = input$export_height / 2,
             units = "cm",
             dpi = input$export_dpi)
    }
  })
  
  showNotification(
    paste("Экспортировано", nrow(group_stats), "отдельных графиков"),
    type = "message",
    duration = 10
  )
})

# ======================================================
# SERVER - ФУНКЦИИ ДЛЯ ОБЩЕГО ГРАФИКА (С ИСПОЛЬЗОВАНИЕМ calculate_mean_curve)
# ======================================================

# Функция для расчета данных группы (воронки) с использованием calculate_mean_curve
calculate_group_funnel <- function(group_data, group_id, n_points = 100) {
  if (nrow(group_data) < 2) return(NULL)
  
  # Определяем диапазон длин для группы
  max_length <- max(group_data$maxlength)
  L <- seq(1, max_length, length.out = n_points)
  
  # Рассчитываем все линии группы
  lines_matrix <- matrix(NA, nrow = n_points, ncol = nrow(group_data))
  for (i in 1:nrow(group_data)) {
    lines_matrix[, i] <- group_data$a[i] * L^group_data$b[i]
  }
  
  # Рассчитываем границы
  min_line <- apply(lines_matrix, 1, min, na.rm = TRUE)
  max_line <- apply(lines_matrix, 1, max, na.rm = TRUE)
  
  # Используем существующую функцию для средней линии
  mean_curve <- calculate_mean_curve(group_data, n_points = n_points, Lmin = 1, power = 3)
  
  # Если функция вернула NULL, вычисляем простую среднюю
  if (is.null(mean_curve)) {
    mean_line <- apply(lines_matrix, 1, mean, na.rm = TRUE)
    # Аппроксимируем параметры a и b через регрессию
    if (all(mean_line > 0)) {
      log_mean <- log(mean_line)
      log_length <- log(L)
      lm_fit <- lm(log_mean ~ log_length)
      a_mean <- exp(coef(lm_fit)[1])
      b_mean <- coef(lm_fit)[2]
    } else {
      a_mean <- exp(mean(log(group_data$a)))
      b_mean <- mean(group_data$b)
    }
  } else {
    mean_line <- mean_curve$weight
    # Аппроксимируем параметры из средней кривой
    if (all(mean_line > 0)) {
      log_mean <- log(mean_line)
      log_length <- log(L)
      lm_fit <- lm(log_mean ~ log_length)
      a_mean <- exp(coef(lm_fit)[1])
      b_mean <- coef(lm_fit)[2]
    } else {
      a_mean <- exp(mean(log(group_data$a)))
      b_mean <- mean(group_data$b)
    }
  }
  
  # Создаем данные для воронки
  funnel_data <- tibble(
    group = group_id,
    length = L,
    min_weight = min_line,
    max_weight = max_line,
    mean_weight = mean_line,
    a_mean = a_mean,
    b_mean = b_mean,
    n_species = nrow(group_data)
  )
  
  return(funnel_data)
}

# Основной график с воронками (исправленная версия)
# Основной график с воронками (полная версия)
output$overall_plot <- renderPlot({
  req(grouped(), input$overall_groups_select)
  
  selected_groups <- as.numeric(input$overall_groups_select)
  if (length(selected_groups) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = "Выберите группы для отображения", size = 8) +
        theme_void()
    )
  }
  
  # Собираем данные для выбранных групп
  all_funnel_data <- list()
  all_mean_data <- list()
  
  for (group_id in selected_groups) {
    group_data <- grouped()$table %>% filter(group == group_id)
    if (nrow(group_data) >= 2) {
      # Данные воронки
      funnel_data <- calculate_group_funnel(group_data, group_id, n_points = 200)
      if (!is.null(funnel_data)) {
        all_funnel_data[[as.character(group_id)]] <- funnel_data
        
        # Данные для средней линии
        mean_line <- tibble(
          group = group_id,
          length = funnel_data$length,
          weight = funnel_data$mean_weight,
          n_species = funnel_data$n_species[1],
          formula = sprintf("W = %.4f × L^{%.3f}", 
                           funnel_data$a_mean[1], 
                           funnel_data$b_mean[1])
        )
        all_mean_data[[as.character(group_id)]] <- mean_line
      }
    }
  }
  
  if (length(all_funnel_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = "Нет данных для выбранных групп", size = 8) +
        theme_void()
    )
  }
  
  # Объединяем данные
  funnel_df <- bind_rows(all_funnel_data)
  mean_df <- bind_rows(all_mean_data)
  
  # Создаем цветовую палитру
  n_groups <- length(unique(funnel_df$group))
  group_colors <- get_group_colors(n_groups)
  
  # Создаем базовый график
  p <- ggplot()
  
  # Добавляем воронки (заливку)
  if (input$overall_style != "mean_only" && input$overall_show_bounds) {
    for (i in seq_along(unique(funnel_df$group))) {
      group_id <- unique(funnel_df$group)[i]
      group_data <- funnel_df %>% filter(group == group_id)
      
      # Создаем полигон для воронки
      funnel_polygon <- bind_rows(
        group_data %>% select(length, weight = min_weight),
        group_data %>% arrange(desc(length)) %>% 
          select(length, weight = max_weight)
      )
      
      if (input$overall_style == "color") {
        p <- p +
          geom_polygon(data = funnel_polygon,
                      aes(x = length, y = weight),
                      fill = group_colors[i],
                      alpha = input$overall_funnel_alpha,
                      color = NA)
      } else {
        p <- p +
          geom_polygon(data = funnel_polygon,
                      aes(x = length, y = weight),
                      fill = "grey80",
                      alpha = input$overall_funnel_alpha,
                      color = NA)
      }
    }
  }
  
  # Добавляем границы воронок
  if (input$overall_style != "mean_only" && input$overall_show_bounds) {
    if (input$overall_style == "color") {
      p <- p +
        geom_line(data = funnel_df,
                 aes(x = length, y = min_weight, group = group, color = factor(group)),
                 linewidth = 0.3, linetype = "dashed", alpha = 0.5) +
        geom_line(data = funnel_df,
                 aes(x = length, y = max_weight, group = group, color = factor(group)),
                 linewidth = 0.3, linetype = "dashed", alpha = 0.5)
    } else {
      p <- p +
        geom_line(data = funnel_df,
                 aes(x = length, y = min_weight, group = group),
                 linewidth = 0.3, linetype = "dashed", color = "grey50", alpha = 0.5) +
        geom_line(data = funnel_df,
                 aes(x = length, y = max_weight, group = group),
                 linewidth = 0.3, linetype = "dashed", color = "grey50", alpha = 0.5)
    }
  }
  
  # Добавляем средние линии
  if (input$overall_show_mean && nrow(mean_df) > 0) {
    if (input$overall_style == "color") {
      p <- p +
        geom_line(data = mean_df,
                 aes(x = length, y = weight, color = factor(group)),
                 linewidth = input$overall_mean_size)
    } else if (input$overall_style == "bw") {
      p <- p +
        geom_line(data = mean_df,
                 aes(x = length, y = weight, linetype = factor(group)),
                 linewidth = input$overall_mean_size,
                 color = "black")
    } else {
      p <- p +
        geom_line(data = mean_df,
                 aes(x = length, y = weight, group = group),
                 linewidth = input$overall_mean_size,
                 color = "black")
    }
  }
  
  # Настройки темы и осей
  p <- p +
    labs(x = "Длина, см", 
         y = "Вес, г",
         title = "Общий график групп регрессий длина-вес",
         color = if (input$overall_show_legend && input$overall_style == "color") "Группа" else NULL,
         linetype = if (input$overall_show_legend && input$overall_style == "bw") "Группа" else NULL) +
    theme_minimal(base_size = input$overall_font_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = input$overall_font_size * 1.2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "grey90"),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      legend.position = if (input$overall_show_legend) "right" else "none",
      legend.background = element_rect(fill = "white", color = "grey70"),
      legend.key = element_rect(fill = "white")
    ) +
    coord_cartesian(
      xlim = c(0, input$overall_x_max),
      ylim = c(0, input$overall_y_max)
    )
  
  # Цветовая шкала для цветного стиля
  if (input$overall_style == "color") {
    p <- p + scale_color_manual(values = group_colors)
  }
  
  # Добавляем аннотации с формулами
  if (input$overall_show_mean && nrow(mean_df) > 0) {
    # Берем последние точки для размещения формул
    formula_positions <- mean_df %>%
      group_by(group) %>%
      summarise(
        x = max(length) * 0.95,
        y = max(weight) * 0.95,
        formula = first(formula),
        n_species = first(n_species),
        .groups = "drop"
      )
    
    p <- p +
      geom_label(data = formula_positions,
                aes(x = x, y = y, label = sprintf("Гр.%d: %s", group, formula)),
                size = input$overall_font_size * 0.3,
                hjust = 1,
                vjust = 1,
                alpha = 0.8,
                label.size = 0.2)
  }
  
  return(p)
})

# Таблица с формулами групп (обновленная)
output$overall_formulas_table <- renderDT({
  req(grouped(), input$overall_groups_select)
  
  selected_groups <- as.numeric(input$overall_groups_select)
  if (length(selected_groups) == 0) return(NULL)
  
  formulas_list <- list()
  
  for (group_id in selected_groups) {
    group_data <- grouped()$table %>% filter(group == group_id)
    if (nrow(group_data) >= 2) {
      # Используем calculate_mean_curve для получения точной формулы
      mean_curve <- calculate_mean_curve(group_data, n_points = 100, Lmin = 1, power = 3)
      
      if (!is.null(mean_curve)) {
        # Аппроксимируем параметры из средней кривой
        L <- mean_curve$length
        W <- mean_curve$weight
        if (all(W > 0)) {
          log_W <- log(W)
          log_L <- log(L)
          lm_fit <- lm(log_W ~ log_L)
          a_mean <- exp(coef(lm_fit)[1])
          b_mean <- coef(lm_fit)[2]
          
          formulas_list[[as.character(group_id)]] <- tibble(
            Группа = group_id,
            `Кол-во видов` = nrow(group_data),
            `Среднее a` = round(a_mean, 6),
            `Среднее b` = round(b_mean, 3),
            Формула = sprintf("W = %.4f × L^{%.3f}", a_mean, b_mean),
            `Мин. длина` = round(min(group_data$min_length), 1),
            `Макс. длина` = round(max(group_data$maxlength), 1),
            `Мощность взвешивания` = 3  # power parameter из calculate_mean_curve
          )
        }
      }
    }
  }
  
  if (length(formulas_list) == 0) return(NULL)
  
  formulas_df <- bind_rows(formulas_list)
  
  datatable(
    formulas_df,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      dom = 'Bfrtip',
      scrollX = TRUE,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json'
      )
    )
  )
})

# Функция для экспорта общего графика с использованием calculate_mean_curve
observeEvent(input$overall_export, {
  tryCatch({
    # Временно перехватываем график
    plot_to_save <- output$overall_plot()
    
    export_folder <- "plotsResult"
    if (!dir.exists(export_folder)) {
      dir.create(export_folder, recursive = TRUE)
    }
    
    filename <- file.path(
      export_folder,
      sprintf("общий_график_групп_%s.png", format(Sys.time(), "%Y%m%d_%H%M"))
    )
    
    # Сохраняем график
    ggsave(filename, plot = plot_to_save,
           width = input$overall_export_width,
           height = input$overall_export_height,
           units = "cm",
           dpi = 300)
    
    showNotification(
      HTML(paste(
        "✅ График сохранен:<br>",
        tags$strong(basename(filename)), "<br>",
        sprintf("Размер: %.1f × %.1f см", 
                input$overall_export_width, 
                input$overall_export_height)
      )),
      type = "message",
      duration = 10
    )
    
  }, error = function(e) {
    showNotification(paste("Ошибка при экспорте:", e$message), 
                    type = "error", duration = 10)
  })
})



# ======================================================
# SERVER - РЕДАКТОР КОДА
# ======================================================

# Реактивное значение для хранения кода
custom_plot_code <- reactiveVal(NULL)

# Загрузка групп для редактирования
observeEvent(grouped(), {
  req(grouped())
  
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1) %>%
    arrange(group)
  
  if (nrow(group_stats) > 0) {
    choices <- setNames(
      group_stats$group,
      sprintf("Группа %d (%d spec.)", group_stats$group, group_stats$n_species)
    )
    
    updateSelectInput(session, "edit_group", choices = choices)
  }
})

# Функция для генерации базового кода графика
generate_base_plot_code <- function(group_id, style = "main", font_size = 10,
                                    show_points = FALSE, show_ci = FALSE,
                                    title = NULL, xlab = NULL, ylab = NULL) {
  
  req(grouped())
  
  group_data <- grouped()$table %>% filter(group == group_id)
  if (nrow(group_data) == 0) return(NULL)
  
  # Базовая структура кода
  base_code <- paste(
    '# =======================================',
    '# КОД ГРАФИКА ГРУППЫ', group_id,
    '# Автоматически сгенерирован',
    '# =======================================',
    '',
    '# Загрузка необходимых библиотек',
    'library(ggplot2)',
    'library(dplyr)',
    '',
    '# Данные группы',
    sprintf('group_data <- data.frame('),
    sprintf('  species = c(%s),', paste0('"', group_data$secies_name_ru, '"', collapse = ", ")),
    sprintf('  a = c(%s),', paste(round(group_data$a, 6), collapse = ", ")),
    sprintf('  b = c(%s),', paste(round(group_data$b, 3), collapse = ", ")),
    sprintf('  maxlength = c(%s)', paste(group_data$maxlength, collapse = ", ")),
    ')',
    '',
    '# Функция для создания данных линий',
    'create_lines_data <- function() {',
    '  lines_list <- list()',
    '  for(i in 1:nrow(group_data)) {',
    '    L <- seq(1, group_data$maxlength[i], length.out = 100)',
    '    W <- group_data$a[i] * L^group_data$b[i]',
    '    lines_list[[i]] <- data.frame(',
    '      species = group_data$species[i],',
    '      length = L,',
    '      weight = W',
    '    )',
    '  }',
    '  return(bind_rows(lines_list))',
    '}',
    '',
    '# Создание данных',
    'plot_data <- create_lines_data()',
    '',
    '# Базовый график',
    'p <- ggplot(plot_data, aes(x = length, y = weight, color = species)) +',
    '  geom_line(linewidth = 1.2) +',
    sep = "\n"
  )
  
  # Добавление стилей
  if (style == "minimal") {
    style_code <- paste(
      '  theme_minimal(base_size = %s) +',
      '  theme(',
      '    panel.grid.minor = element_blank(),',
      '    panel.border = element_rect(fill = NA, color = "grey50"),',
      '    legend.position = "bottom"',
      '  )',
      sep = "\n"
    )
  } else if (style == "publication") {
    style_code <- paste(
      '  theme_bw(base_size = %s) +',
      '  theme(',
      '    panel.grid.minor = element_blank(),',
      '    panel.grid.major = element_line(linewidth = 0.5),',
      '    plot.title = element_text(hjust = 0.5, face = "bold"),',
      '    legend.position = "none"',
      '  )',
      sep = "\n"
    )
  } else if (style == "bw") {
    style_code <- paste(
      '  scale_color_grey(start = 0.1, end = 0.8) +',
      '  theme_bw(base_size = %s) +',
      '  theme(',
      '    legend.position = "bottom",',
      '    plot.title = element_text(hjust = 0.5)',
      '  )',
      sep = "\n"
    )
  } else {
    style_code <- paste(
      '  theme_minimal(base_size = %s) +',
      '  theme(',
      '    legend.position = "bottom",',
      '    plot.title = element_text(hjust = 0.5)',
      '  )',
      sep = "\n"
    )
  }
  
  # Замена плейсхолдера размером шрифта
  style_code <- sprintf(style_code, font_size)
  
  # Добавление подписей
  labels_code <- paste(
    sprintf('  labs('),
    sprintf('    title = "%s",', if(!is.null(title)) title else sprintf("Группа %d: регрессии длина-вес", group_id)),
    sprintf('    x = "%s",', if(!is.null(xlab)) xlab else "Длина, см"),
    sprintf('    y = "%s",', if(!is.null(ylab)) ylab else "Вес, г"),
    '    color = "Вид"',
    '  ) +',
    sep = "\n"
  )
  
  # Добавление точек данных если нужно
  points_code <- ""
  if (show_points) {
    points_code <- paste(
      '',
      '# Добавление точек данных',
      'if (exists("data_gap_filtered")) {',
      '  points_data <- data_gap_filtered() %>%',
      sprintf('    filter(species %%in%% group_data$species)'),
      '  p <- p + geom_point(data = points_data,',
      '                     aes(x = length, y = weight, color = secies_name_ru),',
      '                     size = 2, alpha = 0.5)',
      '}',
      sep = "\n"
    )
  }
  
  # Добавление доверительных интервалов если нужно
  ci_code <- ""
  if (show_ci) {
    ci_code <- paste(
      '',
      '# Добавление доверительных интервалов (пример)',
      'p <- p +',
      '  geom_ribbon(aes(ymin = weight * 0.9, ymax = weight * 1.1, fill = species),',
      '              alpha = 0.1, show.legend = FALSE)',
      sep = "\n"
    )
  }
  
  # Собираем весь код
  final_code <- paste(base_code, labels_code, style_code, points_code, ci_code, sep = "\n")
  
  return(final_code)
}

# Обновление редактора при изменении параметров
observeEvent(input$edit_group, {
  req(input$edit_group)
  
  base_code <- generate_base_plot_code(
    group_id = as.numeric(input$edit_group),
    style = input$edit_style,
    font_size = input$edit_font_size,
    show_points = input$edit_show_points,
    show_ci = input$edit_show_ci
  )
  
  if (!is.null(base_code)) {
    updateAceEditor(session, "code_editor", value = base_code)
    custom_plot_code(base_code)
  }
})

# Применение быстрых правок
observeEvent(input$apply_quick_edit, {
  req(input$edit_group)
  
  base_code <- generate_base_plot_code(
    group_id = as.numeric(input$edit_group),
    style = input$edit_style,
    font_size = input$edit_font_size,
    show_points = input$edit_show_points,
    show_ci = input$edit_show_ci,
    title = input$edit_title,
    xlab = input$edit_xlab,
    ylab = input$edit_ylab
  )
  
  if (!is.null(base_code)) {
    updateAceEditor(session, "code_editor", value = base_code)
    custom_plot_code(base_code)
    
    showNotification("Быстрые правки применены", type = "message", duration = 3)
  }
})

# Выполнение кода и предпросмотр
output$edit_preview_plot <- renderPlot({
  req(input$execute_code > 0 || input$update_preview > 0)
  
  isolate({
    code_to_execute <- input$code_editor
    
    if (is.null(code_to_execute) || code_to_execute == "") {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Введите код графика", size = 8) +
          theme_void()
      )
    }
    
    # Создаем защищенное окружение для выполнения кода
    env <- new.env()
    
    # Добавляем необходимые данные в окружение
    if (exists("grouped")) {
      env$grouped_data <- grouped()$table
    }
    if (exists("data_gap_filtered")) {
      env$data_gap_filtered <- data_gap_filtered()
    }
    
    # Выполняем код
    tryCatch({
      eval(parse(text = code_to_execute), envir = env)
      
      # Проверяем, создан ли график
      if (exists("p", envir = env)) {
        return(env$p)
      } else {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                    label = "Код не создал график 'p'", size = 6) +
            theme_void()
        )
      }
    }, error = function(e) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = paste("Ошибка выполнения:\n", e$message), 
                  size = 6) +
          theme_void()
      )
    })
  })
})

# Сохранение кода в файл
observeEvent(input$save_code, {
  code_to_save <- input$code_editor
  
  if (!is.null(code_to_save) && code_to_save != "") {
    filename <- input$code_filename
    if (!grepl("\\.R$", filename) && !grepl("\\.r$", filename)) {
      filename <- paste0(filename, ".R")
    }
    
    writeLines(code_to_save, filename)
    
    showNotification(
      sprintf("Код сохранен в файл: %s", filename),
      type = "message",
      duration = 5
    )
  }
})

# Загрузка кода из файла
observeEvent(input$load_code, {
  req(input$load_code)
  
  file <- input$load_code
  code <- readLines(file$datapath)
  
  updateAceEditor(session, "code_editor", value = paste(code, collapse = "\n"))
  custom_plot_code(paste(code, collapse = "\n"))
  
  showNotification("Код загружен из файла", type = "message", duration = 3)
})

# Сброс к исходному коду
observeEvent(input$reset_code, {
  req(input$edit_group)
  
  base_code <- generate_base_plot_code(
    group_id = as.numeric(input$edit_group),
    style = input$edit_style,
    font_size = input$edit_font_size,
    show_points = input$edit_show_points,
    show_ci = input$edit_show_ci
  )
  
  if (!is.null(base_code)) {
    updateAceEditor(session, "code_editor", value = base_code)
    custom_plot_code(base_code)
    
    showNotification("Код сброшен к исходному", type = "info", duration = 3)
  }
})

# Кнопки обновления предпросмотра
observeEvent(input$update_preview, {
  # Просто обновляем график
  output$edit_preview_plot <- renderPlot({
    req(input$code_editor)
    
    # ... тот же код выполнения что и выше ...
    # (дублируем для реактивности)
    code_to_execute <- input$code_editor
    
    env <- new.env()
    if (exists("grouped")) {
      env$grouped_data <- grouped()$table
    }
    if (exists("data_gap_filtered")) {
      env$data_gap_filtered <- data_gap_filtered()
    }
    
    tryCatch({
      eval(parse(text = code_to_execute), envir = env)
      
      if (exists("p", envir = env)) {
        return(env$p)
      } else {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                    label = "Код не создал график 'p'", size = 6) +
            theme_void()
        )
      }
    }, error = function(e) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = paste("Ошибка выполнения:\n", e$message), 
                  size = 6) +
          theme_void()
      )
    })
  })
})


 # Динамический список выбранных групп с видами
output$overall_groups_details <- renderUI({
  req(grouped(), input$overall_groups_select)
  
  selected_groups <- as.numeric(input$overall_groups_select)
  if (length(selected_groups) == 0) {
    return(tags$p("Группы не выбраны", style = "color: grey; font-style: italic;"))
  }
  
  # Создаем аккордеон для каждой группы
  accordion_items <- lapply(selected_groups, function(group_id) {
    group_data <- grouped()$table %>% filter(group == group_id)
    
    if (nrow(group_data) == 0) return(NULL)
    
    # Цвет для группы
 get_group_color_by_id <- function(group_id, n_total_groups, style = "color") {
  if (style != "color") return("#666666")
  
  # Используем фиксированную палитру
  colors <- get_group_colors_fixed(n_total_groups)
  
  # Находим индекс цвета для этой группы
  group_index <- which(selected_groups == group_id)
  if (length(group_index) > 0 && group_index <= length(colors)) {
    return(colors[group_index])
  } else {
    # Запасной вариант
    hue <- (group_id * 137) %% 360
    return(sprintf("hsl(%d, 70%%, 60%%)", hue))
  }
}
    
    # Создаем элемент аккордеона
    tags$div(
      class = "panel panel-default",
      style = "margin-bottom: 5px; border-left: 4px solid;",
      style = sprintf("border-left-color: %s;", group_color),
      
      # Заголовок аккордеона
      tags$div(
        class = "panel-heading",
        style = "padding: 8px 15px; cursor: pointer; background-color: #f8f9fa;",
        `data-toggle` = "collapse",
        `data-target` = paste0("#collapse-group-", group_id),
        `aria-expanded` = "false",
        `aria-controls` = paste0("collapse-group-", group_id),
        
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tags$span(
            tags$strong(sprintf("Группа %d", group_id)),
            sprintf(" (%d spec.)", nrow(group_data))
          ),
          tags$span(
            style = sprintf("color: %s; font-weight: bold;", group_color),
            "▾"
          )
        )
      ),
      
      # Содержимое аккордеона
      tags$div(
        id = paste0("collapse-group-", group_id),
        class = "panel-collapse collapse",
        
        tags$div(
          class = "panel-body",
          style = "padding: 10px 15px; max-height: 200px; overflow-y: auto;",
          
          # Виды в группе
          tags$p(tags$strong("Виды:")),
          tags$ul(
            style = "margin-bottom: 5px; padding-left: 20px;",
            lapply(1:nrow(group_data), function(i) {
              tags$li(
                style = "margin-bottom: 2px; font-size: 0.9em;",
                group_data$secies_name_ru[i],
                tags$span(
                  style = "color: #666; font-size: 0.85em; margin-left: 5px;",
                  sprintf("(n=%d)", group_data$n[i])
                )
              )
            })
          ),
          
          # Статистика группы
          tags$p(tags$strong("Статистика:")),
          tags$ul(
            style = "margin-bottom: 5px; padding-left: 20px; font-size: 0.9em;",
            tags$li(sprintf("Наблюдений: %d", sum(group_data$n))),
            tags$li(sprintf("Длина: %.1f-%.1f см", 
                           min(group_data$min_length), 
                           max(group_data$maxlength))),
            tags$li(sprintf("b: %.3f ± %.3f", 
                           mean(group_data$b), 
                           sd(group_data$b)))
          )
        )
      )
    )
  })
  
  # Удаляем NULL элементы
  accordion_items <- Filter(Negate(is.null), accordion_items)
  
  # Обертка для аккордеона
  tagList(
    tags$div(
      class = "panel-group",
      id = "overall-groups-accordion",
      accordion_items
    ),
    
    # JavaScript для работы аккордеона
    tags$script('
      $(document).ready(function() {
        $(".panel-heading").click(function() {
          $(this).find("span:last").text(function(_, text) {
            return text === "▾" ? "▴" : "▾";
          });
        });
      });
    ')
  )
})
  
  
  # Обновление списка групп при изменении выбора
observe({
  req(grouped())
  
  # Получаем все группы с >1 видом
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1) %>%
    arrange(group)
  
  if (nrow(group_stats) > 0) {
    # Создаем метки для групп
    group_labels <- sapply(1:nrow(group_stats), function(i) {
      sprintf("Группа %d (%d sp.)", 
              group_stats$group[i], 
              group_stats$n_species[i])
    })
    
    # Обновляем выбор групп
    updateCheckboxGroupInput(
      session,
      "overall_groups_select",
      choices = setNames(group_stats$group, group_labels),
      selected = if (is.null(input$overall_groups_select)) {
        head(group_stats$group, min(3, nrow(group_stats)))
      } else {
        # Сохраняем текущий выбор если он есть
        intersect(input$overall_groups_select, group_stats$group)
      }
    )
  }
})

# Кнопки выбора/очистки всех групп
observeEvent(input$overall_select_all, {
  req(grouped())
  
  group_stats <- grouped()$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1)
  
  if (nrow(group_stats) > 0) {
    updateCheckboxGroupInput(
      session,
      "overall_groups_select",
      selected = group_stats$group
    )
  }
})

observeEvent(input$overall_clear_all, {
  updateCheckboxGroupInput(
    session,
    "overall_groups_select",
    selected = character(0)
  )
})


# Функция для создания цветовой палитры групп
get_group_colors <- function(n_groups) {
  if (n_groups <= 0) return(character(0))
  
  if (n_groups <= 8) {
    # Качественные цвета для малого количества групп
    colors <- c(
      "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",  # красный, синий, зеленый, фиолетовый
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF"   # оранжевый, желтый, коричневый, розовый
    )
    return(colors[1:n_groups])
    
  } else if (n_groups <= 12) {
    # Дополнительные цвета
    colors <- c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
      "#cab2d6", "#6a3d9a", "#ffff99", "#b15928"
    )
    return(colors[1:n_groups])
    
  } else {
    # Градиент для большого количества групп
    return(colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))(n_groups))
  }
}



}

# Запуск приложения
shinyApp(ui = ui, server = server)