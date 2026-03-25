# app/statistics_module.R

# ============================================
# ОСНОВНЫЕ СТАТИСТИЧЕСКИЕ ФУНКЦИИ
# ============================================

# Функция для форматирования статистики для вывода
format_model_stats <- function(stats, species_name, ribbon_percent = NULL, outliers_data = NULL) {
  if(is.null(stats)) {
    return("Недостаточно данных для построения моделей\n")
  }
  
  # Используем capture.output для правильного форматирования
  output <- capture.output({
    cat("========================================\n")
    cat("СТАТИСТИЧЕСКИЙ АНАЛИЗ\n")
    cat("========================================\n\n")
    cat("ВИД:", species_name, "\n")
    cat("Наблюдений:", stats$n, "\n")
    cat("Диапазон длины:", stats$length_range, "\n")
    cat("Диапазон веса:", stats$weight_range, "\n\n")
    
    cat("=== СТЕПЕННАЯ МОДЕЛЬ (ln(W) ~ ln(L)) ===\n")
    cat("Формула: W = a × L^b\n")
    cat("  a =", round(stats$power$a, 6), "\n")
    cat("  b =", round(stats$power$b, 3), "\n")
    cat("  R2 =", round(stats$power$r2, 4), "\n")
    cat("  AIC =", round(stats$power$aic, 2), "\n\n")
    
    cat("=== ЭКСПОНЕНЦИАЛЬНАЯ МОДЕЛЬ (ln(W) ~ L) ===\n")
    cat("Формула: W = a × e^(b×L)\n")
    cat("  a =", round(stats$exp$a, 6), "\n")
    cat("  b =", round(stats$exp$b, 5), "\n")
    cat("  R2 =", round(stats$exp$r2, 4), "\n")
    cat("  AIC =", round(stats$exp$aic, 2), "\n\n")
    
    cat("=== СРАВНЕНИЕ МОДЕЛЕЙ ===\n")
    aic_diff <- stats$exp$aic - stats$power$aic
    cat("AIC (Exp - Power) =", round(aic_diff, 2), "\n")
    
    if(abs(aic_diff) < 2) {
      cat("Модели примерно одинакового качества\n")
    } else if(aic_diff < 0) {
      cat("Экспоненциальная модель ЛУЧШЕ (AIC =", 
          round(-aic_diff, 2), ")\n")
    } else {
      cat("Степенная модель ЛУЧШЕ (AIC =", 
          round(aic_diff, 2), ")\n")
    }
    
    # Добавляем информацию о выбросах если ribbon активен
    if(!is.null(ribbon_percent)) {
      if(!is.null(outliers_data)) {
        n_outliers <- sum(outliers_data$is_outlier, na.rm = TRUE)
        cat("\n=== ИНФОРМАЦИЯ О ВЫБРОСАХ (критерий: ±", ribbon_percent, "%) ===\n")
        cat("Выбросов обнаружено:", n_outliers, "из", nrow(outliers_data), "\n")
        cat("Процент выбросов:", round(100 * n_outliers / nrow(outliers_data), 1), "%\n")
      }
    }
  })
  
  # Возвращаем результат как строку с переносами
  return(paste(output, collapse = "\n"))
}

# Функции для статистического анализа моделей
calculate_model_stats <- function(sp_data) {
  if(nrow(sp_data) < 3) return(NULL)
  
  tryCatch({
    power_model <- lm(logW ~ logL, data = sp_data)
    exp_model <- lm(logW ~ length, data = sp_data)
    
    list(
      power = list(
        model = power_model,
        a = exp(coef(power_model)[1]),
        b = coef(power_model)[2],
        r2 = summary(power_model)$r.squared,
        aic = AIC(power_model)
      ),
      exp = list(
        model = exp_model,
        a = exp(coef(exp_model)[1]),
        b = coef(exp_model)[2],
        r2 = summary(exp_model)$r.squared,
        aic = AIC(exp_model)
      ),
      n = nrow(sp_data),
      length_range = paste(round(min(sp_data$length), 1), "-", 
                           round(max(sp_data$length), 1), "см"),
      weight_range = paste(round(min(sp_data$weight), 1), "-", 
                           round(max(sp_data$weight), 1), "г")
    )
  }, error = function(e) {
    NULL
  })
}

# Функция сравнения моделей
compare_models <- function(stats) {
  if(is.null(stats)) return(NULL)
  
  aic_diff <- stats$exp$aic - stats$power$aic
  
  list(
    aic_power = round(stats$power$aic, 2),
    aic_exp = round(stats$exp$aic, 2),
    delta_aic = round(aic_diff, 2),
    best_model = ifelse(abs(aic_diff) < 2, "Модели примерно одинакового качества",
                        ifelse(aic_diff < 0, "Экспоненциальная", "Степенная")),
    strength = ifelse(abs(aic_diff) < 2, "",
                      ifelse(abs(aic_diff) < 4, "заметно",
                             ifelse(abs(aic_diff) < 7, "значительно", "существенно"))),
    abs_delta = round(abs(aic_diff), 2)
  )
}

# Функция для обнаружения выбросов
detect_outliers <- function(sp_data, stats, threshold_percent) {
  if(is.null(stats) || nrow(sp_data) < 5) return(NULL)
  
  # Используем лучшую модель для определения выбросов
  aic_diff <- stats$exp$aic - stats$power$aic
  best_model <- ifelse(aic_diff < 0, "exp", "power")
  
  if(best_model == "power") {
    model <- stats$power$model
    pred <- exp(predict(model))
  } else {
    model <- stats$exp$model
    pred <- exp(predict(model))
  }
  
  # Рассчитываем отклонения в процентах
  sp_data <- sp_data %>%
    mutate(
      predicted_weight = pred,
      deviation_percent = 100 * (weight - predicted_weight) / predicted_weight,
      abs_deviation = abs(deviation_percent),
      is_outlier = abs_deviation > threshold_percent
    )
  
  return(sp_data)
}

# Функция для создания предсказаний с доверительными интервалами
create_predictions_with_ribbon <- function(sp_data, stats, ribbon_percent) {
  if(is.null(stats) || nrow(sp_data) < 3) return(NULL)
  
  x_range <- seq(min(sp_data$length), max(sp_data$length), length.out = 100)
  
  # Предсказания для степенной модели
  power_pred <- exp(predict(stats$power$model, 
                            newdata = data.frame(logL = log(x_range))))
  
  # Предсказания для экспоненциальной модели
  exp_pred <- exp(predict(stats$exp$model, 
                          newdata = data.frame(length = x_range)))
  
  # Создаем ribbon данные (верхняя и нижняя границы)
  ribbon_ratio <- ribbon_percent / 100
  
  data.frame(
    length = x_range,
    Power = power_pred,
    Power_upper = power_pred * (1 + ribbon_ratio),
    Power_lower = power_pred * (1 - ribbon_ratio),
    Exp = exp_pred,
    Exp_upper = exp_pred * (1 + ribbon_ratio),
    Exp_lower = exp_pred * (1 - ribbon_ratio)
  )
}

# Таблица сравнения моделей для рендеринга
create_model_comparison_table <- function(stats) {
  if(is.null(stats)) return(NULL)
  
  result_df <- data.frame(
    Модель = c("Степенная", "Экспоненциальная"),
    Формула = c("W = a × L^b", "W = a × e^(b×L)"),
    `Параметр a` = c(round(stats$power$a, 6), round(stats$exp$a, 6)),
    `Параметр b` = c(round(stats$power$b, 3), round(stats$exp$b, 5)),
    `R2` = c(round(stats$power$r2, 4), round(stats$exp$r2, 4)),
    AIC = c(round(stats$power$aic, 2), round(stats$exp$aic, 2)),
    stringsAsFactors = FALSE
  )
  
  return(result_df)
}

# ============================================
# ФУНКЦИИ АНАЛИЗА РАЗРЫВОВ
# ============================================

# Функция для проверки разрывов в размерном ряду
check_length_gaps <- function(lengths, Lmin = 3, maxl) {
  
  # оставляем только конечные значения >= Lmin
  L <- sort(lengths[is.finite(lengths) & lengths >= Lmin])
  
  # если данных недостаточно — сразу бракуем
  if (length(L) < 2) {
    return(data.frame(
      min_L_observed = ifelse(length(L) == 0, NA_real_, min(L)),
      max_L_observed = ifelse(length(L) == 0, NA_real_, max(L)),
      max_gap_cm = NA_real_,
      gap_ratio = NA_real_,
      pass_gap_filter = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  # Проверяем maxl - должен быть один элемент
  if(length(maxl) > 1) {
    maxl <- maxl[1]
  }
  
  # Проверяем maxl на NA и корректность
  if(is.na(maxl) || !is.finite(maxl) || maxl <= Lmin) {
    # Если maxl некорректен, используем максимальное наблюденное значение
    maxl <- max(L, na.rm = TRUE)
  }
  
  # Создаем полный массив точек от Lmin до maxl, включая все промежуточные точки данных
  all_points <- unique(c(Lmin, L, maxl))
  all_points <- sort(all_points)
  
  # Рассчитываем разрывы между всеми соседними точками
  gaps <- diff(all_points)
  
  # Безопасное вычисление max_gap
  if(length(gaps) == 0) {
    max_gap <- NA_real_
  } else {
    max_gap <- max(gaps, na.rm = TRUE)
    if(!is.finite(max_gap)) max_gap <- NA_real_
  }
  
  range_total <- maxl - Lmin
  
  # Проверяем range_total
  if(range_total <= 0 || !is.finite(range_total)) {
    return(data.frame(
      min_L_observed = min(L),
      max_L_observed = max(L),
      max_gap_cm = max_gap,
      gap_ratio = NA_real_,
      gap_percent = NA_real_,
      pass_gap_filter = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  gap_ratio <- max_gap / range_total
  
  data.frame(
    min_L_observed = min(L),
    max_L_observed = max(L),
    max_gap_cm = max_gap,
    gap_ratio = gap_ratio,
    gap_percent = 100 * gap_ratio,
    pass_gap_filter = is.finite(gap_ratio) && gap_ratio <= 1,
    stringsAsFactors = FALSE
  )
}

# Функция для расчета диагностики разрывов
calc_gap_diagnostics <- function(data, Lmin) {
  
  # Проверяем наличие необходимых колонок
  if(!"species" %in% colnames(data)) {
    cat("ВНИМАНИЕ: колонка 'species' не найдена. Используем первую доступную...\n")
    cat("Доступные колонки:", paste(colnames(data), collapse=", "), "\n")
    return(NULL)
  }
  
  # Проверяем наличие was_cleaned
  if(!"was_cleaned" %in% colnames(data)) {
    cat("ДОБАВЛЯЕМ колонку was_cleaned = TRUE\n")
    data$was_cleaned <- TRUE
  }
  
  # Используем species (строчная s)
  result <- tryCatch({
    data %>%
      filter(
        was_cleaned,
        length > 0,
        is.finite(length)
      ) %>%
      group_by(species) %>%  # Только species
      summarise(
        maxlength = ifelse("maxlength" %in% colnames(.),
                           first(maxlength, na_rm = TRUE),
                           max(length, na.rm = TRUE)),
        gap_info = list(
          check_length_gaps(
            lengths = pick(length)[[1]],
            Lmin = Lmin,
            maxl = first(maxlength, na_rm = TRUE)
          )
        ),
        .groups = "drop"
      ) %>%
      tidyr::unnest(gap_info)
  }, error = function(e) {
    cat("Ошибка в calc_gap_diagnostics:", e$message, "\n")
    return(NULL)
  })
  
  return(result)
}

# Функция для фильтрации данных по разрывам
filter_by_gaps <- function(data, Lmin, gap_threshold) {
  
  cat("\n=== DEBUG: filter_by_gaps START ===\n")
  cat("Входные данные: строк =", nrow(data), ", видов =", 
      length(unique(data$species)), "\n")
  cat("Lmin =", Lmin, ", gap_threshold =", gap_threshold, "\n")
  
  if(is.null(data) || nrow(data) == 0) {
    cat("DEBUG: Нет входных данных\n")
    return(list(
      filtered_data = data,
      gap_diagnostics = NULL,
      stats = NULL
    ))
  }
  
  # Проверяем наличие колонки species
  if(!"species" %in% colnames(data)) {
    cat("DEBUG: колонка 'species' не найдена. Доступные колонки:", 
        paste(colnames(data), collapse=", "), "\n")
    return(list(
      filtered_data = data,
      gap_diagnostics = NULL,
      stats = NULL
    ))
  }




  
  # Рассчитываем диагностику разрывов
  gap_diagnostics <- calc_gap_diagnostics(data, Lmin)
  
  cat("DEBUG: gap_diagnostics получено: строк =", 
      ifelse(!is.null(gap_diagnostics), nrow(gap_diagnostics), 0), "\n")
  
  # Применяем фильтр
  if(!is.null(gap_diagnostics) && nrow(gap_diagnostics) > 0) {
    
    cat("\nDEBUG: Анализ gap_diagnostics:\n")
    cat("Колонки:", paste(colnames(gap_diagnostics), collapse=", "), "\n")

    
    # gap_threshold в диапазоне 0-1
    gap_diagnostics <- gap_diagnostics %>%
      mutate(
        gap_threshold_used = gap_threshold,
        pass_gap_filter = gap_ratio <= gap_threshold | is.na(gap_ratio)
      ) %>%
      arrange(desc(gap_ratio))
    
    # Подсчет сколько видов проходит фильтр
    n_passed <- sum(gap_diagnostics$pass_gap_filter, na.rm = TRUE)
    n_failed <- sum(!gap_diagnostics$pass_gap_filter, na.rm = TRUE)
    

    cat("DEBUG: Видов проходит фильтр:", n_passed, "\n")
    cat("DEBUG: Видов НЕ проходит фильтр:", n_failed, "\n")
    



    if (n_failed > 0) {
      cat("\nDEBUG: Виды, не прошедшие фильтр (первые 5):\n")
      failed_species <- gap_diagnostics %>%
        filter(!pass_gap_filter) %>%
        head(5)
      print(failed_species)
    }
    
    # Получаем виды, прошедшие фильтрацию
    passed_species <- gap_diagnostics %>%
      filter(pass_gap_filter) %>%
      pull(species)
    
    # Фильтруем данные
    filtered_data <- data %>%
      filter(species %in% passed_species)
    
    # Статистика фильтрации
    total_species <- length(unique(data$species))
    passed_species_count <- length(unique(filtered_data$species))
    removed_species_count <- total_species - passed_species_count




    cat("DEBUG: Статистика фильтрации:\n")
    cat("  Всего видов:", total_species, "\n")
    cat("  Прошло видов:", passed_species_count, "\n")
    cat("  Удалено видов:", removed_species_count, "\n")
    
    stats <- list(
      total_species = total_species,
      passed_species = passed_species_count,
      removed_species = removed_species_count,
      removed_percent = round(100 * removed_species_count / total_species, 1)
    )
    
  } else {
    cat("DEBUG: gap_diagnostics пусто или NULL\n")
    filtered_data <- data
    stats <- list(
      total_species = length(unique(data$species)),
      passed_species = length(unique(data$species)),
      removed_species = 0,
      removed_percent = 0
    )
  }
  
  
  return(list(
    filtered_data = filtered_data,
    gap_diagnostics = gap_diagnostics,
    stats = stats
  ))
}

# Функция для форматирования статистики фильтрации разрывов
format_gap_filter_stats <- function(gap_results, Lmin, gap_threshold) {
  if(is.null(gap_results$gap_diagnostics)) {
    return("Диагностика разрывов недоступна.\n")
  }
  
  gap_diagnostics <- gap_results$gap_diagnostics
  gap_stats <- gap_results$stats
  
  output <- capture.output({
    cat("=== СТАТИСТИКА ФИЛЬТРАЦИИ ПО РАЗРЫВАМ ===\n\n")
    cat(sprintf("Всего видов: %d\n", gap_stats$total_species))
    cat(sprintf("Прошло фильтр: %d (%.1f%%)\n", 
                gap_stats$passed_species, 
                gap_stats$passed_species/gap_stats$total_species*100))
    cat(sprintf("Отбраковано: %d (%.1f%%)\n", 
                gap_stats$removed_species, 
                gap_stats$removed_percent))
    cat(sprintf("\nПараметры фильтра:\n"))
    cat(sprintf("• Минимальная длина (Lmin): %.1f см\n", Lmin))
    cat(sprintf("• Допустимый разрыв (gap_threshold): %.3f\n", gap_threshold))
    cat(sprintf("• Формула: gap_ratio = max_gap / (maxlength - Lmin)\n"))
    
    # Топ-5 видов с наибольшими разрывами
    if (nrow(gap_diagnostics) > 0) {
      cat("\nТОП-5 ВИДОВ С НАИБОЛЬШИМИ РАЗРЫВАМИ:\n")
      
      top_gap_species <- gap_diagnostics %>%
        filter(!is.na(gap_ratio)) %>%
        arrange(desc(gap_ratio)) %>%
        head(5)
      
      for (i in 1:nrow(top_gap_species)) {
        species_name <- top_gap_species$Species[i]
        max_gap_cm <- top_gap_species$max_gap_cm[i]
        gap_ratio <- top_gap_species$gap_ratio[i]
        gap_percent <- round(100 * gap_ratio, 1)
        passed <- top_gap_species$pass_gap_filter[i]
        
        status <- ifelse(passed, "✓ ПРОШЕЛ", 
                        paste0("✗ УДАЛЕН (>", gap_threshold, ")"))
        cat(sprintf("%d. %s: max_gap=%.1fсм, ratio=%.3f (%.1f%%) %s\n", 
                    i, species_name, max_gap_cm, gap_ratio, gap_percent, status))
      }
    }
  })
  
  return(paste(output, collapse = "\n"))
}

# ============================================
# ФУНКЦИИ ДЛЯ ОЧИСТКИ ДАННЫХ
# ============================================

# Функция для форматирования статистики очистки
format_cleaning_stats <- function(cleaning_result, gap_results = NULL, 
                                  gap_Lmin = NULL, gap_threshold = NULL) {
  if(is.null(cleaning_result)) {
    return("Очистка еще не выполнялась.\nНажмите кнопку 'Запустить очистку' для начала процесса.\n")
  }
  
  # Используем capture.output для правильного форматирования
  output <- capture.output({
    cat("=== РЕЗУЛЬТАТЫ ОЧИСТКИ ===\n\n")
    
    # Параметры очистки
    if (!is.null(cleaning_result$stats$parameters)) {
      cat("ПАРАМЕТРЫ ОЧИСТКИ:\n")
      
      params <- cleaning_result$stats$parameters
      
      if (!is.null(params$final_threshold)) {
        cat("Порог отклонения:", params$final_threshold, "%\n")
      }
      
      if (!is.null(params$model_type)) {
        model_name <- ifelse(params$model_type == "power",
                            "Степенная (W = a × L^b)", 
                            "Экспоненциальная (W = a × e^(b×L))")
        cat("Модель:", model_name, "\n")
      }
    }
    
    # Время выполнения
    if (!is.null(cleaning_result$stats$execution_time)) {
      cat("Время выполнения:", round(cleaning_result$stats$execution_time, 1), "сек\n")
    }
    
    cat("\n")
    
    # Статистика по видам
    if (!is.null(cleaning_result$stats$species_counts)) {
      species_counts <- cleaning_result$stats$species_counts
      
      cat("СТАТИСТИКА ПО ВИДАМ:\n")
      
      if (!is.null(species_counts$total_species)) {
        cat("Всего видов в данных:", species_counts$total_species, "\n")
      }
      
      if (!is.null(species_counts$processed_species)) {
        cat("Успешно обработано:", species_counts$processed_species, "\n")
      }
    }
    
    cat("\n")
    
    # Объем данных
    if (!is.null(cleaning_result$stats$data_counts)) {
      data_counts <- cleaning_result$stats$data_counts
      
      cat("ОБЪЕМ ДАННЫХ:\n")
      
      if (!is.null(data_counts$total_initial)) {
        cat("Исходное количество наблюдений:", data_counts$total_initial, "\n")
      }
      
      if (!is.null(data_counts$total_final)) {
        cat("После очистки:", data_counts$total_final, "\n")
      }
      
      if (!is.null(data_counts$total_outliers)) {
        cat("Удалено выбросов:", data_counts$total_outliers, "\n")
      }
      
      if (!is.null(data_counts$total_removed)) {
        cat("Всего удалено:", data_counts$total_removed, "\n")
      }
    }
    
    # Добавляем статистику фильтрации разрывов если есть
    if(!is.null(gap_results) && !is.null(gap_results$stats)) {
      cat("\n=== ФИЛЬТРАЦИЯ ПО РАЗРЫВАМ ===\n")
      cat(sprintf("Видов после очистки: %d\n", 
                  length(unique(cleaning_result$clean_data$species))))
      cat(sprintf("Видов после фильтрации разрывов: %d\n", 
                  gap_results$stats$passed_species))
      cat(sprintf("Удалено видов по разрывам: %d\n", 
                  gap_results$stats$removed_species))
      
      if(!is.null(gap_Lmin) && !is.null(gap_threshold)) {
        cat(sprintf("Параметры фильтра: Lmin=%.1fсм, threshold=%.3f\n", 
                    gap_Lmin, gap_threshold))
      }

    }
  })
  
  # Возвращаем результат как строку с переносами
  return(paste(output, collapse = "\n"))
}

# Функция для создания графика группы
create_group_plot <- function(grouped_data, current_group, 
                               show_mean = FALSE, 
                               show_ci = FALSE, 
                               ci_width = 10, 
                               power = 3) {
  req(grouped_data, current_group)
  
  tbl_grp <- grouped_data$table %>% 
    filter(group == current_group)
  
  if(nrow(tbl_grp) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = "Нет данных для группы", size = 6) +
             theme_void())
  }
  
  # Вычисляем доверительный интервал
  ci_factor <- if (show_ci) ci_width / 100 else 0
  
  # Создаем линии для каждого вида с доверительными интервалами
  lines_data <- purrr::map_dfr(1:nrow(tbl_grp), function(i) {
    # Используем maxlength каждого вида как максимальную длину для кривой
    L <- seq(1, tbl_grp$maxlength[i], length.out = 100)
    W <- tbl_grp$a[i] * L^tbl_grp$b[i]
    
    tibble(
      species = tbl_grp$species[i],
      species_latin = tbl_grp$species[i],
      length = L,
      weight = W,
      low = W * (1 - ci_factor),
      high = W * (1 + ci_factor),
      type = "species"
    )
  })
  
  # Создаем график
  p <- ggplot(lines_data, aes(x = length, y = weight, color = species)) +
    geom_line(linewidth = 1.2) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Группа", current_group),
      x = "Длина, см",
      y = "Вес, г",
      color = "Вид"
    ) +
    theme(legend.position = "bottom")
  
  # Добавляем доверительные интервалы для видов (БЕЗ ГРАНИЦ)
  if (show_ci && ci_factor > 0) {
    p <- p + 
      geom_ribbon(aes(ymin = low, ymax = high, fill = species), 
                  alpha = 0.2, 
                  color = NA,  # Убираем границы
                  linetype = 0, # Убираем линии границ
                  show.legend = FALSE)
  }
  
  # Добавляем среднюю линию с учетом maxlength
  if (show_mean && nrow(tbl_grp) > 1) {
    mean_line <- calculate_mean_curve(tbl_grp, n_points = 100, Lmin = 1, power = power)
    
    if (!is.null(mean_line)) {
      # Добавляем доверительный интервал для средней линии (БЕЗ ГРАНИЦ)
      if (show_ci && ci_factor > 0) {
        mean_line$low <- mean_line$weight * (1 - ci_factor)
        mean_line$high <- mean_line$weight * (1 + ci_factor)
        
        p <- p +
          geom_ribbon(data = mean_line,
                     aes(x = length, ymin = low, ymax = high),
                     fill = "gray70",
                     alpha = 0.3,
                     color = NA,      # Убираем границы
                     linetype = 0,    # Убираем линии границ
                     inherit.aes = FALSE)
      }
      
      # Добавляем среднюю линию (черный пунктир)
      p <- p +
        geom_line(data = mean_line,
                 aes(x = length, y = weight),
                 color = "black",
                 linewidth = 1.5,
                 linetype = "dashed",
                 inherit.aes = FALSE)
    }
  }
  
  return(p)
}

# Вспомогательная функция для пустого графика
create_empty_plot <- function(message) {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, 
             label = message, size = 6) +
    theme_void()
}


# Функция для графика размеров групп
create_group_sizes_plot <- function(grouped_data) {
  req(grouped_data)
  
  group_stats <- grouped_data$table %>%
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Функция для форматирования статистики группировки

# statistics_module.R - ИСПРАВЛЕННАЯ ВЕРСИЯ

format_grouping_stats <- function(grouped_data, grouping_params = NULL) {
  if(is.null(grouped_data)) {
    return("Группировка не выполнена.\nСначала выполните очистку данных, затем запустите группировку.")
  }
  
  tbl <- grouped_data$table
  
  # Проверка на пустую таблицу
  if(is.null(tbl) || nrow(tbl) == 0) {
    return("Нет данных в таблице групп.")
  }
  
  total_species <- nrow(tbl)
  total_groups <- length(unique(tbl$group))
  
  output <- capture.output({
    cat("=== СТАТИСТИКА ГРУППИРОВКИ ===\n\n")
    
    cat("📊 ОБЩАЯ СТАТИСТИКА:\n")
    cat(sprintf("Всего видов: %d\n", total_species))
    cat(sprintf("Всего групп: %d\n", total_groups))
    
    # Простая статистика без сложных расчетов
    cat("\n🏷️ ГРУППЫ:\n")
    group_summary <- tbl %>%
      group_by(group) %>%
      summarise(
        n_species = n(),
        species_names = paste(species, collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(desc(n_species))
    
    for(i in 1:nrow(group_summary)) {
      cat(sprintf("\nГруппа %d (%d видов):\n", 
                  group_summary$group[i], 
                  group_summary$n_species[i]))
      cat(sprintf("  %s\n", group_summary$species_names[i]))
      
      # Попробуем рассчитать формулу, но с защитой
      if(group_summary$n_species[i] > 1) {
        group_tbl <- tbl %>% filter(group == group_summary$group[i])
        mean_result <- tryCatch({
          calculate_mean_curve(group_tbl)
        }, error = function(e) NULL)
        
        if(!is.null(mean_result) && !is.null(mean_result$formula)) {
          cat(sprintf("  Формула: %s\n", mean_result$formula$formula_text))
          if(!is.na(mean_result$formula$r2)) {
            cat(sprintf("  R² = %.3f\n", mean_result$formula$r2))
          }
        }
      }
    }
    
    # Статистика по типам групп
    singleton_groups <- group_summary %>% filter(n_species == 1) %>% nrow()
    multi_groups <- total_groups - singleton_groups
    
    cat(sprintf("\n📈 СВОДНАЯ СТАТИСТИКА:\n"))
    cat(sprintf("• Групп с >1 вида: %d\n", multi_groups))
    cat(sprintf("• Одиночных видов: %d\n", singleton_groups))
  })
  
  return(paste(output, collapse = "\n"))
}
  

create_groups_summary_table <- function(grouped_data) {
  if(is.null(grouped_data)) {
    return(data.frame(Сообщение = "Нет данных"))
  }
  
  tbl <- grouped_data$table
  
  if(is.null(tbl) || nrow(tbl) == 0) {
    return(data.frame(Сообщение = "Нет данных в таблице"))
  }
  
  # ПРОСТАЯ таблица без формул
  summary <- tbl %>%
    group_by(group) %>%
    summarise(
      `Кол-во видов` = n(),
      `Наблюдений всего` = sum(n),
      `Мин. длина (см)` = round(min(min_length, na.rm = TRUE), 1),
      `Макс. длина (см)` = round(max(max_length, na.rm = TRUE), 1),
      `Предельная длина (см)` = round(max(maxlength, na.rm = TRUE), 1),
      `Средний параметр b` = round(mean(b, na.rm = TRUE), 3),
      `SD параметра b` = ifelse(n() > 1, round(sd(b, na.rm = TRUE), 3), NA),
      `Тип группы` = ifelse(n() > 1, "Мульти-видовая", "Одиночная"),
      `Виды` = paste(species, collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(`Кол-во видов`), group)
  
  return(summary)
}

create_clustering_stats_text <- function(grouped_data) {
  if (is.null(grouped_data) || is.null(grouped_data$table) || nrow(grouped_data$table) == 0) {
    return("Группировка не выполнена.")
  }

  tbl <- grouped_data$table
  group_sizes <- tbl %>% count(group, name = "n_species")

  output <- capture.output({
    cat("=== СТАТИСТИКА КЛАСТЕРИЗАЦИИ ===\n\n")
    cat(sprintf("Всего видов: %d\n", nrow(tbl)))
    cat(sprintf("Всего групп: %d\n", nrow(group_sizes)))
    cat(sprintf("Одиночных групп: %d\n", sum(group_sizes$n_species == 1)))
    cat(sprintf("Мульти-видовых групп: %d\n", sum(group_sizes$n_species > 1)))
    cat(sprintf("Средний размер группы: %.2f\n", mean(group_sizes$n_species)))
  })

  paste(output, collapse = "\n")
}

create_group_parameter_distribution_plot <- function(grouped_data) {
  if (is.null(grouped_data) || is.null(grouped_data$table) || nrow(grouped_data$table) == 0) {
    return(create_empty_plot("Нет данных для распределения параметров"))
  }

  ggplot(grouped_data$table, aes(x = b, fill = factor(group))) +
    geom_histogram(alpha = 0.7, bins = 20, position = "identity") +
    theme_minimal(base_size = 12) +
    labs(x = "Параметр b", y = "Частота", fill = "Группа") +
    theme(legend.position = "none")
}

create_single_species_plot <- function(grouped_data, selected_species, show_groups = TRUE, log_space = FALSE) {
  if (is.null(grouped_data) || is.null(grouped_data$table) || nrow(grouped_data$table) == 0) {
    return(create_empty_plot("Сначала выполните группировку"))
  }

  tbl <- grouped_data$table %>%
    group_by(group) %>%
    filter(n() == 1) %>%
    ungroup()

  if (nrow(tbl) == 0) {
    return(create_empty_plot("Нет одиночных видов (все виды сгруппированы)"))
  }

  selected <- tbl %>% filter(species %in% selected_species)
  if (nrow(selected) == 0) {
    return(create_empty_plot("Выберите хотя бы один одиночный вид"))
  }

  background_data <- purrr::map_dfr(seq_len(nrow(tbl)), function(i) {
    L <- seq(1, tbl$maxlength[i], length.out = 100)
    tibble(species = tbl$species[i], group = tbl$group[i], length = L, weight = tbl$a[i] * L^tbl$b[i])
  })

  selected_data <- purrr::map_dfr(seq_len(nrow(selected)), function(i) {
    L <- seq(1, selected$maxlength[i], length.out = 100)
    tibble(species = selected$species[i], length = L, weight = selected$a[i] * L^selected$b[i])
  })

  p <- ggplot()
  if (show_groups) {
    p <- p + geom_line(data = background_data, aes(length, weight, group = species), color = "grey80", alpha = 0.5)
  }

  p <- p +
    geom_line(data = selected_data, aes(length, weight, color = species), linewidth = 1.2) +
    theme_minimal(base_size = 13) +
    labs(x = "Длина, см", y = "Вес, г", color = "Одиночный вид")

  if (log_space) {
    p <- p + scale_x_log10() + scale_y_log10()
  }

  p
}

create_overall_groups_plot <- function(grouped_data,
                                       selected_species = NULL,
                                       selected_groups = NULL,
                                       show_mean = TRUE,
                                       show_ci = FALSE,
                                       ci_width = 10,
                                       alpha_funnel = 0.2,
                                       x_max = 80,
                                       y_max = 5000,
                                       mean_size = 1.4,
                                       show_bounds = TRUE,
                                       show_legend = TRUE,
                                       style = "color",
                                       font_size = 13,
                                       color_mode = "group") {
  if (is.null(grouped_data) || is.null(grouped_data$table) || nrow(grouped_data$table) == 0) {
    return(create_empty_plot("Сначала выполните группировку"))
  }

  tbl <- grouped_data$table %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    ungroup()

  if (!is.null(selected_groups) && length(selected_groups) > 0) {
    tbl <- tbl %>% filter(group %in% selected_groups)
  }

  if (!is.null(selected_species)) {
    tbl <- tbl %>% filter(species %in% selected_species)
  }

  tbl <- tbl %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    ungroup()

  if (nrow(tbl) == 0) {
    return(create_empty_plot("Нет мульти-видовых групп для отображения"))
  }

  species_lines <- purrr::map_dfr(seq_len(nrow(tbl)), function(i) {
    L <- seq(1, tbl$maxlength[i], length.out = 140)
    tibble(
      group = factor(tbl$group[i]),
      species = tbl$species[i],
      length = L,
      weight = tbl$a[i] * L^tbl$b[i]
    )
  })

  mean_data <- tbl %>%
    group_split(group) %>%
    purrr::map_dfr(function(g) {
      m <- calculate_mean_curve(g, n_points = 220, Lmin = 1, power = 3)

      if (is.null(m) || nrow(m) == 0) {
        L <- seq(1, max(g$maxlength, na.rm = TRUE), length.out = 220)
        a_w <- weighted.mean(g$a, w = g$n, na.rm = TRUE)
        b_w <- weighted.mean(g$b, w = g$n, na.rm = TRUE)
        m <- tibble(length = L, weight = a_w * L^b_w)
      }

      m %>%
        mutate(
          group = factor(first(g$group)),
          low = pmax(0, weight * (1 - ci_width / 100)),
          high = weight * (1 + ci_width / 100)
        )
    })

  color_by_species <- identical(color_mode, "species")

  base_plot <- ggplot(
    species_lines,
    aes(
      length,
      weight,
      group = interaction(group, species),
      color = if (color_by_species) species else group
    )
  ) +
    geom_line(alpha = 0.85, linewidth = 0.9)

  if (identical(style, "mean_only")) {
    p <- ggplot(mean_data, aes(length, weight, color = group))
  } else {
    p <- base_plot
  }

  if (isTRUE(show_ci) && isTRUE(show_bounds)) {
    p <- p +
      geom_ribbon(
        data = mean_data,
        aes(
          x = length,
          ymin = low,
          ymax = high,
          fill = group,
          group = group
        ),
        alpha = alpha_funnel,
        inherit.aes = FALSE,
        color = NA,
        show.legend = FALSE
      )
  }

  if (isTRUE(show_mean)) {
    if (color_by_species) {
      p <- p +
        geom_line(
          data = mean_data,
          aes(x = length, y = weight, group = group),
          color = "black",
          linetype = "dashed",
          linewidth = mean_size,
          inherit.aes = FALSE
        )
    } else {
      p <- p +
        geom_line(
          data = mean_data,
          aes(x = length, y = weight, color = group, group = group),
          linewidth = mean_size,
          inherit.aes = FALSE
        )
    }
  }

  if (identical(style, "bw")) {
    p <- p +
      scale_color_grey(start = 0.2, end = 0.7) +
      scale_fill_grey(start = 0.8, end = 0.4)
  }

  safe_x_max <- suppressWarnings(as.numeric(x_max)[1])
  safe_y_max <- suppressWarnings(as.numeric(y_max)[1])
  if (is.na(safe_x_max) || !is.finite(safe_x_max) || safe_x_max <= 0) {
    safe_x_max <- max(species_lines$length, na.rm = TRUE)
  }
  if (is.na(safe_y_max) || !is.finite(safe_y_max) || safe_y_max <= 0) {
    safe_y_max <- max(species_lines$weight, na.rm = TRUE)
  }

  p +
    coord_cartesian(xlim = c(0, safe_x_max), ylim = c(0, safe_y_max)) +
    theme_minimal(base_size = font_size) +
    labs(
      x = "Длина, см",
      y = "Вес, г",
      color = if (color_by_species) "Вид" else "Группа",
      fill = NULL
    ) +
    theme(legend.position = if (isTRUE(show_legend)) "right" else "none")
}


create_overall_formulas_table <- function(grouped_data, selected_species = NULL, selected_groups = NULL) {
  if (is.null(grouped_data) || is.null(grouped_data$table)) return(tibble())

  tbl <- grouped_data$table %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    ungroup()

  if (!is.null(selected_groups) && length(selected_groups) > 0) {
    tbl <- tbl %>% filter(group %in% selected_groups)
  }

  if (!is.null(selected_species)) {
    tbl <- tbl %>% filter(species %in% selected_species)
  }

  tbl %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    summarise(
      `Видов` = n(),
      `Средний a (взв.)` = round(weighted.mean(a, w = n, na.rm = TRUE), 6),
      `Средний b (взв.)` = round(weighted.mean(b, w = n, na.rm = TRUE), 3),
      `Формула` = sprintf("W = %.5f × L^%.3f", `Средний a (взв.)`, `Средний b (взв.)`),
      .groups = "drop"
    )
}

create_group_plot_for_grid <- function(group_data, group_id, style = "color",
                                       font_size = 10,
                                       title_size_mult = 0.9,
                                       axis_size_mult = 0.8,
                                       show_title = TRUE) {
  if (is.null(group_data) || nrow(group_data) == 0) {
    return(ggplot() + theme_void())
  }

  p <- ggplot()
  colors <- scales::hue_pal()(nrow(group_data))
  line_types <- seq_len(max(1, nrow(group_data)))

  for (i in seq_len(nrow(group_data))) {
    L <- seq(1, group_data$maxlength[i], length.out = 80)
    W <- group_data$a[i] * L^group_data$b[i]
    line_data <- tibble(length = L, weight = W)

    if (style == "color") {
      p <- p + geom_line(data = line_data, aes(length, weight), color = colors[i], linewidth = 0.7)
    } else if (style == "bw") {
      p <- p + geom_line(data = line_data, aes(length, weight), color = "black", linetype = line_types[i], linewidth = 0.6)
    } else if (style == "bw_dots") {
      p <- p +
        geom_line(data = line_data, aes(length, weight), color = "black", linetype = "dashed", linewidth = 0.35) +
        geom_point(data = line_data, aes(length, weight), color = "black", size = 0.5, shape = (line_types[i] %% 24) + 1)
    } else {
      p <- p + geom_line(data = line_data, aes(length, weight), color = "black", linewidth = 0.5, alpha = 0.7)
    }
  }

  title_text <- if (show_title) sprintf("Группа %d (n=%d)", group_id, nrow(group_data)) else NULL

  max_length <- max(group_data$maxlength, na.rm = TRUE)
  max_weight <- max(group_data$a * (group_data$maxlength ^ group_data$b), na.rm = TRUE)

  p +
    labs(x = "Длина, см", y = "Вес, г", title = title_text) +
    theme_minimal(base_size = font_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.2, color = "grey90"),
      panel.border = element_rect(fill = NA, color = "grey70", linewidth = 0.3),
      plot.title = if (show_title) element_text(hjust = 0.5, size = font_size * title_size_mult, face = "bold") else element_blank(),
      axis.title = element_text(size = font_size * axis_size_mult),
      axis.text = element_text(size = font_size * 0.7),
      plot.margin = unit(c(3, 3, 3, 3), "mm"),
      legend.position = "none"
    ) +
    coord_cartesian(xlim = c(0, max_length * 1.05), ylim = c(0, max_weight * 1.1)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4))
}

create_export_preview_plot <- function(grouped_data,
                                       ncol = 2,
                                       style = "color",
                                       font_size = 10,
                                       title_size_mult = 0.9,
                                       axis_size_mult = 0.8,
                                       show_titles = TRUE,
                                       preview_only = TRUE,
                                       selected_groups = NULL) {
  if (is.null(grouped_data) || is.null(grouped_data$table) || nrow(grouped_data$table) == 0) {
    return(create_empty_plot("Нет данных для экспорта"))
  }

  group_stats <- grouped_data$table %>%
    group_by(group) %>%
    summarise(n_species = n(), .groups = "drop") %>%
    filter(n_species > 1) %>%
    arrange(group)

  if (!is.null(selected_groups) && length(selected_groups) > 0) {
    group_stats <- group_stats %>% filter(group %in% selected_groups)
  }

  if (nrow(group_stats) == 0) {
    return(create_empty_plot("Нет групп с количеством видов > 1"))
  }

  groups_to_plot <- if (isTRUE(preview_only)) head(group_stats$group, 4) else group_stats$group

  plot_list <- purrr::map(groups_to_plot, function(group_id) {
    group_data <- grouped_data$table %>% filter(group == group_id)
    create_group_plot_for_grid(
      group_data = group_data,
      group_id = group_id,
      style = style,
      font_size = font_size,
      title_size_mult = title_size_mult,
      axis_size_mult = axis_size_mult,
      show_title = show_titles
    )
  })

  nrow_grid <- ceiling(length(groups_to_plot) / ncol)
  grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = ncol, nrow = nrow_grid, align = "hv", axis = "lb")

  title_text <- if (isTRUE(preview_only)) {
    sprintf("Группы регрессий длина-вес (показано %d из %d групп)", length(groups_to_plot), nrow(group_stats))
  } else {
    sprintf("Группы регрессий длина-вес (%d групп)", nrow(group_stats))
  }

  title <- cowplot::ggdraw() +
    cowplot::draw_label(title_text, fontface = "bold", size = font_size * 1.2)

  cowplot::plot_grid(title, grid_plot, ncol = 1, rel_heights = c(0.05, 0.95))
}


# Функция для экспорта очищенных данных
create_cleaned_data_export <- function(cleaning_results) {
  if (is.null(cleaning_results)) {
    return(NULL)
  }
  
  # Проверяем, какие данные есть
  if (!is.null(cleaning_results$gap_filtered)) {
    # Используем данные после фильтрации разрывов
    clean_data <- cleaning_results$gap_filtered
    data_source <- "После фильтрации разрывов"
  } else if (!is.null(cleaning_results$result)) {
    # Используем данные после очистки выбросов
    clean_data <- cleaning_results$result$clean_data
    data_source <- "После очистки выбросов"
  } else {
    return(NULL)
  }
  
  # Подготавливаем данные для экспорта
  export_data <- clean_data %>%
    mutate(
      length = round(length, 1),
      weight = round(weight, 2),
    ) %>%
    select(
      species = species,
      length = length,
      weight = weight,
      everything()  # остальные колонки если есть
    ) %>%
    arrange(species, length)
  
  # Добавляем информацию о статистике
  stats_info <- data.frame(
    Параметр = c("Дата экспорта", "Всего наблюдений", "Всего видов", 
                 "Источник данных", "Время очистки"),
    Значение = c(
      as.character(Sys.time()),
      nrow(clean_data),
      length(unique(clean_data$species)),
      data_source,
      if(!is.null(cleaning_results$timestamp)) 
        as.character(cleaning_results$timestamp) else "Неизвестно"
    )
  )
  
  return(list(
    data = export_data,
    stats = stats_info
  ))
}

add_length_density_weights <- function(df, length_col = "length", bin_width = 1) {
  L <- df[[length_col]]
  
  # Бины по длине (в см)
  bins <- cut(
    L,
    breaks = seq(floor(min(L)), ceiling(max(L)), by = bin_width),
    include.lowest = TRUE
  )
  
  bin_counts <- table(bins)
  
  # Вес обратно пропорционален плотности
  df$w_length_density <- 1 / as.numeric(bin_counts[bins])
  
  df
}

fit_lw_weighted <- function(df,
                            length_col = "length",
                            weight_col = "weight",
                            bin_width = 1) {
  
  df <- df %>%
    filter(
      is.finite(.data[[length_col]]),
      is.finite(.data[[weight_col]]),
      .data[[length_col]] > 0,
      .data[[weight_col]] > 0
    )
  
  if (nrow(df) < 5) return(NULL)
  
  # Добавляем веса
  df <- add_length_density_weights(df, length_col, bin_width)
  
  fit <- lm(
    log(.data[[weight_col]]) ~ log(.data[[length_col]]),
    data = df,
    weights = w_length_density
  )
  
  coefs <- coef(fit)
  
  list(
    a = exp(coefs[1]),
    b = coefs[2],
    model = fit
  )
}
