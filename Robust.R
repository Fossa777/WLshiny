options(encoding = "UTF-8")
Sys.setlocale("LC_ALL", "ru_RU.UTF-8")



clean_all_species_correct <- function(
    data,
    final_threshold = 10,
    model_type = "power",
    use_thinning = FALSE,
    min_final_n = 5,
    min_points_for_cleaning = 5,
    thinning_params = list(bin_L = 0.5, max_per_bin = 3, min_n = 10),
    verbose = TRUE
) {
  
  start_time <- Sys.time()
  
  if (verbose) {
    cat("=== НАЧАЛО ОЧИСТКИ ===\n")
    cat("Исходных строк:", nrow(data), "\n")
    cat("Исходные колонки:", paste(colnames(data), collapse=", "), "\n")
  }
  
  # 1. СОХРАНЯЕМ ВСЕ ИСХОДНЫЕ КОЛОНКИ
  # Определяем все исходные колонки
  original_columns <- colnames(data)
  
  # 2. ОЧИСТКА ДАННЫХ (сохраняем все колонки)
data_clean <- data %>%
  mutate(
    species = trimws(as.character(species)),  # ИСПРАВЛЕНО
    length = as.numeric(length),
    weight = as.numeric(weight)
  ) %>%
  filter(
    !is.na(species),  # ИСПРАВЛЕНО
    species != "",    # ИСПРАВЛЕНО
    !is.na(length), !is.na(weight),
    length > 0, weight > 0
  ) %>%
  distinct(species, length, weight, .keep_all = TRUE) 
  
  species_list <- unique(data_clean$species)
  
  all_clean <- list()
  all_outliers <- list()
  stats_summary <- data.frame()
  
  total_initial <- 0
  total_final <- 0
  total_outliers <- 0
  total_thinned <- 0
  
  # 3. ЦИКЛ ПО ВИДАМ
  for (i in seq_along(species_list)) {
    species <- species_list[i]
    sp_data <- data_clean[data_clean$species == species, ]
    n_obs <- nrow(sp_data)
    
    if (verbose && i <= 10) {
      cat(sprintf("\n[%d/%d] %s: %d точек", i, length(species_list), species, n_obs))
    }
    
    if (n_obs == 0) next
    
    total_initial <- total_initial + n_obs
    
    # Пропускаем виды с < 3 точек
    if (n_obs < 3) {
      if (verbose && i <= 10) cat(" - пропущен (<3 точек)\n")
      next
    }
    

    initial_n <- n_obs

    current_data <- sp_data

    
    # 6. ОЧИСТКА ВЫБРОСОВ
    outliers_list <- list()
    iteration <- 1
    
    # Копируем данные для очистки (сохраняем все колонки)
    data_for_cleaning <- current_data
    
    while (iteration <= 20 && nrow(data_for_cleaning) >= min_points_for_cleaning) {
      
      if (nrow(data_for_cleaning) < 3) break
      
      # Строим модель только на основе length и weight
      temp_data <- data_for_cleaning %>%
        mutate(
          logL = log(length),
          logW = log(weight)
        )
      
      model_robust <- tryCatch({
        MASS::rlm(logW ~ logL, data = temp_data, maxit = 20)
      }, error = function(e) {
        lm(logW ~ logL, data = temp_data)
      })
      
      pred_r <- exp(predict(model_robust))
      dev_pct <- abs(data_for_cleaning$weight - pred_r) / pred_r * 100
      
      strong_outliers <- which(dev_pct > final_threshold)
      
      if (length(strong_outliers) == 0) break
      
      worst_idx <- strong_outliers[which.max(dev_pct[strong_outliers])]
      worst_deviation <- dev_pct[worst_idx]
      
      # Сохраняем ВСЕ колонки выброса
      outliers_list[[iteration]] <- data_for_cleaning[worst_idx, ] %>%
        mutate(
          iteration_removed = iteration,
          deviation_percent = round(worst_deviation, 1),
          is_outlier = TRUE
        )
      
      data_for_cleaning <- data_for_cleaning[-worst_idx, , drop = FALSE]
      iteration <- iteration + 1
      
      if ((initial_n - nrow(data_for_cleaning)) > (initial_n * 0.3)) break
    }
    
    outliers_df <- bind_rows(outliers_list)
    outliers_removed <- nrow(outliers_df)
    total_outliers <- total_outliers + outliers_removed
    
    final_n <- nrow(data_for_cleaning)
    
    
    # 8. СОХРАНЕНИЕ ОЧИЩЕННЫХ ДАННЫХ (со ВСЕМИ колонками)
    clean_data <- data_for_cleaning %>%
      mutate(
        was_cleaned = TRUE,
        cleaning_iterations = iteration - 1,
        initial_n = initial_n,
        outliers_removed = outliers_removed,
        .before = 1
      )
    
    # 9. ОБРАБОТКА ВЫБРОСОВ
    if (nrow(outliers_df) > 0) {
      outliers_df <- outliers_df %>%
        mutate(
          was_cleaned = FALSE,
          is_outlier = TRUE
        )
    }
    
    all_clean[[species]] <- clean_data
    all_outliers[[species]] <- outliers_df
    total_final <- total_final + final_n
    
    # 10. СТАТИСТИКА
stats_summary <- bind_rows(
  stats_summary,
  data.frame(
    species = species,
    Initial_n = initial_n,
    Final_n = final_n,
    Outliers_removed = outliers_removed,
    Total_removed = outliers_removed,
    Iterations = iteration - 1,
    Percent_removed = ifelse(initial_n > 0, 
                             round(100 * outliers_removed / initial_n, 1),
                             0),  # ДОБАВЬТЕ ЭТУ СТРОКУ
    stringsAsFactors = FALSE
  )
)
    
    if (verbose && i <= 10) {
      total_removed <- outliers_removed 
      cat(sprintf(" - очистка: %d → %d (удалено: %d = %.1f%%)\n",
                  initial_n, final_n, total_removed,
                  round(100 * total_removed / initial_n, 1)))
    }
  }
  
  # 11. ОБЪЕДИНЕНИЕ РЕЗУЛЬТАТОВ
  clean_data_combined <- bind_rows(all_clean)
  outliers_combined <- bind_rows(all_outliers)
  
  total_removed <- total_initial - total_final
  
  # 12. ДОБАВЛЯЕМ logW и logL если их нет
  if (!"logW" %in% colnames(clean_data_combined)) {
    clean_data_combined <- clean_data_combined %>%
      mutate(
        logW = log(weight),
        logL = log(length)
      )
  }
  
  if (!"logW" %in% colnames(outliers_combined) && nrow(outliers_combined) > 0) {
    outliers_combined <- outliers_combined %>%
      mutate(
        logW = log(weight),
        logL = log(length)
      )
  }
  
  # 13. ИТОГОВАЯ СТАТИСТИКА
  total_stats <- list(
    timestamp = start_time,
    execution_time = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    parameters = list(
      final_threshold = final_threshold,
      model_type = model_type,
      min_final_n = min_final_n,
      min_points_for_cleaning = min_points_for_cleaning
    ),
    species_counts = list(
      total_species = length(species_list),
      processed_species = nrow(stats_summary),
      skipped_species = length(species_list) - nrow(stats_summary)
    ),
    data_counts = list(
      total_initial = total_initial,
      total_final = total_final,
      total_outliers = total_outliers,
      total_removed = total_removed
    ),
percentages = list(
  outliers_percent = if (total_initial > 0) 
    round(100 * total_outliers / total_initial, 1) else 0,
  total_removed_percent = if (total_initial > 0) 
    round(100 * total_removed / total_initial, 1) else 0,
  avg_species_removed = if (nrow(stats_summary) > 0) 
    round(mean(stats_summary$Percent_removed, na.rm = TRUE), 1) else 0  # БУДЕТ РАБОТАТЬ ТЕПЕРЬ
),
    integrity_check = list(
      expected_total = nrow(clean_data_combined) + 
        ifelse(!is.null(outliers_combined), nrow(outliers_combined), 0),
      actual_total = nrow(data_clean),
      discrepancy = nrow(data_clean) - (nrow(clean_data_combined) + 
                                          ifelse(!is.null(outliers_combined), 
                                                 nrow(outliers_combined), 0))
    )
  )
  
  if (verbose) {
    cat("\n\n=== РЕЗУЛЬТАТЫ ОЧИСТКИ ===\n")
    cat("Время выполнения:", round(total_stats$execution_time, 1), "сек\n")
    cat("Всего видов:", total_stats$species_counts$total_species, "\n")
    cat("Обработано видов:", total_stats$species_counts$processed_species, "\n")
    cat("Исходные точки:", total_stats$data_counts$total_initial, "\n")
    cat("Очищенные точки:", total_stats$data_counts$total_final, "\n")
    cat("Удалено всего:", total_stats$data_counts$total_removed, 
        sprintf("(%.1f%%)\n", total_stats$percentages$total_removed_percent))
    
    if (nrow(stats_summary) > 0) {
      cat("\nПримеры результатов (первые 3):\n")
      print(head(stats_summary, 3))
    }
  }
  
  # 14. ВОЗВРАТ РЕЗУЛЬТАТА
  result <- list(
    clean_data = clean_data_combined,
    outliers = if (!is.null(outliers_combined) && nrow(outliers_combined) > 0) outliers_combined else NULL,
    stats_summary = stats_summary,
    stats = total_stats
  )
  
  return(result)
}



