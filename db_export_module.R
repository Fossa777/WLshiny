build_big_db_table_test <- function(
  cleaned_data,
  grouped_models   # это grouping_state$grouped$table
) {
  
  # Диапазоны и число промеров по видам
  species_ranges <- cleaned_data %>%
    filter(
      was_cleaned == TRUE,
      is.finite(length),
      is.finite(weight),
      length > 0,
      weight > 0
    ) %>%
    group_by(species) %>%
    summarise(
      min_L = min(length, na.rm = TRUE),
      max_L = max(length, na.rm = TRUE),
      min_W = min(weight, na.rm = TRUE),
      max_W = max(weight, na.rm = TRUE),
      n_measurements_species = n(),
      .groups = "drop"
    )
  
  # Число промеров и видов по группам
  group_measurements <- grouped_models %>%
    left_join(
      species_ranges %>% select(species, n_measurements_species),
      by = "species"
    ) %>%
    group_by(group) %>%
    summarise(
      n_species_in_group = n(),
      n_measurements_group = sum(n_measurements_species, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Групповые коэффициенты
  group_coefs <- grouped_models %>%
    group_by(group) %>%
    summarise(
      a_group = mean(a, na.rm = TRUE),
      b_group = mean(b, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Сборка большой таблицы
  big_db_table <- grouped_models %>%
    left_join(species_ranges, by = "species") %>%
    left_join(group_coefs,     by = "group") %>%
    left_join(group_measurements, by = "group") %>%
    transmute(
      group_id = group,
      
      species_latin = species,
      species_ru    = secies_name_ru,
      
      Family = NA_character_,
      Salt   = NA_character_,
      
      a_group = a_group,
      b_group = b_group,
      
      a_species = a,
      b_species = b,
      
      n_species_in_group = n_species_in_group,
      n_measurements_species = n_measurements_species,
      n_measurements_group  = n_measurements_group,
      
      min_L = min_L,
      max_L = max_L,
      min_W = min_W,
      max_W = max_W
    )
  
  return(big_db_table)
}


build_big_db_table <- function(
  cleaned_data,
  grouped_models   # это grouping_state$grouped$table
) {
  
  # Диапазоны и число промеров по видам
  species_ranges <- cleaned_data %>%
    filter(
      was_cleaned == TRUE,
      is.finite(length),
      is.finite(weight),
      length > 0,
      weight > 0
    ) %>%
    group_by(species) %>%
    summarise(
      min_L = min(length, na.rm = TRUE),
      max_L = max(length, na.rm = TRUE),
      min_W = min(weight, na.rm = TRUE),
      max_W = max(weight, na.rm = TRUE),
      n_measurements_species = n(),
      .groups = "drop"
    )
  
  # Число промеров и видов по группам
  group_measurements <- grouped_models %>%
    left_join(
      species_ranges %>% select(species, n_measurements_species),
      by = "species"
    ) %>%
    group_by(group) %>%
    summarise(
      n_species_in_group = n(),
      n_measurements_group = sum(n_measurements_species, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Функция для расчета групповых коэффициентов через взвешенную среднюю кривую
  calculate_group_coefficients <- function(tbl, power = 3, n_points = 300) {
    if (nrow(tbl) < 2) return(list(a_group = NA_real_, b_group = NA_real_))
    
    Lmin <- 1
    Lmax <- max(tbl$maxlength, na.rm = TRUE)
    L <- seq(Lmin, Lmax, length.out = n_points)
    
    # Веса видов: чем больше maxlength, тем больше вес
    w_species <- (tbl$maxlength / max(tbl$maxlength))^power
    w_species <- w_species / sum(w_species)
    
    # Взвешенная кривая
    Wmat <- sapply(seq_len(nrow(tbl)), function(i) tbl$a[i] * L^tbl$b[i])
    Wmean <- Wmat %*% w_species
    
    # Подбираем коэффициенты a и b к средней кривой
    mean_data <- data.frame(L = L, W = as.numeric(Wmean))
    
    # Фиттинг степенной модели W = a * L^b
    tryCatch({
      # Линеаризация: log(W) = log(a) + b * log(L)
      mean_data$logL <- log(mean_data$L)
      mean_data$logW <- log(mean_data$W)
      
      # Исключаем бесконечные значения
      mean_data <- mean_data[is.finite(mean_data$logL) & is.finite(mean_data$logW), ]
      
      if(nrow(mean_data) < 2) {
        return(list(a_group = mean(tbl$a, na.rm = TRUE), 
                    b_group = mean(tbl$b, na.rm = TRUE)))
      }
      
      fit <- lm(logW ~ logL, data = mean_data)
      
      a_group <- exp(coef(fit)[1])  # exp(intercept)
      b_group <- coef(fit)[2]       # slope
      
      return(list(a_group = a_group, b_group = b_group))
      
    }, error = function(e) {
      # Если не удалось подобрать, используем среднее взвешенное
      return(list(
        a_group = weighted.mean(tbl$a, w = w_species, na.rm = TRUE),
        b_group = weighted.mean(tbl$b, w = w_species, na.rm = TRUE)
      ))
    })
  }
  
  # Применяем для каждой группы
  group_coefs_list <- lapply(unique(grouped_models$group), function(g) {
    tbl_grp <- grouped_models %>% filter(group == g)
    coefs <- calculate_group_coefficients(tbl_grp, power = 3, n_points = 300)
    data.frame(group = g, a_group = coefs$a_group, b_group = coefs$b_group)
  })
  
  group_coefs <- bind_rows(group_coefs_list)
  
  # Функция для получения Family и Salt из cleaned_data
  get_taxonomy_info <- function(species_name, cleaned_data) {
    info <- cleaned_data %>%
      filter(species == species_name) %>%
      slice(1) %>%  # берем первую запись
      select(Family, Salt)
    
    if(nrow(info) > 0) {
      return(list(Family = as.character(info$Family[1]), 
                  Salt = as.character(info$Salt[1])))
    } else {
      return(list(Family = NA_character_, Salt = NA_character_))
    }
  }
  
  # Сборка большой таблицы
  big_db_table <- grouped_models %>%
    left_join(species_ranges, by = "species") %>%
    left_join(group_coefs, by = "group") %>%
    left_join(group_measurements, by = "group") %>%
    rowwise() %>%
    mutate(
      # Получаем Family и Salt из cleaned_data
      tax_info = list(get_taxonomy_info(species, cleaned_data)),
      Family = tax_info$Family,
      Salt = tax_info$Salt
    ) %>%
    ungroup() %>%
    select(-tax_info) %>%
    transmute(
      group_id = group,
      
      species_latin = species,
      species_ru    = secies_name_ru,
      
      Family = Family,
      Salt   = Salt,
      
      # Групповые коэффициенты (взвешенная средняя)
      a_group = round(a_group, 6),
      b_group = round(b_group, 3),
      
      # Индивидуальные коэффициенты вида
      a_species = round(a, 6),
      b_species = round(b, 3),
      
      n_species_in_group = n_species_in_group,
      n_measurements_species = n_measurements_species,
      n_measurements_group  = n_measurements_group,
      
      min_L = round(min_L, 1),
      max_L = round(max_L, 1),
      min_W = round(min_W, 1),
      max_W = round(max_W, 1)
    ) %>%
    arrange(group_id, species_latin)
  
  # Рассчитываем R² для групповых формул
  cat("\n=== РАСЧЕТ R² ДЛЯ ГРУПП ===\n")
  
  big_db_table_with_r2 <- big_db_table
  
  for(g in unique(big_db_table$group_id)) {
    group_species <- big_db_table %>% filter(group_id == g)
    
    if(nrow(group_species) > 1) {
      # Получаем групповые коэффициенты
      a_group <- unique(group_species$a_group)[1]
      b_group <- unique(group_species$b_group)[1]
      
      cat(sprintf("\nГруппа %d: a=%.6f, b=%.3f\n", g, a_group, b_group))
      cat("Виды в группе:", paste(group_species$species_ru, collapse = ", "), "\n")
      
      # Рассчитываем R² между групповой формулой и каждой индивидуальной
      r2_values <- numeric()
      
      for(i in 1:nrow(group_species)) {
        # Генерируем кривые на общем диапазоне длин
        Lmin <- max(group_species$min_L)
        Lmax <- min(group_species$max_L)
        
        if(Lmax > Lmin) {
          L <- seq(Lmin, Lmax, length.out = 100)
          
          # Предсказания групповой и индивидуальной моделей
          W_group <- a_group * L^b_group
          W_species <- group_species$a_species[i] * L^group_species$b_species[i]
          
          # R² = 1 - SSE/SST
          SSE <- sum((W_species - W_group)^2)
          SST <- sum((W_species - mean(W_species))^2)
          
          r2 <- ifelse(SST > 0, 1 - SSE/SST, 1)
          r2_values <- c(r2_values, r2)
          
          cat(sprintf("  %s: R² = %.3f\n", 
                      group_species$species_ru[i], r2))
        }
      }
      
      # Средний R² по группе
      mean_r2 <- mean(r2_values, na.rm = TRUE)
      cat(sprintf("Средний R² группы: %.3f\n", mean_r2))
    }
  }
  
  return(big_db_table)
}