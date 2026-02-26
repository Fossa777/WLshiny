# app/grouping_module.R
# Функции для группировки видов

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
    
    # Диапазоны для экспорта и БД
    min_length = min(sp_data$length, na.rm = TRUE),
    max_length = max(sp_data$length, na.rm = TRUE),
    min_weight = min(sp_data$weight, na.rm = TRUE),
    max_weight = max(sp_data$weight, na.rm = TRUE),
    
    # Коэффициенты
    a = a,
    b = b,
    log_a = log(a)
  )
  
  cat("Успешно:", sp_name, 
      "a =", round(a, 4), 
      "b =", round(b, 3),
      "L =", round(min(sp_data$length),1), "-", round(max(sp_data$length),1),
      "\n")
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

curve_distance_combined <- function(a1, b1, a2, b2, Lmax, points_per_cm = 10, Lmin = 1) {
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
      
      Lmax <- min(tbl$maxlength[i], tbl$maxlength[j])
      
      res <- curve_distance_combined(
        tbl$a[i], tbl$b[i],
        tbl$a[j], tbl$b[j],
        Lmax = Lmax,
        points_per_cm = points_per_cm
      )
      
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
      
      if (is.finite(res$rms) && is.finite(res$growth_ratio) &&
          res$rms <= max_diff && res$growth_ratio <= max_growth) {
        adj[i, j] <- TRUE
        adj[j, i] <- TRUE
      }
      
      idx <- idx + 1
    }
  }
  
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  tbl$group <- igraph::components(g)$membership
  
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

calculate_mean_curve<- function(tbl, n_points = 300, Lmin = 1, power = 3) {
  if (nrow(tbl) < 2) return(NULL)
  Lmax <- max(tbl$maxlength, na.rm = TRUE)
  L <- seq(Lmin, Lmax, length.out = n_points)
  w_species <- (tbl$maxlength / max(tbl$maxlength))^power
  w_species <- w_species / sum(w_species)
  Wmat <- sapply(seq_len(nrow(tbl)), function(i) tbl$a[i]*L^tbl$b[i])
  Wmean <- Wmat %*% w_species
  data.frame(length = L, weight = as.numeric(Wmean), type = "mean_curve")
}

# Функция для инициализации
source_grouping_functions <- function() {
  # Функции уже в глобальной среде
  cat("Функции группировки загружены\n")
}