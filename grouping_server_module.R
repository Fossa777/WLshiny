# app/grouping_server_module.R
# Серверная логика для группировки

grouping_server <- function(input, output, session, cleaned_data) {
  
  ns <- session$ns
  
  # Реактивные значения
  grouping_state <- reactiveValues(
    models = NULL,
    grouped = NULL,
    current_group = 1
  )
  
  # Запуск группировки
  observeEvent(input$run_grouping, {
    req(cleaned_data())
    
    showModal(modalDialog(
      title = "Запуск группировки...",
      "Пожалуйста, подождите. Это может занять несколько секунд.",
      footer = NULL,
      size = "l"
    ))
    
    tryCatch({
      data_for_grouping <- cleaned_data()$clean_data
      
      if(is.null(data_for_grouping)) {
        showNotification("Нет очищенных данных для группировки", 
                         type = "warning", duration = 5)
        return()
      }
      
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
      
      showNotification("✅ Группировка завершена", 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("❌ Ошибка при группировке:", e$message), 
                       type = "error", duration = 10)
    }, finally = {
      removeModal()
    })
  })
  
  # Навигация по группам
  observeEvent(input$group_next, {
    if(!is.null(grouping_state$grouped)) {
      max_group <- max(grouping_state$grouped$table$group)
      new_group <- grouping_state$current_group + 1
      if(new_group > max_group) new_group <- 1
      grouping_state$current_group <- new_group
    }
  })
  
  observeEvent(input$group_prev, {
    if(!is.null(grouping_state$grouped)) {
      max_group <- max(grouping_state$grouped$table$group)
      new_group <- grouping_state$current_group - 1
      if(new_group < 1) new_group <- max_group
      grouping_state$current_group <- new_group
    }
  })
  
  # Информация о группировке
  output$grouping_info <- renderPrint({
    if(is.null(grouping_state$grouped)) {
      cat("Группировка не выполнена.\n")
      cat("Нажмите 'Запустить группировку' для начала.\n")
      return()
    }
    
    tbl <- grouping_state$grouped$table
    current <- grouping_state$current_group
    
    cat(sprintf("Группа %d из %d\n", current, max(tbl$group)))
    
    group_data <- tbl %>% filter(group == current)
    cat(sprintf("Видов в группе: %d\n", nrow(group_data)))
    cat(sprintf("Наблюдений: %d\n", sum(group_data$n)))
  })
  
  # График текущей группы
  output$grouping_plot <- renderPlot({
    req(grouping_state$grouped, grouping_state$current_group)
    
    tbl_grp <- grouping_state$grouped$table %>% 
      filter(group == grouping_state$current_group)
    
    if(nrow(tbl_grp) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Нет данных для группы", size = 6) +
               theme_void())
    }
    
    # Создаем линии для каждого вида
    lines_data <- purrr::map_dfr(1:nrow(tbl_grp), function(i) {
      L <- seq(1, tbl_grp$maxlength[i], length.out = 100)
      tibble(
        species = tbl_grp$secies_name_ru[i],
        length = L,
        weight = tbl_grp$a[i] * L^tbl_grp$b[i]
      )
    })
    
    ggplot(lines_data, aes(x = length, y = weight, color = species)) +
      geom_line(linewidth = 1.2) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Группа", grouping_state$current_group),
        x = "Длина, см",
        y = "Вес, г",
        color = "Вид"
      ) +
      theme(legend.position = "bottom")
  })
  
  # Таблица текущей группы
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
        Вид = secies_name_ru,
        Наблюдения = n,
        `maxlength` = maxlength,
        a = a,
        b = b
      )
    
    datatable(tbl_grp, rownames = FALSE, options = list(pageLength = 10))
  })
  
  # Статистика группировки
  output$grouping_stats <- renderPrint({
    req(grouping_state$grouped)
    
    tbl <- grouping_state$grouped$table
    
    total_species <- nrow(tbl)
    total_groups <- length(unique(tbl$group))
    
    singletons <- tbl %>%
      group_by(group) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      nrow()
    
    clusters <- total_groups - singletons
    
    cat("=== СТАТИСТИКА ГРУППИРОВКИ ===\n\n")
    cat(sprintf("Всего видов: %d\n", total_species))
    cat(sprintf("Всего групп: %d\n", total_groups))
    cat(sprintf("• Групп с >1 вида: %d\n", clusters))
    cat(sprintf("• Одиночных видов: %d (%.1f%%)\n", 
                singletons, singletons/total_species*100))
    
    if(clusters > 0) {
      cluster_sizes <- tbl %>%
        group_by(group) %>%
        summarise(size = n()) %>%
        filter(size > 1)
      
      cat(sprintf("\nСредний размер группы: %.1f\n", mean(cluster_sizes$size)))
      cat(sprintf("Максимальный размер группы: %d\n", max(cluster_sizes$size)))
      cat(sprintf("Минимальный размер группы: %d\n", min(cluster_sizes$size)))
    }
  })
  
  # График размеров групп
  output$group_sizes_plot <- renderPlot({
    req(grouping_state$grouped)
    
    group_stats <- grouping_state$grouped$table %>%
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
  })
  
  # Экспорт результатов
  observeEvent(input$export_groups, {
    req(grouping_state$grouped)
    
    tryCatch({
      export_data <- grouping_state$grouped$table %>%
        select(
          group_id = group,
          species_latin = species,
          species_ru = secies_name_ru,
          n_observations = n,
          maxlength,
          a, b,
          min_length, max_length
        ) %>%
        arrange(group_id, species_ru)
      
      filename <- paste0("species_groups_", Sys.Date(), ".xlsx")
      openxlsx::write.xlsx(export_data, filename)
      
      showNotification(paste("✅ Файл сохранен:", filename), 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("❌ Ошибка при экспорте:", e$message), 
                       type = "error", duration = 10)
    })
  })
  
  # Предпросмотр данных
  output$groups_preview <- renderDT({
    req(grouping_state$grouped)
    
    preview_data <- grouping_state$grouped$table %>%
      select(
        Группа = group,
        Вид = secies_name_ru,
        Наблюдения = n,
        `maxlength` = maxlength,
        a = a,
        b = b
      ) %>%
      head(50)
    
    datatable(preview_data, rownames = FALSE, options = list(pageLength = 10))
  })
  
  # Возвращаем состояние для использования в основном приложении
  return(reactive({
    list(
      models = grouping_state$models,
      grouped = grouping_state$grouped,
      current_group = grouping_state$current_group
    )
  }))
}