compare_before_after_server <- function(input, output, session, raw_data, clean_data, model_type) {
  
  # Функция для получения очищенных данных текущего вида с проверкой
  cleaned_current_species <- reactive({
    req(clean_data())
    req(input$species)
    
    result <- clean_data()
    
    if(is.null(result$clean_data)) {
      message("Нет clean_data в результате очистки")
      return(NULL)
    }
    
    cl_data <- result$clean_data %>%
      filter(species == input$species, was_cleaned == TRUE)
    
    message(paste("Очищенных данных для вида", input$species, ":", nrow(cl_data)))
    
    if(nrow(cl_data) == 0) {
      message("Нет очищенных данных для этого вида")
      return(NULL)
    }
    
    # Проверяем наличие числовых данных
    cl_data_clean <- cl_data %>%
      filter(!is.na(length), !is.na(weight),
             length > 0, weight > 0)
    
    message(paste("После фильтрации NA/нулей:", nrow(cl_data_clean)))
    
    return(cl_data_clean)
  })
  
  
  outliers_current_species <- reactive({
    req(clean_data())
    req(input$species)
    
    res <- clean_data()
    
    if (is.null(res$outliers)) return(NULL)
    
    res$outliers %>%
      dplyr::filter(species == input$species)
  })
  
  
  # Функция для построения модели на очищенных данных
  clean_model <- reactive({
    cl_data <- cleaned_current_species()
    
    if(is.null(cl_data) || nrow(cl_data) < 3) {
      message("Мало данных для модели очищенных (<3)")
      return(NULL)
    }
    
    message(paste("Построение модели на", nrow(cl_data), "очищенных точках"))
    print(head(cl_data[, c("length", "weight")]))
    
    tryCatch({
      if(model_type() == "power") {
        # Проверяем, что все значения положительные
        if(any(cl_data$length <= 0)) {
          message("Есть неположительные длины в очищенных данных")
          return(NULL)
        }
        if(any(cl_data$weight <= 0)) {
          message("Есть неположительные веса в очищенных данных")
          return(NULL)
        }
        
        cl_data$log_length <- log(cl_data$length)
        cl_data$log_weight <- log(cl_data$weight)
        
        model <- lm(log_weight ~ log_length, data = cl_data)
        message("Степенная модель построена успешно")
      } else {
        model <- lm(log(weight) ~ length, data = cl_data)
        message("Экспоненциальная модель построена успешно")
      }
      return(model)
    }, error = function(e) {
      message("Ошибка при построении модели очищенных данных: ", e$message)
      return(NULL)
    })
  })
  
  # Функция для построения модели на исходных данных
  raw_model <- reactive({
    rw_data <- raw_data()
    
    if(is.null(rw_data) || nrow(rw_data) < 3) {
      message("Мало исходных данных (<3)")
      return(NULL)
    }
    
    # Очищаем исходные данные
    rw_data_clean <- rw_data %>%
      filter(!is.na(length), !is.na(weight),
             length > 0, weight > 0)
    
    if(nrow(rw_data_clean) < 3) {
      message("После очистки мало исходных данных (<3)")
      return(NULL)
    }
    
    message(paste("Построение модели на", nrow(rw_data_clean), "исходных точках"))
    
    tryCatch({
      if(model_type() == "power") {
        rw_data_clean$log_length <- log(rw_data_clean$length)
        rw_data_clean$log_weight <- log(rw_data_clean$weight)
        model <- lm(log_weight ~ log_length, data = rw_data_clean)
      } else {
        model <- lm(log(weight) ~ length, data = rw_data_clean)
      }
      return(model)
    }, error = function(e) {
      message("Ошибка при построении модели исходных данных: ", e$message)
      return(NULL)
    })
  })
  
  # Предсказания и ribbon
  compare_predictions <- reactive({
    rw_data <- raw_data()
    cl_model <- clean_model()
    raw_mod <- raw_model()
    
    # Отладка
    message("=== Начало compare_predictions ===")
    message(paste("Raw data:", !is.null(rw_data), "rows:", ifelse(!is.null(rw_data), nrow(rw_data), 0)))
    message(paste("Clean model:", !is.null(cl_model)))
    message(paste("Raw model:", !is.null(raw_mod)))
    
    if(is.null(rw_data) || nrow(rw_data) < 3) {
      message("Недостаточно исходных данных")
      return(NULL)
    }
    
    if(is.null(cl_model) || is.null(raw_mod)) {
      message("Одна из моделей не построена")
      return(NULL)
    }
    
    # Очищаем данные для диапазона
    rw_clean <- rw_data %>%
      filter(!is.na(length), !is.na(weight),
             length > 0, weight > 0)
    
    if(nrow(rw_clean) < 3) {
      message("После очистки мало данных для диапазона")
      return(NULL)
    }
    
    # Создаем диапазон для предсказаний
    min_len <- max(0.1, min(rw_clean$length))  # Защита от нуля
    max_len <- max(rw_clean$length)
    x_range <- seq(min_len, max_len, length.out = 100)
    
    message(paste("Диапазон длины:", min_len, "-", max_len))
    
    ribbon_percent <- input$compare_ribbon_percent / 100
    
    tryCatch({
      if(model_type() == "power") {
        # Для степенной модели
        # Проверяем, что x_range не содержит неположительных значений
        if(any(x_range <= 0)) {
          message("Есть неположительные значения в x_range")
          x_range <- pmax(x_range, 0.1)  # Заменяем на маленькое положительное
        }
        
        log_x_range <- log(x_range)
        
        # Предсказания для очищенных данных
        clean_pred <- exp(predict(cl_model, 
                                  newdata = data.frame(log_length = log_x_range)))
        
        # Предсказания для исходных данных
        raw_pred <- exp(predict(raw_mod, 
                                newdata = data.frame(log_length = log_x_range)))
        
        message("Степенная модель: предсказания выполнены")
      } else {
        # Для экспоненциальной модели
        clean_pred <- exp(predict(cl_model, 
                                  newdata = data.frame(length = x_range)))
        raw_pred <- exp(predict(raw_mod, 
                                newdata = data.frame(length = x_range)))
        
        message("Экспоненциальная модель: предсказания выполнены")
      }
      
      # Проверяем результаты
      if(any(is.na(clean_pred)) || any(is.na(raw_pred))) {
        message("Есть NA в предсказаниях")
        print(head(clean_pred))
        print(head(raw_pred))
      }
      
      result <- data.frame(
        length = x_range,
        before = raw_pred,
        before_lo = raw_pred * (1 - ribbon_percent),
        before_hi = raw_pred * (1 + ribbon_percent),
        after = clean_pred,
        after_lo = clean_pred * (1 - ribbon_percent),
        after_hi = clean_pred * (1 + ribbon_percent)
      )
      
      message("Предсказания успешно созданы")
      return(result)
      
    }, error = function(e) {
      message("Ошибка при предсказаниях: ", e$message)
      message("Трассировка ошибки:")
      print(traceback())
      return(NULL)
    })
  })
  
  output$compare_plot <- renderPlot({
    raw_sp <- raw_data()
    clean_sp <- cleaned_current_species()
    pred <- compare_predictions()
    out_sp <- outliers_current_species()
    
    message("=== Начало отрисовки графика ===")
    message(paste("Raw data points:", ifelse(!is.null(raw_sp), nrow(raw_sp), 0)))
    message(paste("Clean data points:", ifelse(!is.null(clean_sp), nrow(clean_sp), 0)))
    message(paste("Predictions:", !is.null(pred)))
    
    if(is.null(raw_sp) || nrow(raw_sp) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Нет данных для отображения", size = 6) +
               labs(title = "Сравнение до и после очистки") +
               theme_void())
    }
    
    if(is.null(pred)) {
  return(
    ggplot() + 
      annotate(
        "text", x = 0.5, y = 0.5, 
        label = "Не удалось построить модели сравнения\nПроверьте, что очистка выполнена....\nИли данный вид не прошел очистку.", 
        size = 6
      ) +
      labs(
        title = paste("Сравнение до и после очистки —", input$species)
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      )
  )
}
    
    # Очищаем исходные данные для графика
    raw_sp_clean <- raw_sp %>%
      filter(!is.na(length), !is.na(weight),
             length > 0, weight > 0)
    
    # Определяем пределы осей
    max_length <- max(c(raw_sp_clean$length, 
                        if(!is.null(clean_sp) && nrow(clean_sp) > 0) clean_sp$length else 0),
                      na.rm = TRUE)
    
    max_weight <- max(c(raw_sp_clean$weight,
                        if(!is.null(clean_sp) && nrow(clean_sp) > 0) clean_sp$weight else 0,
                        pred$after_hi, pred$before_hi),
                      na.rm = TRUE)
    
    # Добавляем небольшой запас (5%)
    max_length <- max_length * 1.05
    max_weight <- max_weight * 1.05
    

    p <- ggplot() +
      
      # ===== RIBBON ДО =====
    geom_ribbon(
      data = pred,
      aes(x = length, ymin = before_lo, ymax = before_hi),
      fill = "gray60",
      alpha = 0.25
    ) +
      
      # ===== RIBBON ПОСЛЕ =====
    geom_ribbon(
      data = pred,
      aes(x = length, ymin = after_lo, ymax = after_hi),
      fill = "steelblue",
      alpha = 0.25
    ) +
      
      # ===== ТОЧКИ ДО =====
    geom_point(
      data = raw_sp_clean,
      aes(x = length, y = weight),
      color = "gray40",
      alpha = 0.5,
      size = 2
    ) +
      
      
      # ===== ВЫБРОСЫ =====
    (if (!is.null(out_sp) && nrow(out_sp) > 0) {
      geom_point(
        data = out_sp,
        aes(x = length, y = weight),
        color = "red3",
        shape = 4,
        size = 3,
        stroke = 1.2,
        alpha = 0.9
      )
    })+
      
      
      # ===== ТОЧКИ ПОСЛЕ =====
    (if(!is.null(clean_sp) && nrow(clean_sp) > 0) {
      geom_point(
        data = clean_sp,
        aes(x = length, y = weight),
        color = "steelblue",
        alpha = 0.8,
        size = 2.5
      )
    }) +
      
      # ===== ЛИНИИ =====
    geom_line(
      data = pred,
      aes(x = length, y = before),
      color = "gray20",
      linewidth = 1,
      linetype = "dashed"
    ) +
      geom_line(
        data = pred,
        aes(x = length, y = after),
        color = "steelblue4",
        linewidth = 1.2
      ) +
      
      # ВАЖНО: Ограничиваем оси, начиная с 0
      coord_cartesian(xlim = c(0, max_length),
                      ylim = c(0, max_weight),
                      expand = TRUE) +
      
      # Альтернатива: можно использовать scale_x_continuous и scale_y_continuous
      # scale_x_continuous(limits = c(0, max_length), expand = c(0, 0.05)) +
      # scale_y_continuous(limits = c(0, max_weight), expand = c(0, 0.05)) +
      
      labs(
        title = paste("Сравнение до и после очистки —", input$species),
        subtitle = paste(
          "Серый: исходные данные (n=", nrow(raw_sp_clean), 
          ") | Синий: после очистки (n=", 
          ifelse(!is.null(clean_sp), nrow(clean_sp), 0),
          ") | Ribbon ±", input$compare_ribbon_percent, "%"
        ),
        x = "Длина (см)",
        y = "Вес (г)"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        # Убираем отступы по умолчанию
        plot.margin = margin(10, 10, 10, 10)
      )
    
    return(p)
  })
}