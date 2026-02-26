# plotting_module.R

# Основная функция для построения графика регрессии
create_regression_plot <- function(sp_data, pred_data, outliers = NULL, 
                                   highlight_outliers = FALSE,
                                   plot_settings) {
  
  if(is.null(sp_data) || nrow(sp_data) == 0) {
    plot(0, 0, type = "n", xlab = "", ylab = "", 
         main = "Недостаточно данных")
    return(ggplot())
  }
  
  # Получаем максимальный размер для этого вида
  max_size <- ifelse(!is.null(sp_data$maxlength[1]), 
                     round(sp_data$maxlength[1], 0),
                     NA)
  
  p <- ggplot()
  
  # Добавляем доверительную полосу если нужно
  if(plot_settings$show_ribbon && !is.null(pred_data)) {
    
    # Выбираем какую полосу показывать
    if(plot_settings$ribbon_model %in% c("Степенная", "Обе")) {
      p <- p + geom_ribbon(data = pred_data,
                           aes(x = length, 
                               ymin = Power_lower, 
                               ymax = Power_upper),
                           fill = "red", 
                           alpha = plot_settings$ribbon_alpha)
    }
    
    if(plot_settings$ribbon_model %in% c("Экспоненциальная", "Обе")) {
      p <- p + geom_ribbon(data = pred_data,
                           aes(x = length, 
                               ymin = Exp_lower, 
                               ymax = Exp_upper),
                           fill = "darkgreen", 
                           alpha = plot_settings$ribbon_alpha)
    }
  }
  
  # Добавляем точки
  if(plot_settings$show_points) {
    if(!is.null(outliers) && highlight_outliers) {
      # Подсвечиваем выбросы
      p <- p + 
        geom_point(data = outliers %>% filter(!is_outlier),
                   aes(x = length, y = weight),
                   size = plot_settings$point_size,
                   alpha = plot_settings$alpha,
                   color = "steelblue") +
        geom_point(data = outliers %>% filter(is_outlier),
                   aes(x = length, y = weight),
                   size = plot_settings$point_size + 2,
                   alpha = 1,
                   color = "red",
                   shape = 17)  # Треугольники для выбросов
    } else {
      # Обычные точки
      p <- p + geom_point(data = sp_data, 
                          aes(x = length, y = weight),
                          size = plot_settings$point_size,
                          alpha = plot_settings$alpha,
                          color = "steelblue")
    }
  }
  
  # Добавляем линии моделей
  if(plot_settings$show_power && !is.null(pred_data)) {
    p <- p + geom_line(data = pred_data,
                       aes(x = length, y = Power, color = "Степенная"),
                       size = plot_settings$line_size,
                       linetype = "solid")
  }
  
  if(plot_settings$show_exp && !is.null(pred_data)) {
    p <- p + geom_line(data = pred_data,
                       aes(x = length, y = Exp, color = "Экспоненциальная"),
                       size = plot_settings$line_size,
                       linetype = "dashed")
  }
  
  # Настройка осей
  if(plot_settings$log_x) {
    p <- p + scale_x_log10()
    x_min <- 0.1
  } else {
    x_min <- 0
  }
  
  if(plot_settings$log_y) {
    p <- p + scale_y_log10()
    y_min <- 0.1
  } else {
    y_min <- 0
  }
  
  # Основные настройки графика
  subtitle_text <- paste(
    "n =", nrow(sp_data), "наблюдений |",
    "Длина в выборке:", round(min(sp_data$length), 1), "-", 
    round(max(sp_data$length), 1), "см"
  )
  
  if(!is.na(max_size)) {
    subtitle_text <- paste(subtitle_text, "| Макс. размер вида:", max_size, "см")
  }
  
  if(plot_settings$show_ribbon) {
    subtitle_text <- paste(subtitle_text, "| Ribbon: ±", plot_settings$ribbon_percent, "%")
  }
  
  p <- p +
    coord_cartesian(xlim = c(x_min, max(sp_data$length) * 1.05),
                    ylim = c(y_min, max(sp_data$weight) * 1.05)) +
    labs(
      title = paste("Вид:", unique(sp_data$species)[1]),
      subtitle = subtitle_text,
      x = "Длина (см)", 
      y = "Вес (г)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "gray50"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
  
  # Добавляем легенду для линий
  if((plot_settings$show_power || plot_settings$show_exp) && !is.null(pred_data)) {
    p <- p + scale_color_manual(
      name = "Модель",
      values = c("Степенная" = "red", 
                 "Экспоненциальная" = "darkgreen"),
      breaks = c("Степенная", "Экспоненциальная")
    )
  }
  
  # Добавляем аннотацию для ribbon если она показана
  if(plot_settings$show_ribbon) {
    p <- p + annotate("text",
                      x = max(sp_data$length) * 0.7,
                      y = max(sp_data$weight) * 0.9,
                      label = paste("Доверительная полоса: ±", 
                                    plot_settings$ribbon_percent, "%"),
                      size = 4,
                      color = "gray40",
                      hjust = 0)
  }
  
  return(p)
}

# Создание легенды в HTML формате
create_legend_html <- function(plot_settings, highlight_outliers, ribbon_percent) {
  tags$div(
    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
    tags$h5("Обозначения:"),
    tags$ul(
      tags$li(tags$span(style = "color: steelblue; font-weight: bold;", "●"), 
              " - Наблюдаемые данные"),
      if(highlight_outliers) tags$li(tags$span(style = "color: red; font-weight: bold;", "▲"), 
                                    " - Выбросы (отклонение >", ribbon_percent, "%)"),
      if(plot_settings$show_power) tags$li(tags$span(style = "color: red; font-weight: bold;", "━━"), 
                                           " - Степенная модель (W = axL^b)"),
      if(plot_settings$show_exp) tags$li(tags$span(style = "color: darkgreen; font-weight: bold;", "⸺"), 
                                         " - Экспоненциальная модель (W = a × e^(b×L))"),
      if(plot_settings$show_ribbon && plot_settings$ribbon_model %in% c("Степенная", "Обе")) 
        tags$li(tags$span(style = "background-color: rgba(255,0,0,0.3); padding: 2px 10px;", " "), 
                " - Доверительная полоса степенной модели (±", ribbon_percent, "%)"),
      if(plot_settings$show_ribbon && plot_settings$ribbon_model %in% c("Экспоненциальная", "Обе")) 
        tags$li(tags$span(style = "background-color: rgba(0,100,0,0.3); padding: 2px 10px;", " "), 
                " - Доверительная полоса экспоненциальной модели (±", ribbon_percent, "%)")
    )
  )
}

# Создание HTML для информации о выбросах
gap_visualization_plot <- function(Lmin = 3, gap_threshold = 45, maxlength = 30) {
  
  # Рассчитываем диапазон
  total_range <- maxlength - Lmin
  gap_size <- total_range * (gap_threshold / 100)  # Размер зоны в см
  
  # Определяем где разместить зону разрыва
  # Если зона большая (>40%) - показываем от начала
  # Если зона маленькая (<40%) - показываем в середине
  if (gap_threshold > 40) {
    # Большая зона - от Lmin
    gap_start <- Lmin
    gap_end <- Lmin + gap_size
  } else {
    # Маленькая зона - в середине
    mid_point <- Lmin + total_range / 2
    gap_start <- mid_point - gap_size / 2
    gap_end <- mid_point + gap_size / 2
  }
  
  # Убедимся, что зона в пределах диапазона
  gap_start <- max(Lmin, gap_start)
  gap_end <- min(maxlength, gap_end)
  
  # Создаем плавную линию регрессии
  a <- 0.01
  b <- 3.0
  L_smooth <- seq(Lmin, maxlength, length.out = 100)
  W_smooth <- a * (L_smooth^b)
  
  # Создаем данные для графика
  df_line <- data.frame(
    length = L_smooth,
    weight = W_smooth
  )
  
  # Цвет зоны в зависимости от размера
  zone_color <- ifelse(gap_threshold > 50, "#ff9999", 
                      ifelse(gap_threshold > 30, "#ffcc99", "#ccffcc"))
  
  # Создаем график
  p <- ggplot() +
    # Фоновая линия регрессии (светлая)
    geom_line(data = df_line, aes(x = length, y = weight), 
              color = "gray80", size = 2.5, alpha = 0.3) +
    # Основная линия регрессии
    geom_line(data = df_line, aes(x = length, y = weight), 
              color = "#3498db", size = 1.2) +
    # ЗОНА РАЗРЫВА (самое главное!)
    annotate("rect", 
             xmin = gap_start, xmax = gap_end,
             ymin = 0, ymax = max(W_smooth) * 1.1,
             fill = zone_color, alpha = 0.5,
             color = "black", size = 0.8) +
    # Подпись зоны разрыва ВНУТРИ зоны
    annotate("text",
             x = (gap_start + gap_end) / 2,
             y = max(W_smooth) * 0.6,
             label = sprintf("Разрыв: %.1f%%\n(%.1f см)", 
                           gap_threshold, gap_size),
             color = "black",
             size = 5,
             fontface = "bold",
             hjust = 0.5,
             vjust = 0.5) +
    # Стрелки показывающие границы зоны
    geom_segment(
      aes(x = gap_start, xend = gap_start,
          y = max(W_smooth) * 0.2, yend = max(W_smooth) * 0.4),
      color = "black",
      size = 1,
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) +
    geom_segment(
      aes(x = gap_end, xend = gap_end,
          y = max(W_smooth) * 0.2, yend = max(W_smooth) * 0.4),
      color = "black",
      size = 1,
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) +
    # Подписи границ зоны
    annotate("text",
             x = gap_start,
             y = max(W_smooth) * 0.15,
             label = sprintf("%.1f см", gap_start),
             color = "black",
             size = 4,
             fontface = "bold") +
    annotate("text",
             x = gap_end,
             y = max(W_smooth) * 0.15,
             label = sprintf("%.1f см", gap_end),
             color = "black",
             size = 4,
             fontface = "bold") +
    # Lmin и maxlength подписи (мелкие)
    annotate("text",
             x = Lmin,
             y = max(W_smooth) * 0.05,
             label = paste("Lmin =", Lmin),
             color = "gray50",
             size = 3.5,
             hjust = ifelse(Lmin < maxlength/2, 1.2, -0.2)) +
    annotate("text",
             x = maxlength,
             y = max(W_smooth) * 0.05,
             label = paste("maxlength =", maxlength),
             color = "gray50",
             size = 3.5,
             hjust = ifelse(maxlength > Lmin + total_range/2, -0.1, 1.1)) +
    # Вертикальные линии Lmin и maxlength (тонкие)
    geom_vline(xintercept = Lmin, 
               color = "gray70", 
               size = 0.5,
               linetype = "dashed") +
    geom_vline(xintercept = maxlength, 
               color = "gray70", 
               size = 0.5,
               linetype = "dashed") +
    # Заголовки
    labs(
      title = sprintf("Ширина допустимого разрыва между данными: %.1f%% от диапазона", gap_threshold),
      subtitle = sprintf("Общий диапазон: %.1f см (от %.1f до %.1f см)", 
                        total_range, Lmin, maxlength),
      x = "Длина, см",
      y = "Вес, г"
    ) +
    # Чистая тема
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(color = "#666", size = 12, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 11),
      panel.grid.major.y = element_line(color = "gray92"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    # Масштаб
    ylim(0, max(W_smooth) * 1.15)
  
  return(p)
}