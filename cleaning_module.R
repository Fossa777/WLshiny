# cleaning_module.R

# Функции для очистки данных

# Упрощенная функция для создания Excel файла
create_excel_file <- function(result, file) {
  tryCatch({
    # Создаем книгу Excel
    wb <- createWorkbook()
    
    # Лист 1: Очищенные данные
    addWorksheet(wb, "Очищенные данные")
    writeData(wb, "Очищенные данные", result$clean_data)
    
    # Лист 2: Статистика
    addWorksheet(wb, "Статистика")
    if(!is.null(result$stats$stats_summary)) {
      writeData(wb, "Статистика", result$stats$stats_summary)
    }
    
    # Лист 3: Выбросы (если есть)
    if(!is.null(result$outliers) && nrow(result$outliers) > 0) {
      addWorksheet(wb, "Выбросы")
      writeData(wb, "Выбросы", result$outliers)
    }
    
    # Сохраняем файл
    saveWorkbook(wb, file, overwrite = TRUE)
    
    return(TRUE)
  }, error = function(e) {
    print(paste("Ошибка при создании Excel:", e$message))
    return(FALSE)
  })
}


