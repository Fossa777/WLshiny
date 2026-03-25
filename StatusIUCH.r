library(readxl)
library(dplyr)
library(openxlsx)
library(purrr)
library(stringr)
library(rfishbase)
library(rredlist)

# 1. читаем файл
df <- read_excel("species_with_status.xlsx")

# 2. убираем старые заглушки
df <- df %>%
  mutate(
    status = ifelse(taxon_level == "genus", "NE", NA),
    status_ru = ifelse(taxon_level == "genus", "Не оценён", NA)
  )

# 3. определяем нормальные биноминалы
is_binomial <- function(x) {
  !is.na(x) &&
    str_detect(x, "^[A-Z][a-z]+ [a-z-]+$") &&
    !str_detect(x, "\\b(sp|cf|aff)\\.?\\b")
}

df <- df %>%
  mutate(is_species = map_lgl(clean_name, is_binomial))

# 4. валидируем названия через FishBase
species_list <- df %>%
  filter(is_species) %>%
  pull(clean_name) %>%
  unique()

valid_map <- tibble(
  clean_name = species_list,
  valid_name = validate_names(species_list)
)

df <- df %>%
  left_join(valid_map, by = "clean_name") %>%
  mutate(
    valid_name = ifelse(
      is_species & !is.na(valid_name) & valid_name != "",
      valid_name,
      ifelse(is_species, clean_name, NA)
    )
  )

# 5. функция запроса статуса IUCN
get_iucn_latest <- function(sp_name) {
  parts <- str_split(sp_name, "\\s+", simplify = TRUE)

  if (ncol(parts) < 2) {
    return(tibble(
      valid_name = sp_name,
      status_iucn = NA_character_,
      iucn_year = NA_integer_,
      population_trend_code = NA_character_
    ))
  }

  genus <- parts[1]
  species <- parts[2]

  out <- tryCatch(
    rl_species_latest(genus = genus, species = species),
    error = function(e) NULL
  )

  if (is.null(out)) {
    return(tibble(
      valid_name = sp_name,
      status_iucn = NA_character_,
      iucn_year = NA_integer_,
      population_trend_code = NA_character_
    ))
  }

  tibble(
    valid_name = sp_name,
    status_iucn = tryCatch(out$red_list_category$code, error = function(e) NA_character_),
    iucn_year = tryCatch(as.integer(out$year_published), error = function(e) NA_integer_),
    population_trend_code = tryCatch(out$population_trend$code, error = function(e) NA_character_)
  )
}

# 6. список валидных видов
valid_species <- df %>%
  filter(is_species, !is.na(valid_name)) %>%
  pull(valid_name) %>%
  unique()

# 7. массовый запрос
iucn_map <- map_dfr(valid_species, function(x) {
  message("Checking: ", x)
  Sys.sleep(0.4)
  get_iucn_latest(x)
})

# 8. присоединяем результаты
df <- df %>%
  left_join(iucn_map, by = "valid_name")

# 9. русские расшифровки
ru_map <- c(
  "LC" = "Наименьшие опасения",
  "NT" = "Близкий к уязвимому положению",
  "VU" = "Уязвимый",
  "EN" = "Под угрозой исчезновения",
  "CR" = "На грани исчезновения",
  "EW" = "Исчезнувший в дикой природе",
  "EX" = "Исчезнувший",
  "DD" = "Недостаточно данных",
  "NE" = "Не оценён"
)

trend_ru_map <- c(
  "1" = "Увеличивается",
  "2" = "Стабильная",
  "3" = "Снижается",
  "4" = "Неизвестно"
)

# 10. финальные колонки
df <- df %>%
  mutate(
    status = ifelse(!is.na(status_iucn), status_iucn, status),
    status_ru = ifelse(!is.na(status), unname(ru_map[status]), NA),
    iucn_population_trend_ru = ifelse(
      !is.na(population_trend_code),
      unname(trend_ru_map[population_trend_code]),
      NA
    ),
    report_ready = ifelse(
      taxon_level == "species" & !is.na(valid_name) & !is.na(status),
      "yes",
      "no"
    )
  )

# 11. сохраняем
wb <- createWorkbook()

addWorksheet(wb, "all_rows")
writeData(wb, "all_rows", df)

addWorksheet(wb, "report_ready_species")
writeData(
  wb,
  "report_ready_species",
  df %>%
    filter(report_ready == "yes") %>%
    select(
      idbiont,
      species_name_lat,
      clean_name,
      valid_name,
      status,
      status_ru,
      iucn_year,
      iucn_population_trend_ru
    )
)

addWorksheet(wb, "unmatched")
writeData(
  wb,
  "unmatched",
  df %>%
    filter(is_species, is.na(status)) %>%
    select(idbiont, species_name_lat, clean_name, valid_name)
)

saveWorkbook(wb, "final_iucn_result_v2.xlsx", overwrite = TRUE)