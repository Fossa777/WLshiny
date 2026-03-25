library(rfishbase)
library(readxl)
library(dplyr)
library(openxlsx)

df <- read_excel("species_with_status.xlsx")

# только нормальные биноминалы
x <- df$clean_name
x <- x[!is.na(x) & grepl("^[A-Z][a-z]+ [a-z]+$", x)]

# валидные названия по FishBase
valid_map <- tibble(
  clean_name = x,
  valid_name = validate_names(x)
) %>% distinct()

result <- df %>%
  left_join(valid_map, by = "clean_name")

write.xlsx(result, "species_valid_names.xlsx")