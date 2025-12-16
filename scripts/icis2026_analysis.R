library(tidyverse)
library(text2sdg)

data_raw <- read_csv("data/all_1020.csv", show_col_types = FALSE)

sdg_hits <- detect_sdg_systems(text = data_raw$Abstract, system = "SDGO")

sdg_keywords_map <- sdg_hits |>
  mutate(
    sdg_num = as.numeric(str_extract(sdg, "\\d+")),
    sdg_tag = paste0("sdg", sdg_num)
  ) |>
  # FIX: Include sdg_num here so it is not dropped
  distinct(document, sdg_tag, sdg_num) |>
  arrange(sdg_num) |>
  group_by(document) |>
  summarise(
    sdg_string = paste(sdg_tag, collapse = "; ")
  ) |>
  ungroup() |>
  mutate(document = as.numeric(as.character(document)))

data_processed <- data_raw |>
  mutate(temp_id = row_number()) |> 
  left_join(sdg_keywords_map, by = c("temp_id" = "document")) |>
  mutate(
    `Author Keywords` = case_when(
      !is.na(`Author Keywords`) & !is.na(sdg_string) ~ paste(`Author Keywords`, sdg_string, sep = "; "),
      is.na(`Author Keywords`) & !is.na(sdg_string) ~ sdg_string,
      TRUE ~ `Author Keywords`
    )
  ) |>
  select(-temp_id, -sdg_string)

write_csv(data_processed, "data/bib_data/all_bib.csv")
