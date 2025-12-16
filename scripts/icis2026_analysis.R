library(tidyverse)
library(text2sdg)

data_raw <- read_csv("data/all_1020.csv", show_col_types = FALSE)

sdg_hits <- detect_sdg_systems(text = data_raw$Abstract, system = "SDGO")

sdg_list <- sdg_hits |>
  mutate(
    sdg_num = as.numeric(str_extract(sdg, "\\d+")),
    sdg_tag = paste0("sdg", sdg_num)
  ) |>
  distinct(document, sdg_tag, sdg_num)

sdg_keywords_map <- sdg_list |>
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

sdg_counts <- sdg_list |>
  count(sdg_tag, sort = TRUE)

print(sdg_counts)

ggplot(sdg_counts, aes(x = reorder(sdg_tag, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "SDG Frequency in Dataset", x = "SDG", y = "Count") +
  theme_minimal()


library(purrr)

output_dir <- "data/bib_data"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

walk(1:17, function(i) {
  tag <- paste0("sdg", i)
  pattern <- paste0("\\b", tag, "\\b")
  
  subset_df <- data_processed |>
    filter(str_detect(`Author Keywords`, pattern))
  
  if (nrow(subset_df) > 0) {
    write_csv(subset_df, file.path(output_dir, paste0(tag, "_papers.csv")))
  }
})
