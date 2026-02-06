library(tidyverse)
library(text2sdg)

data_raw <- read_csv(
  "data/all_1227.csv",
  col_types = cols(.default = col_character())
)

sdg_hits <- detect_sdg_systems(text = data_raw$Abstract)

sdg_list <- sdg_hits |>
  mutate(
    sdg_num = as.numeric(str_extract(sdg, "\\d+")),
    sdg_tag = paste0("sdg", sdg_num)
  ) |>
  distinct(document, sdg_tag, sdg_num) |> 
  mutate(document = as.integer(document))

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
    ),
    `Index Keywords` = case_when(
      !is.na(`Index Keywords`) & !is.na(sdg_string) ~ paste(`Index Keywords`, sdg_string, sep = "; "),
      is.na(`Index Keywords`) & !is.na(sdg_string) ~ sdg_string,
      TRUE ~ `Index Keywords`
    )
  ) |> 
  select(-temp_id, -sdg_string)


write_csv(data_processed, "data/bib_data/all_bib.csv")

sdg_counts <- sdg_list |>
  count(sdg_tag, sort = TRUE)

print(sdg_counts)

ggplot(sdg_counts, aes(x = reorder(sdg_tag, n), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = n),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(x = "SDG", y = "Count") +
  theme_minimal()


# Fig
sdg_year <- data_raw |>
  mutate(document = row_number(),
         year = as.integer(`Year`)) |>
  filter(!is.na(year)) |>
  filter(year >= 2000) |> 
  left_join(sdg_list, by = "document") |>
  filter(!is.na(sdg_num), !is.na(sdg_tag))

sdg_counts_year <- sdg_year |>
  count(year, sdg_tag, sdg_num, name = "n") |>
  mutate(
    sdg_lab = factor(
      paste0("SDG ", sdg_num),
      levels = paste0("SDG ", sort(unique(sdg_num)))
    )
  )

ggplot(sdg_counts_year, aes(year, n, color = sdg_lab, group = sdg_lab)) +
  geom_line() +
  labs(color = "SDG") +
  theme_minimal() +
  scale_color_brewer(palette = "Set3") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 6))


# Ensure sdg_colors is defined as shown previously
sdg_colors <- c(
  "SDG 1"  = "#E5243B", "SDG 2"  = "#DDA63A", "SDG 3"  = "#4C9F38",
  "SDG 4"  = "#C5192D", "SDG 5"  = "#FF3A21", "SDG 6"  = "#26BDE2",
  "SDG 7"  = "#FCC30B", "SDG 8"  = "#A21942", "SDG 9"  = "#FD6925",
  "SDG 10" = "#DD1367", "SDG 11" = "#FD9D24", "SDG 12" = "#BF8B2E",
  "SDG 13" = "#3F7E44", "SDG 14" = "#0A97D9", "SDG 15" = "#56C02B",
  "SDG 16" = "#00689D", "SDG 17" = "#19486A"
)

top_n <- 8

keep_sdg <- sdg_counts_year |>
  group_by(sdg_lab) |>
  summarise(total = sum(n), .groups = "drop") |>
  slice_max(total, n = top_n) |>
  pull(sdg_lab)

ggplot(
  sdg_counts_year |> filter(sdg_lab %in% keep_sdg),
  aes(year, n, color = sdg_lab, group = sdg_lab)
) +
  geom_line(linewidth = 1.2) + # Slightly thicker lines for better color visibility
  theme_minimal() +
  labs(title = "Top 8 SDGs Over Time", x = "Year", y = "Count", color = "SDG") +
  scale_color_manual(values = sdg_colors) + # Use scale_color_manual here
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 4)) # 4 columns looks cleaner for 8 items

# Fig 2
library(treemapify)

sdg_counts <- sdg_list |>
  filter(!is.na(sdg_num), !is.na(sdg_tag)) |>
  count(sdg_num, sdg_tag, name = "n") |>
  mutate(sdg_lab = factor(paste0("SDG ", sdg_num), levels = paste0("SDG ", 1:17)))

# 1. Define the official SDG hex color palette
sdg_colors <- c(
  "SDG 1"  = "#E5243B", "SDG 2"  = "#DDA63A", "SDG 3"  = "#4C9F38",
  "SDG 4"  = "#C5192D", "SDG 5"  = "#FF3A21", "SDG 6"  = "#26BDE2",
  "SDG 7"  = "#FCC30B", "SDG 8"  = "#A21942", "SDG 9"  = "#FD6925",
  "SDG 10" = "#DD1367", "SDG 11" = "#FD9D24", "SDG 12" = "#BF8B2E",
  "SDG 13" = "#3F7E44", "SDG 14" = "#0A97D9", "SDG 15" = "#56C02B",
  "SDG 16" = "#00689D", "SDG 17" = "#19486A"
)

# 2. Update the Treemap
ggplot(sdg_counts, aes(area = n, fill = sdg_lab, label = paste0(sdg_lab, "\n", n))) +
  geom_treemap(alpha = 0.9, color = "white", size = 2) + # White borders look cleaner with SDG colors
  geom_treemap_text(
    reflow = TRUE, 
    place = "centre", 
    color = "black"
  ) +
  scale_fill_manual(values = sdg_colors) + # Apply the manual palette
  theme_void() +
  guides(fill = "none")

# New fig
dat_reg <- read.csv("data/updated_data.csv") |> 
  filter(year >= 2000)

dat_reg |>
  select(year, starts_with("sdg")) |>
  pivot_longer(starts_with("sdg"), names_to = "sdg", values_to = "v") |>
  filter(v == 1, !is.na(year)) |>
  count(year, sdg) |>
  group_by(sdg) |>
  mutate(tot = sum(n)) |>
  ungroup() |>
  filter(dense_rank(desc(tot)) <= 17) |>
  group_by(year) |>
  mutate(r = dense_rank(desc(n))) |>
  ungroup() |>
  ggplot(aes(year, r, color = sdg, group = sdg)) +
  geom_line(linewidth = 1) +
  scale_y_reverse(breaks = 1:17) +
  theme_minimal() +
  labs(x = "Year", y = "Rank (1 = most)", color = "SDG")


library(ggalluvial)

dat_reg |>
  select(starts_with("sdg"), LS, SS, PS, HS) |>
  pivot_longer(c(LS, SS, PS, HS), names_to = "macro", values_to = "m") |>
  filter(m == 1) |>
  pivot_longer(starts_with("sdg"), names_to = "sdg", values_to = "v") |>
  filter(v == 1) |>
  count(macro, sdg, name = "n") |>
  ggplot(aes(axis1 = macro, axis2 = sdg, y = n)) +
  geom_alluvium(aes(fill = macro), width = 1/12) +
  geom_stratum(width = 1/10, fill = "grey90", color = "grey50") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Macro", "SDG"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  guides(fill = "none")




d <- dat_reg |>
  select(year, starts_with("sdg")) |>
  pivot_longer(starts_with("sdg"), names_to = "sdg", values_to = "v") |>
  filter(!is.na(year), v == 1) |>
  count(year, sdg, name = "n") |>
  mutate(sdg = factor(sdg, levels = sprintf("sdg%02d", 1:17)))

dat_reg |>
  select(starts_with("sdg")) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(everything(), names_to = "sdg", values_to = "n") |>
  mutate(sdg = factor(sdg, levels = sprintf("sdg%02d", 1:17))) |>
  ggplot(aes(sdg, n)) +
  geom_col() +
  coord_polar() +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  labs(y = "Count")

d |>
  group_by(sdg) |>
  summarise(
    first_year = min(year),
    peak_year  = year[which.max(n)],
    .groups = "drop"
  ) |>
  ggplot(aes(first_year, sdg)) +
  geom_point(size = 3) +
  geom_segment(aes(x = first_year, xend = peak_year, yend = sdg), linewidth = 1) +
  theme_minimal() +
  labs(x = "Year", y = "SDG")

