library(tidyverse)
library(ggplot2)
library(fixest) # For OLS with robust standard errors

# --- Phase 1: Data Processing ---

# Function to load and standardize data
load_data <- function(file, country_code) {
  read.csv(file) |>
    select(Year, 'Cited.by', DOI) |>
    rename(year = Year, citations = 'Cited.by') |>
    distinct(DOI, .keep_all = TRUE) |>
    mutate(country = country_code)
}

# Load all datasets
v_jap <- load_data("data/all_1020.csv", "jpn")
v_korea <- load_data("data/DiD/viet_korea.csv", "kor")
v_china <- load_data("data/DiD/viet_china.csv", "chn")
v_thai <- load_data("data/DiD/viet_thai.csv", "tha")
v_sing <- load_data("data/DiD/viet_sing.csv", "sgp")

# Get all unique DOIs from the treatment group
treatment_dois <- v_jap |> pull(DOI)

# Create the "pure" control datasets by removing any paper that is *also* a VJ collab
controls_korea <- v_korea |> filter(!DOI %in% treatment_dois)
controls_other <- bind_rows(v_china, v_thai, v_sing) |>
  distinct(DOI, .keep_all = TRUE) |>
  filter(!DOI %in% treatment_dois)

# --- Phase 2: Create DiD Datasets (Paper Level) ---

# Main Analysis Dataset: VJ (Treat) vs. VK (Control)
did_data_main <- bind_rows(
  v_jap |> mutate(treat = 1),
  controls_korea |> mutate(treat = 0)
) |>
  mutate(post = ifelse(year >= 2014, 1, 0)) |>
  drop_na(citations, year, treat, post)

# Robustness Check Dataset: VJ (Treat) vs. Others (Control)
did_data_robust <- bind_rows(
  v_jap |> mutate(treat = 1),
  controls_other |> mutate(treat = 0)
) |>
  mutate(post = ifelse(year >= 2014, 1, 0)) |>
  drop_na(citations, year, treat, post)


# --- Phase 3: Create DiD Datasets (Volume/Group-Time Level) ---

create_volume_data <- function(paper_level_data) {
  paper_level_data |>
    count(year, treat) |>
    rename(paper_count = n) |>
    complete(year, treat, fill = list(paper_count = 0)) |>
    mutate(
      post = ifelse(year >= 2014, 1, 0)
    )
}

# Volume dataset for Main Analysis (VJ vs VK)
did_volume_main <- create_volume_data(did_data_main)

# Volume dataset for Robustness Check (VJ vs Others)
did_volume_robust <- create_volume_data(did_data_robust)

# --- Phase 4: Parallel Trends Check (Main Analysis) ---

# Plot 1: Average Citations (VJ vs VK)
avg_citations_main <- did_data_main |>
  group_by(year, treat) |>
  summarise(mean_citations = mean(citations, na.rm = TRUE)) |>
  ungroup() |>
  mutate(Group = ifelse(treat == 1, "Vietnam-Japan", "Vietnam-Korea"))

ggplot(avg_citations_main, aes(x = year, y = mean_citations, color = Group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2013.5, linetype = "dashed", color = "red") +
  labs(title = "Parallel Trends: Mean Citations (VJ vs VK)", y = "Mean Citations") +
  theme_minimal()

# Plot 2: Publication Volume (VJ vs VK)
did_volume_main |>
  mutate(Group = ifelse(treat == 1, "Vietnam-Japan", "Vietnam-Korea")) |>
  ggplot(aes(x = year, y = paper_count, color = Group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2013.5, linetype = "dashed", color = "red") +
  labs(title = "Parallel Trends: Publication Volume (VJ vs VK)", y = "Paper Count") +
  theme_minimal()


# --- Phase 5: OLS Models (Main Analysis) ---

# Model 1: Impact on Citation
# We use log(citations + 1) due to skew
# We cluster standard errors by year
model1_citations <- feols(log(citations + 1) ~ treat + post + treat:post,
  data = did_data_main,
  cluster = ~year
)

print("--- Model 1: Impact on Citations (VJ vs VK) ---")
summary(model1_citations)


# Model 2: Impact on Volume
# We use log(paper_count + 1)
model2_volume <- feols(log(paper_count + 1) ~ treat + post + treat:post,
  data = did_volume_main,
  cluster = ~year
)

print("--- Model 2: Impact on Volume (VJ vs VK) ---")
summary(model2_volume)


# --- Phase 6: "Later Phase" (Robustness Checks) ---

# You can now re-run the models using `did_data_robust` and `did_volume_robust`
# This checks if the VJ effect holds when compared to V-China, V-Thailand, and V-Singapore

model1_citations_robust <- feols(log(citations + 1) ~ treat + post + treat:post,
  data = did_data_robust,
  cluster = ~year
)

model2_volume_robust <- feols(log(paper_count + 1) ~ treat + post + treat:post,
  data = did_volume_robust,
  cluster = ~year
)

print("--- Model 1 (Robustness): Impact on Citations (VJ vs Others) ---")
summary(model1_citations_robust)

print("--- Model 2 (Robustness): Impact on Volume (VJ vs Others) ---")
summary(model2_volume_robust)
