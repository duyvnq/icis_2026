# Setup ------------------------------------------------------------------------
library(tidyverse)
library(text2sdg)

# Define file paths
DATA_DIR <- "data"
FUNDER_DIR <- file.path(DATA_DIR, "data_by_funders")

# 1. LOAD MAIN DATA ------------------------------------------------------------
data_reg <- read_csv(file.path(DATA_DIR, "all_1020.csv"),
  show_col_types = FALSE
) |>
  select(Year, "Source title", "Cited by", Affiliations, DOI, "Open Access", Abstract) |>
  rename(
    year = Year,
    title = "Source title",
    cited = "Cited by",
    affiliations = Affiliations,
    open_access_status = "Open Access", # Renamed to avoid conflict with OA flag
    abstract = Abstract
  ) |>
  mutate(
    id = row_number(),
    OA = as.integer(!is.na(open_access_status) & open_access_status != "") # More direct 0/1
  ) |>
  select(-open_access_status) |>
  relocate(id, .before = 1)

# 2. MERGE JOURNAL-LEVEL DATA (SJR & SUBJECT AREAS) ---------------------------

# Load source classification (subject areas, macro-areas)
data_source <- read_csv(file.path(DATA_DIR, "sources.csv"),
  show_col_types = FALSE
) |>
  distinct(title, .keep_all = TRUE) # Distinct before arranging if title is primary key

# Load SJR data
sjr <- read_delim(file.path(DATA_DIR, "sjr.csv"),
  delim = ";",
  show_col_types = FALSE
) |>
  filter(Type == "journal") |>
  select(Title, SJR, `SJR Best Quartile`) |>
  rename(
    title = Title,
    sjr_score = SJR,
    quartile = `SJR Best Quartile`
  ) |>
  mutate(
    sjr_score = parse_number(sjr_score, locale = locale(grouping_mark = ",")) / 1000,
    quartile = factor(quartile, levels = c("Q1", "Q2", "Q3", "Q4"))
  ) |>
  distinct(title, .keep_all = TRUE)

# Merge journal data
data_reg <- data_reg |>
  left_join(data_source, by = "title") |>
  left_join(sjr, by = "title")

message(sprintf("SJR missingness: %.1f%%", 100 * mean(is.na(data_reg$sjr_score))))

# 3. EXTRACT COLLABORATION STRUCTURE -------------------------------------------
affiliations_processed <- data_reg |>
  select(id, affiliations) |>
  separate_rows(affiliations, sep = ";") |>
  mutate(
    affiliations = str_trim(affiliations),
    country = str_extract(affiliations, "[^,]+$") |> str_trim()
  ) |>
  distinct(id, country) # Ensure unique country per paper for counting

# Count unique countries per paper
coop_type <- affiliations_processed |>
  count(id, name = "n_countries") |>
  mutate(
    coop = case_when(
      n_countries == 2 ~ "bilateral",
      n_countries > 2 ~ "multilateral",
      TRUE ~ NA_character_
    )
  )

data_reg <- data_reg |>
  left_join(coop_type, by = "id")

message(sprintf(
  "Bilateral: %d | Multilateral: %d",
  sum(data_reg$coop == "bilateral", na.rm = TRUE),
  sum(data_reg$coop == "multilateral", na.rm = TRUE)
))

# 4. MERGE FUNDING DATA --------------------------------------------------------
funder_files_df <- tribble(
  ~file,              ~region, ~sector,
  "asian_pub.csv",    "asian", "pub",
  "asian_uni.csv",    "asian", "uni",
  "eu_ind.csv",       "eu",    "ind",
  "eu_pub.csv",       "eu",    "pub",
  "eu_uni.csv",       "eu",    "uni",
  "int_ind.csv",      "int",   "ind",
  "int_pub.csv",      "int",   "pub",
  "jap_ind.csv",      "jap",   "ind",
  "jap_pub.csv",      "jap",   "pub",
  "jap_pub_2.csv",    "jap",   "pub",
  "jap_uni.csv",      "jap",   "uni",
  "us_ind.csv",       "us",    "ind",
  "us_pub.csv",       "us",    "pub",
  "vn_pub.csv",       "vn",    "pub",
  "vn_uni.csv",       "vn",    "uni"
)

funder_data <- funder_files_df |>
  rowwise() |> # Process row by row
  group_map(~ { # use group_map for more controlled iteration
    file_path <- file.path(FUNDER_DIR, .x$file)
    if (file.exists(file_path)) {
      read_csv(file_path, show_col_types = FALSE) |>
        select(DOI) |>
        mutate(
          !!sym(.x$region) := 1L, # Use 1L for integer 1
          !!sym(.x$sector) := 1L
        )
    } else {
      warning(sprintf("File not found: %s", file_path))
      tibble()
    }
  }, .keep = TRUE) |>
  list_rbind() |> # Combine list of dataframes
  group_by(DOI) |>
  summarise(
    across(everything(), ~ as.integer(any(. == 1, na.rm = TRUE))), # Already good
    .groups = "drop"
  )

# Merge with main data
data_reg <- data_reg |>
  left_join(funder_data, by = "DOI") |>
  mutate(across(c(asian, eu, int, jap, us, vn, ind, pub, uni), ~ replace_na(., 0L))) # Specify columns

# Create funding indicators
data_reg <- data_reg |>
  mutate(
    fund = as.integer(if_any(asian:vn, ~ . == 1)),
    n_regions = asian + eu + int + jap + us + vn,
    n_sectors = ind + pub + uni,
    bilateral_funding = as.integer(jap == 1 & vn == 1)
  )

message(sprintf(
  "Funded papers: %d (%.1f%%)",
  sum(data_reg$fund == 1),
  100 * mean(data_reg$fund == 1)
))

# 5. EXTRACT AUTHORSHIP PATTERNS -----------------------------------------------
# Pre-split affiliations once for efficiency
data_reg <- data_reg |>
  mutate(
    affiliations_list = str_split(affiliations, ";")
  ) |>
  mutate(
    # First author location
    first_affiliation = map_chr(affiliations_list, ~ str_trim(.x[1])),
    fa_vn = as.integer(str_detect(first_affiliation, regex("vietnam|viet nam", ignore_case = TRUE))),
    fa_jp = as.integer(str_detect(first_affiliation, regex("japan", ignore_case = TRUE))),
    fa_o = as.integer(fa_vn == 0 & fa_jp == 0),

    # Count VN and JP authors
    n_vn_authors = map_int(affiliations_list, ~ sum(str_detect(.x, regex("vietnam|viet nam", ignore_case = TRUE)))),
    n_jp_authors = map_int(affiliations_list, ~ sum(str_detect(.x, regex("japan", ignore_case = TRUE)))),

    # Derived authorship metrics
    n_vn_jp_authors = n_vn_authors + n_jp_authors,
    prop_vn_authors = n_vn_authors / n_vn_jp_authors,

    # Leadership flags
    vn_led = as.integer(fa_vn == 1),
    jp_led = as.integer(fa_jp == 1),

    # Collaboration balance
    collab_type = case_when(
      prop_vn_authors > 0.6 ~ "VN-dominated",
      prop_vn_authors < 0.4 ~ "JP-dominated",
      between(prop_vn_authors, 0.4, 0.6) ~ "Balanced",
      TRUE ~ "Other"
    )
  ) |>
  select(-first_affiliation, -affiliations_list) # Remove temporary columns

message(sprintf(
  "VN-led: %d | JP-led: %d | Other-led: %d",
  sum(data_reg$vn_led), sum(data_reg$jp_led), sum(data_reg$fa_o)
))

# 7. MAP TO SDGs ---------------------------------------------------------------
sdg_results <- detect_sdg_systems(text = data_reg$abstract, system = "SDGO") |>
  mutate(document = as.integer(as.character(document))) |>
  distinct(document, sdg) |>
  mutate(hit = 1L) |>
  pivot_wider(
    id_cols = document,
    names_from = sdg,
    values_from = hit,
    values_fill = 0L # Fill with integer 0
  )


# Merge with main data
data_reg <- data_reg |>
  left_join(sdg_results, by = c("id" = "document")) |>
  mutate(
    across(starts_with("SDG"), ~ replace_na(., 0L)), # Fill with integer 0
    n_sdg = rowSums(pick(starts_with("SDG")), na.rm = TRUE)
  )

message(sprintf("Mean SDGs per paper: %.2f", mean(data_reg$n_sdg)))

# 9. FACTORIZE CATEGORICAL VARIABLES -------------------------------------------
data_reg <- data_reg |>
  mutate(
    OA = factor(OA, levels = c(0, 1), labels = c("Not OA", "OA")),
    coop = factor(coop, levels = c("bilateral", "multilateral")),
    fund = factor(fund, levels = c(0, 1), labels = c("Not funded", "Funded")),
    collab_type = factor(collab_type, levels = c(
      "VN-dominated", "JP-dominated",
      "Balanced", "Other"
    ))
  )

# 10. FINAL CLEANING AND EXPORT ------------------------------------------------
# Remove temporary abstract column (large, not needed for regression)
data_reg <- data_reg |> select(-abstract)

# Reorder columns logically
data_reg <- data_reg |>
  select(
    # Identifiers
    id, DOI, year,

    # Basic info
    title, cited, quartile, sjr_score, OA,

    # Collaboration
    coop, n_countries, affiliations, # Keep original affiliations

    # Funding
    fund, n_regions, n_sectors, bilateral_funding,
    asian, eu, int, jap, us, vn,
    ind, pub, uni,

    # Authorship
    fa_vn, fa_jp, fa_o, vn_led, jp_led,
    n_vn_authors, n_jp_authors, n_vn_jp_authors, prop_vn_authors,
    collab_type,

    # Disciplines (macro-areas)
    LS, SS, PS, HS, mult,

    # Disciplines (specific subjects - 26 columns)
    agr_bio:heal,

    # SDGs
    starts_with("SDG"), n_sdg
  )

# Write final processed data
output_file <- file.path(DATA_DIR, "processed_data.csv")
write_csv(data_reg, output_file, na = "")
