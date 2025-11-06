
# Setup ------------------------------------------------------------------------
library(tidyverse)
library(text2sdg)

# Define file paths
DATA_DIR <- "data"
FUNDER_DIR <- file.path(DATA_DIR, "data_by_funders")

# 1. LOAD MAIN DATA ------------------------------------------------------------
data_raw <- read_csv(file.path(DATA_DIR, "all_1020.csv"), 
                     show_col_types = FALSE)

# Split into regression data (minimal columns) and full bibliometric data
data_reg <- data_raw |> 
  select(Year, 'Source title', 'Cited by', Affiliations, DOI, 'Open Access', Abstract) |> 
  rename(
    year = Year,
    title = 'Source title',
    cited = 'Cited by',
    affiliations = Affiliations,
    open_access = 'Open Access',
    abstract = Abstract
  ) |> 
  mutate(
    id = row_number(),
    OA = if_else(open_access == "", 0, 1)
  ) |> 
  select(-open_access) |> 
  relocate(id, .before = 1)

# 2. MERGE JOURNAL-LEVEL DATA (SJR & SUBJECT AREAS) ---------------------------

# Load source classification (subject areas, macro-areas)
data_source <- read_csv(file.path(DATA_DIR, "sources.csv"), 
                        show_col_types = FALSE) |> 
  arrange(title) |> 
  distinct(title, .keep_all = TRUE)

# Load SJR data
sjr <- read_delim(file.path(DATA_DIR, "sjr.csv"), 
                  delim = ";", 
                  show_col_types = FALSE) |> 
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
affiliations_long <- data_reg |> 
  select(id, affiliations) |> 
  separate_rows(affiliations, sep = ";") |> 
  mutate(
    affiliations = str_trim(affiliations),
    country = str_extract(affiliations, "[^,]+$") |> str_trim()
  )

# Count unique countries per paper
coop_type <- affiliations_long |> 
  distinct(id, country) |> 
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

message(sprintf("Bilateral: %d | Multilateral: %d", 
                sum(data_reg$coop == "bilateral", na.rm = TRUE),
                sum(data_reg$coop == "multilateral", na.rm = TRUE)))

# 4. MERGE FUNDING DATA --------------------------------------------------------
# Define funder file mapping
funder_files <- tribble(
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

# Load and merge all funder files efficiently
funder_data <- map_dfr(1:nrow(funder_files), function(i) {
  file_path <- file.path(FUNDER_DIR, funder_files$file[i])
  if (file.exists(file_path)) {
    read_csv(file_path, show_col_types = FALSE) |> 
      select(DOI) |> 
      mutate(
        !!funder_files$region[i] := 1,
        !!funder_files$sector[i] := 1
      )
  } else {
    warning(sprintf("File not found: %s", file_path))
    tibble()
  }
}) |> 
  group_by(DOI) |> 
  summarise(
    across(everything(), ~ as.integer(any(. == 1, na.rm = TRUE))),
    .groups = "drop"
  )

# Merge with main data
data_reg <- data_reg |> 
  left_join(funder_data, by = "DOI") |> 
  mutate(across(asian:vn, ~ replace_na(., 0)))

# Create funding indicators
data_reg <- data_reg |> 
  mutate(
    fund = as.integer(if_any(asian:vn, ~ . == 1)),
    n_regions = asian + eu + int + jap + us + vn,
    n_sectors = ind + pub + uni,  # FIXED: was ind + pub + ind
    bilateral_funding = as.integer(jap == 1 & vn == 1)  # NEW
  )

message(sprintf("Funded papers: %d (%.1f%%)", 
                sum(data_reg$fund == 1), 
                100 * mean(data_reg$fund == 1)))

# 5. EXTRACT AUTHORSHIP PATTERNS -----------------------------------------------
data_reg <- data_reg |> 
  mutate(
    # First author location
    first_affiliation = str_extract(affiliations, "^[^;]+"),
    fa_vn = as.integer(str_detect(first_affiliation, regex("vietnam|viet nam", ignore_case = TRUE))),
    fa_jp = as.integer(str_detect(first_affiliation, regex("japan", ignore_case = TRUE))),
    fa_o = as.integer(fa_vn == 0 & fa_jp == 0),
    
    # Count VN and JP authors
    n_vn_authors = map_int(affiliations, ~ {
      affs <- str_split(.x, ";")[[1]]
      sum(str_detect(affs, regex("vietnam|viet nam", ignore_case = TRUE)))
    }),
    n_jp_authors = map_int(affiliations, ~ {
      affs <- str_split(.x, ";")[[1]]
      sum(str_detect(affs, regex("japan", ignore_case = TRUE)))
    }),
    
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
  select(-first_affiliation)

message(sprintf("VN-led: %d | JP-led: %d | Other-led: %d",
                sum(data_reg$vn_led), sum(data_reg$jp_led), sum(data_reg$fa_o)))

# 7. MAP TO SDGs ---------------------------------------------------------------
sdg_results <- detect_sdg_systems(text = data_reg$abstract, system = "SDGO") 

sdg_results <- sdg_results |> 
  mutate(document = as.integer(as.character(document))) |> 
  distinct(document, sdg) |> 
  mutate(hit = 1) |> 
  pivot_wider(
    id_cols = document,
    names_from = sdg,
    values_from = hit,
    values_fill = 0
  )

sdg_cols <- names(sdg_results)[str_starts(names(sdg_results), "SDG")]
sdg_results <- sdg_results |> 
  mutate(across(all_of(sdg_cols), ~ if_else(. > 0, 1, 0)))

# Merge with main data
data_reg <- data_reg |> 
  left_join(sdg_results, by = c("id" = "document")) |> 
  mutate(
    across(starts_with("SDG"), ~ replace_na(., 0)),
    n_sdg = rowSums(pick(starts_with("SDG")), na.rm = TRUE)
  )

message(sprintf("Mean SDGs per paper: %.2f", mean(data_reg$n_sdg)))

# 9. FACTORIZE CATEGORICAL VARIABLES -------------------------------------------
data_reg <- data_reg |> 
  mutate(
    OA = factor(OA, levels = c(0, 1), labels = c("Not OA", "OA")),
    coop = factor(coop, levels = c("bilateral", "multilateral")),
    fund = factor(fund, levels = c(0, 1), labels = c("Not funded", "Funded")),
    collab_type = factor(collab_type, levels = c("VN-dominated", "JP-dominated", 
                                                   "Balanced", "Other"))
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
    coop, n_countries, affiliations,
    
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

message(sprintf("\n=== PROCESSING COMPLETE ==="))
message(sprintf("Final dataset: %d articles × %d variables", 
                nrow(data_reg), ncol(data_reg)))
message(sprintf("Saved to: %s", output_file))

# Print summary statistics
message("\n=== SUMMARY STATISTICS ===")
cat(sprintf("
Time period: %d - %d
Publications by phase:
  Phase 1 (2000-2008): %d
  Phase 2 (2009-2013): %d
  Phase 3 (2014-2025): %d

Collaboration:
  Bilateral: %d (%.1f%%)
  Multilateral: %d (%.1f%%)

Funding:
  Funded: %d (%.1f%%)
  Mean funding regions: %.2f
  Bilateral co-funding: %d (%.1f%%)

Authorship:
  VN-led: %d (%.1f%%)
  JP-led: %d (%.1f%%)
  Mean VN author proportion: %.2f

Open Access:
  OA papers: %d (%.1f%%)

VJU affiliation:
  VJU papers: %d (%.1f%%)

Journal quality:
  Q1: %d (%.1f%%)
  Q2: %d (%.1f%%)
  Q3: %d (%.1f%%)
  Q4: %d (%.1f%%)
  Missing: %d (%.1f%%)
",
min(data_reg$year), max(data_reg$year),
sum(data_reg$phase == "Phase 1 (2000-2008)", na.rm = TRUE),
sum(data_reg$phase == "Phase 2 (2009-2013)", na.rm = TRUE),
sum(data_reg$phase == "Phase 3 (2014-2025)", na.rm = TRUE),
sum(data_reg$coop == "bilateral", na.rm = TRUE), 
100 * mean(data_reg$coop == "bilateral", na.rm = TRUE),
sum(data_reg$coop == "multilateral", na.rm = TRUE),
100 * mean(data_reg$coop == "multilateral", na.rm = TRUE),
sum(data_reg$fund == "Funded", na.rm = TRUE),
100 * mean(data_reg$fund == "Funded", na.rm = TRUE),
mean(data_reg$n_regions, na.rm = TRUE),
sum(data_reg$bilateral_funding == 1, na.rm = TRUE),
100 * mean(data_reg$bilateral_funding == 1, na.rm = TRUE),
sum(data_reg$vn_led == 1, na.rm = TRUE),
100 * mean(data_reg$vn_led == 1, na.rm = TRUE),
sum(data_reg$jp_led == 1, na.rm = TRUE),
100 * mean(data_reg$jp_led == 1, na.rm = TRUE),
mean(data_reg$prop_vn_authors, na.rm = TRUE),
sum(data_reg$OA == "OA", na.rm = TRUE),
100 * mean(data_reg$OA == "OA", na.rm = TRUE),
sum(data_reg$vju_affiliated == 1, na.rm = TRUE),
100 * mean(data_reg$vju_affiliated == 1, na.rm = TRUE),
sum(data_reg$quartile == "Q1", na.rm = TRUE),
100 * mean(data_reg$quartile == "Q1", na.rm = TRUE),
sum(data_reg$quartile == "Q2", na.rm = TRUE),
100 * mean(data_reg$quartile == "Q2", na.rm = TRUE),
sum(data_reg$quartile == "Q3", na.rm = TRUE),
100 * mean(data_reg$quartile == "Q3", na.rm = TRUE),
sum(data_reg$quartile == "Q4", na.rm = TRUE),
100 * mean(data_reg$quartile == "Q4", na.rm = TRUE),
sum(is.na(data_reg$quartile)),
100 * mean(is.na(data_reg$quartile))
))

# ==============================================================================
# END OF PROCESSING SCRIPT
# ==============================================================================