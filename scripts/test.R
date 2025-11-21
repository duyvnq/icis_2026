# ===================================================================
# COMPREHENSIVE CITATION IMPACT REGRESSION MODEL
# Japan-Vietnam Research Collaboration Dataset
# ===================================================================

# Load required packages
library(tidyverse)
library(car)        # For VIF diagnostics
library(broom)      # For tidy model outputs
library(stargazer)  # For regression tables

# Load data
data <- read_csv("data/processed_data.csv")

# ===================================================================
# DATA PREPARATION
# ===================================================================

# Create log-transformed citation variable
data <- data %>%
  mutate(log_cited = log(cited + 1))

# Check for missing data in key variables
missing_summary <- data %>%
  summarise(
    n_total = n(),
    missing_sjr = sum(is.na(sjr_score)),
    missing_quartile = sum(is.na(quartile)),
    missing_prop_vn = sum(is.na(prop_vn_authors)),
    missing_collab_type = sum(is.na(collab_type))
  )
print(missing_summary)

# ===================================================================
# MODEL 1: MOST COMPREHENSIVE (CONTINUOUS VARIABLES)
# ===================================================================
# Uses continuous variables where possible for maximum information
# Includes journal quality (sjr_score)

model1 <- lm(log_cited ~ 
               # === FUNDING VARIABLES ===
               # Regional funders (excluding jap/vn to avoid collinearity)
               asian + eu + us + int +
               # Funder sectors
               pub + uni + ind +
               
               # === COLLABORATION STRUCTURE ===
               n_countries +              # Number of collaborating countries
               prop_vn_authors +          # Vietnamese author proportion (balance)
               n_vn_jp_authors +          # Team size
               
               # === QUALITY & VISIBILITY ===
               sjr_score +                # Journal prestige (continuous)
               OA +                       # Open access status
               n_sdg +                    # SDG relevance/breadth
               
               # === TEMPORAL ===
               year +                     # Publication year (citation time)
               
               # === DISCIPLINARY ===
               PS + LS + HS + SS,         # Macro-area indicators
               
             data = data)

# Model summary
summary(model1)

# ===================================================================
# MODEL 2: COMPREHENSIVE WITH CATEGORICAL COLLABORATION
# ===================================================================
# Uses categorical collaboration variables for easier interpretation

model2 <- lm(log_cited ~ 
               # === FUNDING ===
               asian + eu + us + int +
               pub + uni + ind +
               
               # === COLLABORATION (CATEGORICAL) ===
               coop +                     # Bilateral vs multilateral
               collab_type +              # VN-dominated/JP-dominated/Balanced
               n_vn_jp_authors +
               
               # === QUALITY & VISIBILITY ===
               sjr_score +
               OA +
               n_sdg +
               
               # === TEMPORAL ===
               year +
               
               # === DISCIPLINARY ===
               PS + LS + HS + SS,
               
             data = data)

summary(model2)

# ===================================================================
# MODEL 3: WITHOUT JOURNAL QUALITY (ADDRESSING ENDOGENEITY)
# ===================================================================
# Excludes sjr_score to avoid endogeneity concerns
# Journal prestige may be both cause and effect of citations

model3 <- lm(log_cited ~ 
               # === FUNDING ===
               asian + eu + us + int +
               pub + uni + ind +
               
               # === COLLABORATION ===
               n_countries +
               prop_vn_authors +
               n_vn_jp_authors +
               
               # === VISIBILITY & RELEVANCE ===
               OA +
               n_sdg +
               
               # === TEMPORAL ===
               year +
               
               # === DISCIPLINARY ===
               PS + LS + HS + SS,
               
             data = data)

summary(model3)

# ===================================================================
# MODEL 4: MAXIMAL MODEL (ALL REASONABLE PREDICTORS)
# ===================================================================
# Includes leadership variables and more granular controls

model4 <- lm(log_cited ~ 
               # === FUNDING ===
               asian + eu + us + int +
               pub + uni + ind +
               
               # === COLLABORATION ===
               n_countries +
               prop_vn_authors +
               vn_led +                   # Vietnamese leadership
               n_vn_jp_authors +
               
               # === QUALITY & VISIBILITY ===
               sjr_score +
               OA +
               n_sdg +
               
               # === TEMPORAL ===
               year +
               
               # === DISCIPLINARY ===
               PS + LS + HS + SS + mult,  # Including multidisciplinary
               
             data = data)

summary(model4)

# ===================================================================
# DIAGNOSTICS
# ===================================================================

# Variance Inflation Factors (check multicollinearity)
# VIF > 10 indicates serious collinearity problems
# VIF > 5 suggests potential concerns

cat("\n=== MULTICOLLINEARITY DIAGNOSTICS (Model 1) ===\n")
vif_results <- vif(model1)
print(vif_results)
cat("\nVariables with VIF > 5:\n")
print(vif_results[vif_results > 5])

# Residual diagnostics
cat("\n=== RESIDUAL DIAGNOSTICS ===\n")
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# Cook's Distance (influential observations)
cooks_d <- cooks.distance(model1)
influential <- which(cooks_d > 4/nrow(data))
cat("\nNumber of influential observations (Cook's D > 4/n):", length(influential), "\n")

# ===================================================================
# MODEL COMPARISON
# ===================================================================

# Compare models using AIC and R-squared
model_comparison <- data.frame(
  Model = c("Model 1: Continuous", 
            "Model 2: Categorical Collab", 
            "Model 3: No Journal Quality",
            "Model 4: Maximal"),
  N = c(nobs(model1), nobs(model2), nobs(model3), nobs(model4)),
  R_squared = c(summary(model1)$r.squared,
                summary(model2)$r.squared,
                summary(model3)$r.squared,
                summary(model4)$r.squared),
  Adj_R_squared = c(summary(model1)$adj.r.squared,
                    summary(model2)$adj.r.squared,
                    summary(model3)$adj.r.squared,
                    summary(model4)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4))
)

print(model_comparison)

# ===================================================================
# EXPORT RESULTS
# ===================================================================

# Create regression table
stargazer(model1, model2, model3, model4,
          type = "text",
          title = "Citation Impact Regression Models",
          dep.var.labels = "Log(Citations + 1)",
          column.labels = c("Continuous", "Categorical", "No Journal Qual", "Maximal"),
          covariate.labels = c(
            "Asian Funder", "EU Funder", "US Funder", "Int'l Funder",
            "Public Funder", "University Funder", "Industry Funder",
            "N Countries", "Prop VN Authors", "VN+JP Author Count",
            "SJR Score", "Open Access", "N SDGs",
            "Year", "Physical Sci", "Life Sci", "Health Sci", "Social Sci",
            "Multilateral", "Collab: JP-dominated", "Collab: Other", "Collab: VN-dominated",
            "VN-led", "Multidisciplinary"
          ),
          out = "citation_regression_results.txt")

# Export tidy coefficients
model1_tidy <- tidy(model1, conf.int = TRUE) %>%
  mutate(model = "Model 1")
write_csv(model1_tidy, "model1_coefficients.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results exported to:\n")
cat("- citation_regression_results.txt\n")
cat("- model1_coefficients.csv\n")

# ===================================================================
# INTERPRETATION NOTES
# ===================================================================

cat("\n=== KEY VARIABLES FOR INTERPRETATION ===\n")
cat("
FUNDING EFFECTS:
- asian, eu, us, int: Presence of funders from these regions
- pub, uni, ind: Public, university, or industry funding

COLLABORATION STRUCTURE:
- n_countries: More countries = more diverse collaboration
- prop_vn_authors: Higher = more Vietnamese-led; 0.5 = balanced
- n_vn_jp_authors: Larger teams
- vn_led: Vietnamese first author
- collab_type: Balance of Vietnamese vs Japanese authors

QUALITY INDICATORS:
- sjr_score: Higher = more prestigious journal (CAUTION: may be endogenous)
- OA: Open access may increase visibility
- n_sdg: Broader SDG coverage = wider relevance

TEMPORAL:
- year: Later years have less time to accumulate citations (negative expected)

DISCIPLINARY:
- PS, LS, HS, SS: Different citation practices by field
")

# ===================================================================
# NOTES ON VARIABLE OVERLAP (WHAT WAS EXCLUDED AND WHY)
# ===================================================================

cat("\n=== VARIABLES EXCLUDED TO AVOID OVERLAP ===\n")
cat("
EXCLUDED           | REASON
-------------------|--------------------------------------------------
fund               | Redundant - covered by regional/sector dummies
jap, vn            | Would create perfect collinearity issues
n_regions          | Count of regional dummies (collinear)
n_sectors          | Count of sector dummies (collinear)
bilateral_funding  | Subset of jap + vn (per your request)
n_vn_authors       | Component of prop_vn_authors
n_jp_authors       | Component of prop_vn_authors
jp_led             | Opposite of vn_led (can use one or neither)
quartile           | Categorical version of sjr_score
fa_vn, fa_jp, fa_o | Components of vn_led/jp_led
[26 subject areas] | More granular than macro-areas (can substitute)
")