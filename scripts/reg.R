# ==============================================================================
# CITATION IMPACT ANALYSIS: Japan-Vietnam Research Collaboration
# Negative Binomial Regression with Comprehensive Diagnostics
# ==============================================================================

# Load required packages
library(tidyverse)      # Data manipulation and visualization
library(MASS)           # Negative binomial regression
library(car)            # VIF and diagnostics
library(performance)    # Model performance checks
library(pscl)           # Pseudo R-squared and zero-inflated models
library(effects)        # Marginal effects
library(sjPlot)         # Publication-ready tables
library(ggeffects)      # Predicted values and plots
library(broom)          # Tidy model outputs

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================

# Load data
data <- read_csv("data/processed_data.csv")

# Create derived variables
data <- data %>%
  mutate(
    # Exposure variable: years available for citations
    years_available = 2025 - year,
    years_available = pmax(years_available, 0.5),  # Minimum 0.5 to avoid log(0)
    
    # Ensure factors are properly ordered
    quartile = factor(quartile, levels = c("Q4", "Q3", "Q2", "Q1")),  # Q1 as reference (highest)
    collab_type = factor(collab_type, levels = c("JP-dominated", "Balanced", "VN-dominated", "Other")),
    coop = factor(coop, levels = c("bilateral", "multilateral")),
    OA = factor(OA, levels = c("Not OA", "OA")),
    n_regions = factor(n_regions),
    
    # Log-transformed citation (for alternative models)
    log_cited = log(cited + 1)
  )

# Descriptive statistics
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
data %>%
  dplyr::select(cited, years_available, collab_type, coop, bilateral_funding, 
         n_regions, quartile, OA, n_sdg, year) %>%
  summary() %>%
  print()

# Check for missing values in key variables
cat("\n=== MISSING VALUES ===\n")
data %>%
  dplyr::select(cited, collab_type, coop, bilateral_funding, n_regions, 
         quartile, OA, n_sdg, year, LS, PS, HS, SS) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()

# Remove rows with missing values in model variables
data_model <- data %>%
  filter(!is.na(cited) & !is.na(collab_type) & !is.na(coop) & 
         !is.na(bilateral_funding) & !is.na(quartile) & !is.na(OA) &
         !is.na(n_sdg))

cat(sprintf("\nOriginal N = %d, Model N = %d (%.1f%% retained)\n", 
            nrow(data), nrow(data_model), 100*nrow(data_model)/nrow(data)))

# ==============================================================================
# 2. EXPLORATORY VISUALIZATIONS
# ==============================================================================

# Distribution of citations
p1 <- ggplot(data_model, aes(x = cited)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Citation Distribution (log scale)",
       x = "Citations", y = "Frequency") +
  theme_minimal()
print(p1)

# Citations by collaboration type
p2 <- ggplot(data_model, aes(x = collab_type, y = cited, fill = collab_type)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Citations by Collaboration Type",
       x = "Collaboration Type", y = "Citations (log scale)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)

# Citations by quartile
p3 <- ggplot(data_model, aes(x = quartile, y = cited, fill = quartile)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Citations by Journal Quartile",
       x = "Quartile", y = "Citations (log scale)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3)

# ==============================================================================
# 3. CORRELATION ANALYSIS
# ==============================================================================

cat("\n=== CORRELATION MATRIX ===\n")

# Create numeric versions of key variables for correlation
data_cor <- data_model %>%
  mutate(
    collab_balanced = as.numeric(collab_type == "Balanced"),
    collab_vn = as.numeric(collab_type == "VN-dominated"),
    multilateral = as.numeric(coop == "multilateral"),
    q1 = as.numeric(quartile == "Q1"),
    q2 = as.numeric(quartile == "Q2"),
    oa = as.numeric(OA == "OA"),
    n_reg = as.numeric(as.character(n_regions))
  ) %>%
  dplyr::select(cited, collab_balanced, collab_vn, multilateral, bilateral_funding,
         n_reg, q1, q2, oa, n_sdg, year, LS, PS, HS, SS)

cor_matrix <- cor(data_cor, use = "complete.obs")
print(round(cor_matrix, 2))

# Visualize correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Correlation Matrix of Model Variables",
         mar = c(0,0,2,0))

# ==============================================================================
# 4. PRIMARY MODEL: NEGATIVE BINOMIAL REGRESSION
# ==============================================================================

cat("\n=== FITTING PRIMARY MODEL: NEGATIVE BINOMIAL ===\n")

# Fit the model
model_nb <- glm.nb(
  cited ~ 
    collab_type +           # Collaboration balance
    coop +                  # Bilateral vs multilateral
    bilateral_funding +     # Japan-Vietnam co-funding
    n_regions +            # Funding diversity
    quartile +             # Journal quality
    OA +                   # Open access
    n_sdg +                # SDG breadth
    year +                 # Time trend
    LS + PS + HS + SS +    # Disciplines
    offset(log(years_available)),  # Exposure
  data = data_model
)

# Model summary
summary(model_nb)

# ==============================================================================
# 5. MODEL DIAGNOSTICS
# ==============================================================================

cat("\n=== MODEL DIAGNOSTICS ===\n")

# 5.1 Overdispersion test
cat("\n--- Overdispersion Test ---\n")
od_test <- check_overdispersion(model_nb)
print(od_test)

# 5.2 Multicollinearity (VIF)
cat("\n--- Variance Inflation Factors (VIF) ---\n")
cat("Rule of thumb: VIF < 5 is acceptable, VIF < 3 is ideal\n\n")
vif_results <- vif(model_nb)
print(vif_results)

# Flag high VIF
high_vif <- vif_results[vif_results > 5]
if(length(high_vif) > 0) {
  cat("\n⚠️  WARNING: High VIF detected for:\n")
  print(high_vif)
} else {
  cat("\n✓ All VIF values < 5 (no multicollinearity concerns)\n")
}

# 5.3 Pseudo R-squared
cat("\n--- Model Fit Statistics ---\n")
pseudo_r2 <- pR2(model_nb)
print(pseudo_r2)

# 5.4 Predicted vs Observed
data_model$predicted <- predict(model_nb, type = "response")
cor_pred <- cor(data_model$cited, data_model$predicted, use = "complete.obs")
cat(sprintf("\nCorrelation (observed vs predicted): %.3f\n", cor_pred))

# Plot predicted vs observed
p4 <- ggplot(data_model, aes(x = predicted, y = cited)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Predicted vs Observed Citations",
       x = "Predicted Citations", y = "Observed Citations") +
  theme_minimal()
print(p4)

# 5.5 Residual diagnostics
cat("\n--- Residual Diagnostics ---\n")
residuals_pearson <- residuals(model_nb, type = "pearson")
cat(sprintf("Mean Pearson residual: %.4f (should be ~0)\n", mean(residuals_pearson)))
cat(sprintf("SD Pearson residual: %.4f (should be ~1)\n", sd(residuals_pearson)))

# Residual plot
p5 <- ggplot(data_model, aes(x = predicted, y = residuals_pearson)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(color = "orange", se = TRUE) +
  scale_x_log10() +
  labs(title = "Residual Plot",
       x = "Predicted Citations", y = "Pearson Residuals") +
  theme_minimal()
print(p5)

# ==============================================================================
# 6. COEFFICIENT INTERPRETATION
# ==============================================================================

cat("\n=== INCIDENCE RATE RATIOS (IRR) ===\n")
cat("IRR > 1: Increases citations\n")
cat("IRR < 1: Decreases citations\n")
cat("IRR = 1.5 means 50% more citations\n\n")

# Calculate IRRs and confidence intervals
irr <- exp(coef(model_nb))
ci <- exp(confint(model_nb))
irr_table <- data.frame(
  Variable = names(irr),
  IRR = irr,
  CI_lower = ci[,1],
  CI_upper = ci[,2],
  p_value = summary(model_nb)$coefficients[,4]
) %>%
  mutate(
    Significant = ifelse(p_value < 0.001, "***",
                  ifelse(p_value < 0.01, "**",
                  ifelse(p_value < 0.05, "*", ""))),
    Effect = case_when(
      IRR > 1.1 & p_value < 0.05 ~ "Positive",
      IRR < 0.9 & p_value < 0.05 ~ "Negative",
      TRUE ~ "Non-significant"
    )
  ) %>%
  arrange(desc(abs(IRR - 1)))

print(irr_table, digits = 3)

# Export coefficient table

# ==============================================================================
# 7. VISUALIZE KEY EFFECTS
# ==============================================================================

cat("\n=== GENERATING MARGINAL EFFECTS PLOTS ===\n")

# 7.1 Effect of collaboration type
eff_collab <- ggpredict(model_nb, terms = "collab_type")
p6 <- plot(eff_collab) +
  labs(title = "Predicted Citations by Collaboration Type",
       x = "Collaboration Type",
       y = "Predicted Citations") +
  theme_minimal()
print(p6)

# 7.2 Effect of journal quartile
eff_quartile <- ggpredict(model_nb, terms = "quartile")
p7 <- plot(eff_quartile) +
  labs(title = "Predicted Citations by Journal Quartile",
       x = "Quartile",
       y = "Predicted Citations") +
  theme_minimal()
print(p7)

# 7.3 Effect of bilateral funding
eff_bilateral <- ggpredict(model_nb, terms = "bilateral_funding")
p8 <- plot(eff_bilateral) +
  labs(title = "Effect of Japan-Vietnam Co-Funding",
       x = "Bilateral Funding",
       y = "Predicted Citations") +
  theme_minimal()
print(p8)

# 7.4 Effect of open access
eff_oa <- ggpredict(model_nb, terms = "OA")
p9 <- plot(eff_oa) +
  labs(title = "Open Access Citation Advantage",
       x = "Open Access Status",
       y = "Predicted Citations") +
  theme_minimal()
print(p9)

# 7.5 SDG breadth effect
eff_sdg <- ggpredict(model_nb, terms = "n_sdg [0:12]")
p10 <- plot(eff_sdg) +
  labs(title = "Citations by SDG Breadth",
       x = "Number of SDGs Addressed",
       y = "Predicted Citations") +
  theme_minimal()
print(p10)

# ==============================================================================
# 9. ROBUSTNESS CHECKS
# ==============================================================================

cat("\n=== ROBUSTNESS CHECKS ===\n")

# 9.1 Zero-Inflated Negative Binomial (if many zeros)
zero_rate <- mean(data_model$cited == 0)
cat(sprintf("\nProportion of zero citations: %.2f%%\n", 100*zero_rate))

if(zero_rate > 0.15) {
  cat("Testing Zero-Inflated Negative Binomial...\n")
  model_zinb <- zeroinfl(
    cited ~ 
      collab_type + coop + bilateral_funding + n_regions + 
      quartile + OA + n_sdg + year + LS + PS + HS + SS | 
      1,  # Zero-inflation model (can add predictors here)
    data = data_model,
    dist = "negbin"
  )
  
  # Compare with Vuong test
  vuong_test <- vuong(model_zinb, model_nb)
  print(vuong_test)
}

# 9.2 Exclude recent papers (< 2 years)
cat("\n--- Robustness: Excluding papers from 2023-2024 ---\n")
data_robust <- data_model %>% filter(year <= 2022)
model_robust <- update(model_nb, data = data_robust)
cat(sprintf("N reduced from %d to %d\n", nrow(data_model), nrow(data_robust)))

# Compare key coefficients
coef_comparison <- data.frame(
  Variable = names(coef(model_nb)),
  Full_Model = coef(model_nb),
  Robust_Model = coef(model_robust)
) %>%
  mutate(Difference = Full_Model - Robust_Model)

print(head(coef_comparison, 15))

# 9.3 Alternative: OLS with log-transformed citations
cat("\n--- Alternative Model: OLS with log(citations + 1) ---\n")
model_ols <- lm(
  log_cited ~ 
    collab_type + coop + bilateral_funding + n_regions + 
    quartile + OA + n_sdg + year + LS + PS + HS + SS,
  data = data_model
)
summary(model_ols)

# ==============================================================================
# 10. PUBLICATION-READY OUTPUT
# ==============================================================================

cat("\n=== GENERATING PUBLICATION TABLES ===\n")

# HTML table
tab_model(model_nb, 
          show.ci = TRUE,
          show.se = TRUE,
          show.stat = TRUE,
          string.stat = "z-value",
          string.ci = "95% CI",
          string.p = "p-value",
          dv.labels = "Citations (Negative Binomial)",
          file = "outputs/regression_table.html")

# Export summary statistics
summary_stats <- data_model %>%
  dplyr::select(cited, collab_type, coop, bilateral_funding, quartile, OA, n_sdg) %>%
  summary()
capture.output(summary_stats, file = "outputs/summary_statistics.txt")

cat("\n✓ Tables exported to results/ folder\n")

# ==============================================================================
# 11. KEY FINDINGS SUMMARY
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("                           KEY FINDINGS SUMMARY                                \n")
cat("================================================================================\n\n")

# Extract significant effects
sig_effects <- irr_table %>%
  filter(p_value < 0.05, !grepl("Intercept|year|^LS|^PS|^HS|^SS", Variable)) %>%
  arrange(p_value)

cat("SIGNIFICANT PREDICTORS OF CITATION IMPACT:\n\n")
for(i in 1:min(10, nrow(sig_effects))) {
  row <- sig_effects[i,]
  direction <- ifelse(row$IRR > 1, "increases", "decreases")
  pct_change <- round((row$IRR - 1) * 100, 1)
  cat(sprintf("%d. %s %s citations by %+.1f%% (p %s)\n",
              i, row$Variable, direction, pct_change, row$Significant))
}

cat("\n")
cat("MODEL PERFORMANCE:\n")
cat(sprintf("- McFadden's Pseudo R²: %.3f\n", pseudo_r2["McFadden"]))
cat(sprintf("- Predicted-Observed Correlation: %.3f\n", cor_pred))
cat(sprintf("- N observations: %d\n", nrow(data_model)))

cat("\n")
cat("DIAGNOSTIC CHECKS:\n")
cat(sprintf("- Overdispersion: %s\n", ifelse(od_test$dispersion_ratio > 1, "Present (NB appropriate)", "Absent")))
cat(sprintf("- Max VIF: %.2f (threshold: 5)\n", max(vif_results)))
cat(sprintf("- Zero citations: %.1f%%\n", 100*zero_rate))

cat("\n")
cat("================================================================================\n")
cat("Analysis complete! Check results/ folder for exported tables and plots.\n")
cat("================================================================================\n")

# Save workspace
save.image("outputs/citation_analysis.RData")
cat("\n✓ Workspace saved to outputs/citation_analysis.RData\n")