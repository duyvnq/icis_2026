library(tidyverse)
library(kableExtra)
library(MASS)
library(pscl)

dat <- read_csv("data/processed_data.csv") |>
  filter(year >= 2000) |>
  mutate(
    is_hyperauthored = (n_authors > 50 | n_countries > 10),
    collaboration_mode = case_when(
      is_hyperauthored ~ "Hyperauthored",
      n_countries == 2 ~ "Strict Bilateral",
      n_countries > 2 ~ "Multilateral",
      TRUE ~ "Other"
    ),
    collaboration_mode = factor(collaboration_mode, 
                                levels = c("Strict Bilateral", "Multilateral", "Hyperauthored", "Other"))
  )

dat |>
  group_by(collaboration_mode) |>
  summarise(
    n = n(),
    `%` = sprintf("%.1f%%", n() / nrow(dat) * 100),
    `Mean Authors` = round(mean(n_authors, na.rm = TRUE), 1),
    `Mean Countries` = round(mean(n_countries, na.rm = TRUE), 1),
    `Mean Citations` = round(mean(cited, na.rm = TRUE), 1),
    `Median Citations` = median(cited, na.rm = TRUE)
  ) |>
  kable(caption = "Table 3.2.1: Structural Characteristics by Collaboration Mode (n=9,982)",
        format = "latex", booktabs = TRUE) 

dat_core <- dat |>
  filter(collaboration_mode %in% c("Strict Bilateral", "Multilateral")) |>
  filter(!is.na(cited), !is.na(vn_led), !is.na(jp_led), 
         !is.na(LS), !is.na(PS), !is.na(HS), !is.na(SS))

m_poisson <- glm(cited ~ vn_led + jp_led + year + LS + PS + HS + SS, 
                data = dat_core, family = poisson)

m_nb <- glm.nb(cited ~ vn_led + jp_led + year + LS + PS + HS + SS, 
              data = dat_core)

lr_stat <- 2 * (logLik(m_nb)[1] - logLik(m_poisson)[1])
lr_pval <- pchisq(lr_stat, df = 1, lower.tail = FALSE)

m_zinb <- zeroinfl(cited ~ vn_led + jp_led + year + LS + PS + HS + SS, 
                   dist = "negbin", data = dat_core)

vuong_test <- vuong(m_nb, m_zinb)

tibble(
  Test = c("Poisson vs. NB (Over-dispersion)", "NB vs. ZINB (Zero-inflation)"),
  `Test statistic` = c(sprintf("LR = %.1f", lr_stat), 
                       sprintf("z = %.2f", vuong_test$statistic)),
  df = c(1, NA),
  `p-value` = c(format.pval(lr_pval, digits = 3), 
                format.pval(vuong_test$p.value, digits = 3)),
  Decision = c("Use Negative Binomial", "Use Negative Binomial (not ZINB)")
) |>
  kable(caption = "Table 3.4.1: Over-dispersion and Zero-Inflation Tests",
        format = "latex", booktabs = TRUE) |>
  kable_styling()

