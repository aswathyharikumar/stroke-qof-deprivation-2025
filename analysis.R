# ================================================
# Stroke QOF Deprivation Study — Analysis
# Author: Aswathy Harikumar
# Date: 2026
# ================================================

# Load packages
library(tidyverse)
library(rstatix)
library(gamlss)
library(car)

# ------------------------------------------------
# SECTION 1 — Load cleaned dataset
# ------------------------------------------------

final_clean <- read.csv("C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/data/cleaned_stroke_dataset.csv")

# Convert imd_quintile to factor
final_clean$imd_quintile <- factor(final_clean$imd_quintile)

# ------------------------------------------------
# SECTION 2 — Descriptive Statistics
# ------------------------------------------------

descriptive_stats <- final_clean %>%
  group_by(imd_quintile) %>%
  summarise(
    median_overall = median(overall_achievement, na.rm = TRUE),
    iqr_overall = IQR(overall_achievement, na.rm = TRUE),
    median_stia007 = median(prop_stia007, na.rm = TRUE),
    iqr_stia007 = IQR(prop_stia007, na.rm = TRUE),
    median_stia014 = median(prop_stia014, na.rm = TRUE),
    iqr_stia014 = IQR(prop_stia014, na.rm = TRUE),
    median_stia015 = median(prop_stia015, na.rm = TRUE),
    iqr_stia015 = IQR(prop_stia015, na.rm = TRUE)
  )

print(descriptive_stats)

# ------------------------------------------------
# SECTION 3 — Box Plots
# ------------------------------------------------

plot_data <- final_clean %>%
  select(imd_quintile, prop_stia007, prop_stia014,
         prop_stia015, overall_achievement) %>%
  pivot_longer(
    cols = c(prop_stia007, prop_stia014, prop_stia015, overall_achievement),
    names_to = "indicator",
    values_to = "proportion"
  ) %>%
  mutate(
    indicator = recode(indicator,
                       "overall_achievement" = "Overall Achievement",
                       "prop_stia007" = "STIA007",
                       "prop_stia014" = "STIA014",
                       "prop_stia015" = "STIA015"
    ),
    imd_quintile = factor(imd_quintile,
                          labels = c("Q1\n(least deprived)",
                                     "Q2", "Q3", "Q4",
                                     "Q5\n(most deprived)"))
  )

ggplot(plot_data, aes(x = imd_quintile, y = proportion, fill = imd_quintile)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "QOF Stroke Indicator Performance by IMD Quintile",
    x = "IMD Quintile",
    y = "Proportion",
    caption = "Q1 = least deprived, Q5 = most deprived"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/boxplots_by_quintile.png",
       width = 10, height = 8, dpi = 300)

# ------------------------------------------------
# SECTION 4 — Kruskal-Wallis Tests
# ------------------------------------------------

kw_overall <- kruskal.test(overall_achievement ~ imd_quintile, data = final_clean)
kw_stia007 <- kruskal.test(prop_stia007 ~ imd_quintile, data = final_clean)
kw_stia014 <- kruskal.test(prop_stia014 ~ imd_quintile, data = final_clean)
kw_stia015 <- kruskal.test(prop_stia015 ~ imd_quintile, data = final_clean)

cat("Kruskal-Wallis Results:\n")
cat("Overall Achievement p =", kw_overall$p.value, "\n")
cat("STIA007 p =", kw_stia007$p.value, "\n")
cat("STIA014 p =", kw_stia014$p.value, "\n")
cat("STIA015 p =", kw_stia015$p.value, "\n")

# ------------------------------------------------
# SECTION 5 — Wilcoxon Pairwise Tests
# ------------------------------------------------

wilcox_stia007 <- final_clean %>%
  wilcox_test(prop_stia007 ~ imd_quintile, p.adjust.method = "bonferroni")

wilcox_stia015 <- final_clean %>%
  wilcox_test(prop_stia015 ~ imd_quintile, p.adjust.method = "bonferroni")

print(wilcox_stia007)
print(wilcox_stia015)

# ------------------------------------------------
# SECTION 6 — ZOIBR Regression Models
# ------------------------------------------------

final_model <- na.omit(final_clean)
final_model$imd_quintile <- factor(final_model$imd_quintile)

# STIA007
model_stia007_uni <- gamlss(prop_stia007 ~ imd_quintile,
                            family = BEINF, data = final_model, trace = FALSE)
model_stia007_multi <- gamlss(prop_stia007 ~ imd_quintile + listsize_quintile +
                                prevalence_quintile + age65_quintile + female_quintile,
                              family = BEINF, data = final_model, trace = FALSE)

# STIA014
model_stia014_uni <- gamlss(prop_stia014 ~ imd_quintile,
                            family = BEINF, data = final_model, trace = FALSE)
model_stia014_multi <- gamlss(prop_stia014 ~ imd_quintile + listsize_quintile +
                                prevalence_quintile + age65_quintile + female_quintile,
                              family = BEINF, data = final_model, trace = FALSE)

# STIA015
model_stia015_uni <- gamlss(prop_stia015 ~ imd_quintile,
                            family = BEINF, data = final_model, trace = FALSE)
model_stia015_multi <- gamlss(prop_stia015 ~ imd_quintile + listsize_quintile +
                                prevalence_quintile + age65_quintile + female_quintile,
                              family = BEINF, data = final_model, trace = FALSE)

# Overall Achievement
model_overall_uni <- gamlss(overall_achievement ~ imd_quintile,
                            family = BEINF, data = final_model, trace = FALSE)
model_overall_multi <- gamlss(overall_achievement ~ imd_quintile + listsize_quintile +
                                prevalence_quintile + age65_quintile + female_quintile,
                              family = BEINF, data = final_model, trace = FALSE)

# ------------------------------------------------
# SECTION 7 — Extract Odds Ratios and CIs
# ------------------------------------------------

extract_results <- function(model) {
  or <- exp(coef(model))
  ci <- exp(confint(model))
  mu_rows <- grep("^mu\\.", rownames(ci))
  results <- data.frame(
    OR = round(or, 3),
    Lower_CI = round(ci[mu_rows, 1], 3),
    Upper_CI = round(ci[mu_rows, 2], 3)
  )
  return(results)
}

cat("\n=== STIA007 ===\n")
print(extract_results(model_stia007_multi))

cat("\n=== STIA014 ===\n")
print(extract_results(model_stia014_multi))

cat("\n=== STIA015 ===\n")
print(extract_results(model_stia015_multi))

cat("\n=== OVERALL ACHIEVEMENT ===\n")
print(extract_results(model_overall_multi))

# ------------------------------------------------
# SECTION 8 — VIF Check
# ------------------------------------------------

vif_model <- lm(prop_stia007 ~ listsize_quintile +
                  prevalence_quintile +
                  age65_quintile +
                  female_quintile,
                data = final_model)

cat("\n=== VIF CHECK ===\n")
print(vif(vif_model))

# ------------------------------------------------
# SECTION 9 — Variability Analysis
# ------------------------------------------------

variability <- final_clean %>%
  group_by(imd_quintile) %>%
  summarise(
    n_practices = n(),
    stia007_below_threshold = sum(prop_stia007 < 0.57, na.rm = TRUE),
    stia007_pct_below = round(stia007_below_threshold / n_practices * 100, 1),
    stia014_below_threshold = sum(prop_stia014 < 0.40, na.rm = TRUE),
    stia014_pct_below = round(stia014_below_threshold / n_practices * 100, 1),
    stia015_below_threshold = sum(prop_stia015 < 0.46, na.rm = TRUE),
    stia015_pct_below = round(stia015_below_threshold / n_practices * 100, 1)
  )

cat("\n=== VARIABILITY ANALYSIS ===\n")
print(variability, width = Inf)
  