# -----------------------------------------------------------
# 03_models.R
# Logistic regression models (bivariate and multivariable)
# -----------------------------------------------------------

library(dplyr)

# Load clean dataset
df_clean <- read.table("data/processed/df_clean_trastuzumab_dic_alta.tsv", header = TRUE, sep = "\t")

# Binary outcome
df_clean$motivo_cierre_dic <- ifelse(df_clean$Motivo_cierre_descarte == "fallecido", 1, 0)

# Relevel factors
df_clean$prevision_dic <- relevel(factor(df_clean$prevision_dic), ref = "otras")
df_clean$region_dic <- relevel(factor(df_clean$region_dic), ref = "Metropolitana_de_Santiago")

# Variables
factores_lrs <- c("Sexo", "edad_categoria", "prevision_dic", "nationality_dic", "region_dic")

# Bivariate models
resultados <- data.frame()
for (var in factores_lrs) {
  formula <- as.formula(paste("motivo_cierre_dic ~", var))
  modelo <- glm(formula, data = df_clean, family = binomial)
  summary_modelo <- summary(modelo)$coefficients
  for (i in 2:nrow(summary_modelo)) {
    coef <- summary_modelo[i, "Estimate"]
    se <- summary_modelo[i, "Std. Error"]
    p <- summary_modelo[i, "Pr(>|z|)"]
    OR <- exp(coef)
    CI_low <- exp(coef - 1.96 * se)
    CI_high <- exp(coef + 1.96 * se)
    resultados <- rbind(resultados, data.frame(
      Variable = rownames(summary_modelo)[i],
      Coef = round(coef, 3), OR = round(OR, 3),
      CI_low = round(CI_low, 3), CI_high = round(CI_high, 3),
      p_value = round(p, 3)
    ))
  }
}
write.csv(resultados, "outputs/bivariate_results.csv", row.names = FALSE)

# Multivariate model
formula_multivariada <- motivo_cierre_dic ~ edad_categoria + prevision_dic + region_dic + nationality_dic
modelo_multivariado <- glm(formula_multivariada, data = df_clean, family = binomial)
summary(modelo_multivariado)

# Odds ratios with CI
coef <- summary(modelo_multivariado)$coefficients[, "Estimate"]
se <- summary(modelo_multivariado)$coefficients[, "Std. Error"]
OR <- exp(coef)
CI <- exp(confint(modelo_multivariado))
tabla <- data.frame(Variable = names(coef), OR, CI_low = CI[,1], CI_high = CI[,2])
write.csv(tabla, "outputs/multivariable_results.csv", row.names = FALSE)

# McFadden R²
logLik_null <- logLik(glm(motivo_cierre_dic ~ 1, data = df_clean, family = binomial))
logLik_model <- logLik(modelo_multivariado)
R2_McFadden <- 1 - (logLik_model / logLik_null)
print(paste("McFadden's R²:", round(R2_McFadden, 4)))

message("03_models.R completed successfully")
