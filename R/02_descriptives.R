# -----------------------------------------------------------
# 02_descriptives.R
# Descriptive statistics and crude death proportions
# -----------------------------------------------------------

library(dplyr)
library(table1)

# Load clean dataset
df_clean <- read.table("data/processed/df_clean_trastuzumab_dic_alta.tsv", header = TRUE, sep = "\t")

# Outcome
df_clean$motivo_cierre_dic <- ifelse(df_clean$Motivo_cierre_descarte == "fallecido", 1, 0)

# Table1 summary
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ Edad + factor(edad_categoria) + factor(Sexo) + factor(prevision_dic) + 
         factor(nationality_dic) + factor(region_dic) | Motivo_cierre_descarte, 
       data = df_clean, pval = TRUE, overall = FALSE, extra.col = list(`P-value` = pvalue))

# Crude proportion of deaths by region
fallecidos_por_region <- df_clean %>%
  group_by(Region_tratamiento) %>%
  summarise(
    Total = n(),
    Fallecidos = sum(Motivo_cierre_descarte == "fallecido", na.rm = TRUE)
  ) %>%
  mutate(Proporcion_fallecidos = Fallecidos / Total)

print(fallecidos_por_region)

message("02_descriptives.R completed successfully")
