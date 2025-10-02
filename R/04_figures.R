# -----------------------------------------------------------
# 04_figures.R
# Plots: forest, bar, choropleth map, alluvial
# -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(broom)
library(scales)
library(ggalluvial)
library(RColorBrewer)
library(sf)

# Load clean dataset
df_clean <- read.table("data/processed/df_clean_trastuzumab_dic_alta.tsv", header = TRUE, sep = "\t")

# Example: Forest plot (multivariate model should be run first in 03_models.R)
modelo <- glm(motivo_cierre_dic ~ edad_categoria + prevision_dic + region_dic + nationality_dic,
              data = df_clean, family = binomial)
coef_data <- tidy(modelo, conf.int = TRUE, exponentiate = TRUE)
coef_data <- coef_data[coef_data$term != "(Intercept)", ]

ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_x_log10() +
  labs(x = "Odds Ratio (log scale)", y = "Variables",
       title = "Forest Plot for Multivariate Model") +
  theme_classic()

ggsave("figures/forest_plot.png", width = 6, height = 4)

# Crude proportion of deaths by region (bar chart)
fallecidos_por_region <- df_clean %>%
  group_by(Region_tratamiento) %>%
  summarise(Total = n(),
            Fallecidos = sum(Motivo_cierre_descarte == "fallecido", na.rm = TRUE)) %>%
  mutate(Proporcion_fallecidos = Fallecidos / Total)

ggplot(fallecidos_por_region, aes(x = reorder(Region_tratamiento, Proporcion_fallecidos),
                                  y = Proporcion_fallecidos)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Region", y = "Proportion of deaths",
       title = "Proportion of deaths by treatment region")

ggsave("figures/deaths_by_region.png", width = 6, height = 4)

message("04_figures.R completed successfully")
