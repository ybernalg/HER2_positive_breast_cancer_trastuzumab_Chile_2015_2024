# -----------------------------------------------------------
# 01_prepare.R
# Data cleaning, recoding, filters for HER2+ breast cancer
# -----------------------------------------------------------

# Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(data.table)
library(readr)

# Load dataset
LRS_2024_12 <- read_excel("data/raw/LRS_2024_12.xlsx")

# Rename columns
colnames(LRS_2024_12) <- c(
  "Num_referencia", "Problema_salud", "Estado", "Decretos", "Edad", "Sexo", 
  "Prevision", "Tramo", "Nacionalidad", "Fecha_confirmacion_descartes", 
  "Principio_activo", "Motivo_cierre_descarte", "Establecimiento_origen", 
  "Seremi_servicio_salud_origen", "Region_origen", "Region_tratamiento", 
  "Establecimiento_tratamiento", "Seremi_servicio_salud_tratamiento"
)

# Remove N/A in Motivo_cierre_descarte
lrs <- filter(LRS_2024_12, Motivo_cierre_descarte != "N/A")

# Recode motivo cierre
lrs$Motivo_cierre_descarte <- ifelse(
  lrs$Motivo_cierre_descarte %in% c("Falleci\x97", "Falleció", "Fallecido por Sistema", "Fallecimiento"),
  "fallecido", lrs$Motivo_cierre_descarte
)

lrs$Motivo_cierre_descarte <- ifelse(
  lrs$Motivo_cierre_descarte %in% c("Alta médica del paciente", "Tratamiento con medicamento Finalizado"),
  "alta", lrs$Motivo_cierre_descarte
)

# Keep only alta or fallecido
lrs <- filter(lrs, Motivo_cierre_descarte %in% c("alta", "fallecido"))

# Keep HER2+ breast cancer
lrs <- filter(lrs, Problema_salud == "Cáncer de mama Gen Her2")

# Derived variables
lrs$Edad <- as.numeric(lrs$Edad)
lrs$nationality_dic <- ifelse(lrs$Nacionalidad == "CHILE", "chilean", "non-chilean")
lrs$region_dic <- ifelse(lrs$Region_origen == "Metropolitana de Santiago", "Metropolitana_de_Santiago", "otras_regiones")
lrs$prevision_dic <- ifelse(lrs$Prevision == "Fonasa", "FONASA", "otras")

lrs$edad_categoria <- cut(
  lrs$Edad,
  breaks = c(-Inf, 34, 44, 54, 64, 74, 84, Inf),
  labels = c("menor de 35", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
  right = TRUE
)

# Save clean dataset
write.table(lrs, "data/processed/df_clean_trastuzumab_dic_alta.tsv", row.names = FALSE, sep = "\t")

message("01_prepare.R completed successfully")
