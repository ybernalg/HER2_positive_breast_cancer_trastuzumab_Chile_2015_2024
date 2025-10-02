## Repository structure

```text
├─ data/
│  ├─ raw/                    # Original files from FONASA (e.g., LRS_2024_12.xlsx)
│  └─ processed/              # Clean/derived data (e.g., df_clean_trastuzumab_dic_alta.tsv)
├─ R/
│  ├─ 01_prepare.R            # Read, rename, filters for HER2+, recodes & dictionaries
│  ├─ 02_descriptives.R       # Tables & crude regional death proportions
│  ├─ 03_models.R             # Logistic regression (uni/multivariable), McFadden’s R²
│  ├─ 04_figures.R            # Forest plot, bar charts, choropleth map, alluvial flows
│  └─ 05_network_mgm.R        # Categorical MGM network + qgraph
├─ figures/                   # Exported plots (forest, map, bars, alluvial)
├─ outputs/                   # Final tables (OR, 95% CI, p), logs, saved objects
├─ README.md
└─ LICENSE
