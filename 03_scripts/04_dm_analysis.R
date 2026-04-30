# ================================
# Colostrum Bioactive Atlas
# Dry Matter (DM) Analysis
# ================================

library(readxl)
library(tidyverse)

data_dm <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = "Dry matter",
  col_names = FALSE
)

excel_sheets("01_raw_data/Colostrum Comparison Study - Results.xlsx")

View(data_dm)


select(1, 3)

data_dm_clean <- data_dm %>%
  slice(3:15) %>%
  select(1, 3)

colnames(data_dm_clean) <- c("Sample", "DM_percent")

data_dm_clean$DM_percent <- as.numeric(data_dm_clean$DM_percent)

View(data_dm_clean)

data_dm_clean <- data_dm_clean %>%
  mutate(
    Group = case_when(
      str_detect(Sample, "Beef") ~ "Beef",
      str_detect(Sample, "Dairy") ~ "Dairy",
      str_detect(Sample, "Defatted") ~ "Defatted",
      str_detect(Sample, "Pre") ~ "SCCL_Pre",
      str_detect(Sample, "Dried") ~ "SCCL_Dried",
      TRUE ~ "Other"
    )
  )

View(data_dm_clean)

dm_summary <- data_dm_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_DM = mean(DM_percent, na.rm = TRUE),
    SD_DM = sd(DM_percent, na.rm = TRUE)
  )

dm_summary

ggplot(data_dm_clean, aes(x = Group, y = DM_percent)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  labs(
    title = "Dry Matter (%) across Colostrum Groups",
    y = "DM (%)"
  ) +
  theme_classic(base_size = 14)

write.csv(
  dm_summary,
  "04_results/tables/dm_summary.csv",
  row.names = FALSE
)

ggsave(
  "04_results/figures/dm_group_comparison.png",
  width = 7,
  height = 5,
  dpi = 300
)
  