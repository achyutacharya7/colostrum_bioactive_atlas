# ===============================
# Colostrum Bioactive Atlas
# IgG1 Analysis
# ===============================

library(readxl)
library(tidyverse)

# Check sheet names
excel_sheets("01_raw_data/Colostrum Comparison Study - Results.xlsx")

# Import IgG1 sheet
data_igg1 <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = "IgG1",
  col_names = FALSE
)

# Inspect raw sheet
View(data_igg1)

data_igg1_clean <- data_igg1 %>%
  slice(34:46) %>%
  select(1, 2)

colnames(data_igg1_clean) <- c("Sample", "IgG1_gL")

data_igg1_clean$IgG1_gL <- as.numeric(data_igg1_clean$IgG1_gL)

View(data_igg1_clean)
range(data_igg1_clean$IgG1_gL, na.rm = TRUE)

data_igg1_clean <- data_igg1_clean %>%
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

igg1_summary <- data_igg1_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_IgG1 = mean(IgG1_gL, na.rm = TRUE),
    SD_IgG1 = sd(IgG1_gL, na.rm = TRUE)
  )

igg1_summary

write.csv(
  igg1_summary,
  "04_results/tables/igg1_summary.csv",
  row.names = FALSE
)

ggplot(data_igg1_clean, aes(x = Group, y = IgG1_gL)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  labs(
    title = "IgG1 Concentration across Colostrum Groups",
    y = "IgG1 (g/L)"
  ) +
  theme_classic(base_size = 14)

ggsave(
  "04_results/figures/igg1_group_comparison.png",
  width = 7,
  height = 5,
  dpi = 300
)
