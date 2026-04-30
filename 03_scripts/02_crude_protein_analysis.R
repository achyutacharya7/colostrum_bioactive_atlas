# ===============================
# Colostrum Bioactive Atlas
# Crude Protein Analysis
# ===============================

library(readxl)
library(tidyverse)

# Step 1: Import Crude Protein sheet
data_cp <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = "Crude Protein",
  col_names = FALSE
)

# Step 2: Clean data
View(data_cp)
# -------------------------------
# Step 2: Extract clean CP table (right-side block)
# -------------------------------
unique(data_cp_clean$CP_percent)
data_cp_clean <- data_cp_clean %>%
  filter(!is.na(CP_percent)) %>%
  filter(CP_percent != "Average CP, as is %")

data_cp_clean$CP_percent <- as.numeric(data_cp_clean$CP_percent)
unique(data_cp_clean$CP_percent)
data_cp_clean <- data_cp_clean %>%
  mutate(
    Group = case_when(
      str_detect(Sample, "Beef") ~ "Beef",
      str_detect(Sample, "Dairy") ~ "Dairy",
      str_detect(Sample, "Defatted") ~ "Defatted",
      str_detect(Sample, "Pre trt") ~ "SCCL_Pre",
      str_detect(Sample, "Dried") ~ "SCCL_Dried",
      TRUE ~ "Other"
    )
  )

View(data_cp_clean)
cp_summary <- data_cp_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_CP = mean(CP_percent, na.rm = TRUE),
    SD_CP = sd(CP_percent, na.rm = TRUE)
  )
cp_summary <- data_cp_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_CP = mean(CP_percent, na.rm = TRUE),
    SD_CP = sd(CP_percent, na.rm = TRUE)
  )
data_cp_clean <- data_cp_clean %>%
  filter(Group != "Other") %>%
  distinct(Sample, .keep_all = TRUE)
cp_summary
cp_summary
write.csv(
  cp_summary,
  "04_results/tables/crude_protein_summary.csv",
  row.names = FALSE
)
ggplot(data_cp_clean, aes(x = Group, y = CP_percent)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  labs(
    title = "Crude Protein across Colostrum Groups",
    x = NULL,
    y = "Crude Protein (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave(
  "04_results/figures/crude_protein_group_comparison.png",
  width = 7,
  height = 5,
  dpi = 300
)
