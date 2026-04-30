# ===============================
# Colostrum Bioactive Atlas
# Dry Matter Analysis
# ===============================

# Load libraries
library(readxl)
library(tidyverse)

# -------------------------------
# Step 1: Import data
# -------------------------------
data_dm <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = 1,
  col_names = FALSE
)

# -------------------------------
# Step 2: Clean data
# -------------------------------
colnames(data_dm) <- c(
  "Sample",
  "Final_dry_matter",
  "DM_percent",
  "Blank_column",
  "Rep_A",
  "Rep_B",
  "Mean",
  "DM_percent_2",
  "SD"
)

data_dm <- data_dm[-1, ]
data_dm$Blank_column <- NULL
data_dm$Mean <- as.numeric(data_dm$Mean)

# -------------------------------
# Step 3: Create groups
# -------------------------------
data_dm <- data_dm %>%
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

# -------------------------------
# Step 4: Summary table
# -------------------------------
dry_matter_summary <- data_dm %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_DM = mean(Mean, na.rm = TRUE),
    SD_DM = sd(Mean, na.rm = TRUE)
  )

write.csv(
  dry_matter_summary,
  "04_results/tables/dry_matter_summary.csv",
  row.names = FALSE
)

# -------------------------------
# Step 5: Plot
# -------------------------------
p <- ggplot(data_dm, aes(x = Group, y = Mean)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  labs(
    title = "Dry Matter Content across Colostrum Groups",
    x = NULL,
    y = "Dry matter proportion"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(
  "04_results/figures/dry_matter_group_comparison.png",
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)