library(readxl)
library(tidyverse)

# Read sheet 1 without headers
data_dm <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = 1,
  col_names = FALSE
)

# Manually give safe column names
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

# Remove first row because it was the Excel header row
data_dm <- data_dm[-1, ]

# Remove blank column
data_dm$Blank_column <- NULL

# View clean data
View(data_dm)
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

View(data_dm)
data_dm <- data_dm[-1, ]
data_dm$Mean <- as.numeric(data_dm$Mean)
data_dm %>%
  group_by(Group) %>%
  summarise(
    Mean_DM = mean(Mean, na.rm = TRUE),
    SD_DM = sd(Mean, na.rm = TRUE)
  )
View(data_dm)
library(ggplot2)

ggplot(data_dm, aes(x = Group, y = Mean, fill = Group)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "Dry Matter (%) across Colostrum Groups",
    x = "Group",
    y = "Dry Matter"
  ) +
  theme_minimal()
# Publication-style dry matter figure
ggplot(data_dm, aes(x = Group, y = Mean)) +
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
  filename = "04_results/figures/dry_matter_group_comparison.png",
  width = 7,
  height = 5,
  dpi = 300
)
dry_matter_summary <- data_dm %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_DM = mean(Mean, na.rm = TRUE),
    SD_DM = sd(Mean, na.rm = TRUE)
  )

dry_matter_summary
write.csv(
  dry_matter_summary,
  "04_results/tables/dry_matter_summary.csv",
  row.names = FALSE
)
excel_sheets("01_raw_data/Colostrum Comparison Study - Results.xlsx")
