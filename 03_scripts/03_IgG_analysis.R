excel_sheets("01_raw_data/Colostrum Comparison Study - Results.xlsx")
# ===============================
# Colostrum Bioactive Atlas
# Total IgG Analysis
# ===============================

library(readxl)
library(tidyverse)

# Step 1: Import Total IgG sheet
data_igg <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = "Total IgG",
  col_names = FALSE
)

# Step 2: Extract correct IgG data (LEFT block)

# Select correct columns (Origin + IgG)
data_igg_clean <- data_igg %>%
  select(1, 3) %>%
  rename(
    Sample = 1,
    IgG_gL = 3
  )

# Clean rows
data_igg_clean <- data_igg_clean %>%
  filter(!is.na(Sample)) %>%
  filter(!is.na(IgG_gL)) %>%
  filter(Sample != "Origin")  # remove header row

# Convert to numeric
data_igg_clean$IgG_gL <- as.numeric(data_igg_clean$IgG_gL)

# Check
View(data_igg_clean)
summary(data_igg_clean)
data_igg_clean <- data_igg[, 1:2]

colnames(data_igg_clean) <- c("Sample", "IgG_gL")
colnames(data_igg)
data_igg_clean <- data_igg[, 1:2]

colnames(data_igg_clean) <- c("Sample", "IgG_gL")
data_igg_clean <- data_igg_clean %>%
  filter(!is.na(Sample)) %>%
  filter(!is.na(IgG_gL)) %>%
  filter(Sample != "Origin")

data_igg_clean$IgG_gL <- as.numeric(data_igg_clean$IgG_gL)
View(data_igg_clean)
# Extract ONLY correct IgG block (rows 32–44)

data_igg_clean <- data_igg %>%
  slice(32:44) %>%
  select(1, 2)

colnames(data_igg_clean) <- c("Sample", "IgG_gL")

# Clean
data_igg_clean <- data_igg_clean %>%
  filter(!is.na(Sample)) %>%
  filter(Sample != "Sample")

# Convert
data_igg_clean$IgG_gL <- as.numeric(data_igg_clean$IgG_gL)

# Check
View(data_igg_clean)
summary(data_igg_clean)
data_igg <- read_excel(
  "01_raw_data/Colostrum Comparison Study - Results.xlsx",
  sheet = "Total IgG",
  col_names = FALSE
)

data_igg_clean <- data_igg %>%
  slice(41:53) %>%
  select(1, 2)

colnames(data_igg_clean) <- c("Sample", "IgG_gL")

data_igg_clean$IgG_gL <- as.numeric(data_igg_clean$IgG_gL)

View(data_igg_clean)
range(data_igg_clean$IgG_gL, na.rm = TRUE)

data_igg_clean <- data_igg_clean %>%
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

igg_summary <- data_igg_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    Mean_IgG = mean(IgG_gL, na.rm = TRUE),
    SD_IgG = sd(IgG_gL, na.rm = TRUE)
  )

igg_summary

ggplot(data_igg_clean, aes(x = Group, y = IgG_gL)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.5, alpha = 0.8) +
  labs(
    title = "IgG Concentration across Colostrum Groups",
    y = "IgG (g/L)"
  ) +
  theme_classic(base_size = 14)

write.csv(
  igg_summary,
  "04_results/tables/igg_summary.csv",
  row.names = FALSE
)

ggsave(
  "04_results/figures/igg_group_comparison.png",
  width = 7,
  height = 5,
  dpi = 300
)

