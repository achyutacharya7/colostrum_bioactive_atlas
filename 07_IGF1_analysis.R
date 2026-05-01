library(readxl)
library(tidyverse)

file_path <- "01_raw_data/Colostrum Comparison Study - Results.xlsx"

igf1_raw <- read_excel(file_path, sheet = "IGF-1", col_names = FALSE)

igf1_raw %>%
  mutate(row_number = row_number()) %>%
  select(row_number, everything()) %>%
  View()

igf1_clean <- igf1_raw %>%
  slice(38:50) %>%
  select(1, 4) %>%
  rename(
    Group = 1,
    IGF1_ng_mL_DM = 2
  )

View(igf1_clean)

igf1_clean <- igf1_clean %>%
  mutate(
    IGF1_ng_mL_DM = as.numeric(IGF1_ng_mL_DM),
    Group = case_when(
      Group == "Beef colostrum" ~ "Beef",
      str_detect(Group, "Dairy") ~ "Dairy",
      str_detect(Group, "Defatted") ~ "Defatted",
      str_detect(Group, "Pre trt") ~ "SCCL_Pre",
      str_detect(Group, "Dried") ~ "SCCL_Dried",
      TRUE ~ Group
    )
  )

View(igf1_clean)

igf1_summary <- igf1_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_IGF1 = mean(IGF1_ng_mL_DM),
    sd_IGF1 = sd(IGF1_ng_mL_DM),
    .groups = "drop"
  )

View(igf1_summary)

write_csv(igf1_summary, "04_results/tables/igf1_summary.csv")

ggplot(igf1_clean, aes(x = Group, y = IGF1_ng_mL_DM)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "IGF-1 Concentration (DM corrected)",
    x = "Group",
    y = "IGF-1 (ng/mL of DM)"
  ) +
  theme_classic(base_size = 14)

ggsave(
  "04_results/figures/igf1_plot.png",
  width = 6,
  height = 4,
  dpi = 300
)

