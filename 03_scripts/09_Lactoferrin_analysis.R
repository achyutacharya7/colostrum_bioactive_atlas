library(readxl)
library(tidyverse)

file_path <- "01_raw_data/Colostrum Comparison Study - Results.xlsx"

lactoferrin_raw <- read_excel(file_path, sheet = "Lactoferrin", col_names = FALSE)

lactoferrin_raw %>%
  mutate(row_number = row_number()) %>%
  select(row_number, everything()) %>%
  View()

lactoferrin_clean <- lactoferrin_raw %>%
  slice(39:51) %>%
  select(1, 4) %>%
  rename(
    Group = 1,
    Lactoferrin_ug_mL_DM = 2
  )

View(lactoferrin_clean)

lactoferrin_clean <- lactoferrin_clean %>%
  mutate(
    Lactoferrin_ug_mL_DM = as.numeric(Lactoferrin_ug_mL_DM),
    Group = case_when(
      Group == "Beef colostrum" ~ "Beef",
      str_detect(Group, "Dairy") ~ "Dairy",
      str_detect(Group, "Defatted") ~ "Defatted",
      str_detect(Group, "Pre trt") ~ "SCCL_Pre",
      str_detect(Group, "Dried") ~ "SCCL_Dried",
      TRUE ~ Group
    )
  )

View(lactoferrin_clean)

lactoferrin_summary <- lactoferrin_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_Lactoferrin = mean(Lactoferrin_ug_mL_DM),
    sd_Lactoferrin = sd(Lactoferrin_ug_mL_DM),
    .groups = "drop"
  )

View(lactoferrin_summary)

write_csv(lactoferrin_summary, "04_results/tables/lactoferrin_summary.csv")

ggplot(lactoferrin_clean, aes(x = Group, y = Lactoferrin_ug_mL_DM)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "Lactoferrin Concentration (DM corrected)",
    x = "Group",
    y = "Lactoferrin (ug/mL of DM)"
  ) +
  theme_classic(base_size = 14)

ggsave(
  "04_results/figures/lactoferrin_plot.png",
  width = 6,
  height = 4,
  dpi = 300
)

