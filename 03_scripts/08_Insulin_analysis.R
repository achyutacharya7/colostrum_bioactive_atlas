library(readxl)
library(tidyverse)

file_path <- "01_raw_data/Colostrum Comparison Study - Results.xlsx"

insulin_raw <- read_excel(file_path, sheet = "Insulin", col_names = FALSE)

insulin_raw %>%
  mutate(row_number = row_number()) %>%
  select(row_number, everything()) %>%
  View()

insulin_clean <- insulin_raw %>%
  slice(36:48) %>%
  select(1, 4) %>%
  rename(
    Group = 1,
    Insulin_ng_mL_DM = 2
  )

View(insulin_clean)

insulin_clean <- insulin_clean %>%
  mutate(
    Insulin_ng_mL_DM = as.numeric(Insulin_ng_mL_DM),
    Group = case_when(
      Group == "Beef colostrum" ~ "Beef",
      str_detect(Group, "Dairy") ~ "Dairy",
      str_detect(Group, "Defatted") ~ "Defatted",
      str_detect(Group, "Pre trt") ~ "SCCL_Pre",
      str_detect(Group, "Dried") ~ "SCCL_Dried",
      TRUE ~ Group
    )
  )

View(insulin_clean)

insulin_summary <- insulin_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_Insulin = mean(Insulin_ng_mL_DM),
    sd_Insulin = sd(Insulin_ng_mL_DM),
    .groups = "drop"
  )

View(insulin_summary)
