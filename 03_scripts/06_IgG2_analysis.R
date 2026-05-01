setwd("F:/Colostrum_bioactive_atlas")
list.files()
list.files("01_raw_data")
excel_sheets(file_path)
igg2_raw <- read_excel(file_path, sheet = "IgG2", col_names = FALSE)

igg2_raw %>%
  mutate(row_number = row_number()) %>%
  select(row_number, everything()) %>%
  View()

igg2_clean <- igg2_raw %>%
  slice(34:42) %>%
  select(4, 5) %>%
  rename(
    Group = 1,
    IgG2_gL = 2
  )
View(igg2_clean)

igg2_clean <- igg2_clean %>%
  mutate(
    IgG2_gL = as.numeric(IgG2_gL),
    Group = case_when(
      Group == "Beef colostrum" ~ "Beef",
      str_detect(Group, "Dairy") ~ "Dairy",
      Group == "Defatted Colostrum" ~ "Defatted",
      str_detect(Group, "SCCL") ~ "SCCL_Pre",
      TRUE ~ Group
    )
  )

View(igg2_clean)

igg2_summary <- igg2_clean %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_IgG2 = mean(IgG2_gL),
    sd_IgG2 = sd(IgG2_gL),
    .groups = "drop"
  )

View(igg2_summary)

write_csv(igg2_summary, "04_results/tables/igg2_summary.csv")

ggplot(igg2_clean, aes(x = Group, y = IgG2_gL)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "IgG2 Concentration across Colostrum Groups",
    x = "Group",
    y = "IgG2 (g/L)"
  ) +
  theme_classic(base_size = 14)

ggsave(
  "04_results/figures/igg2_plot.png",
  width = 6,
  height = 4,
  dpi = 300
)

