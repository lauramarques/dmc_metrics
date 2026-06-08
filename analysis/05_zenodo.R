# This script processes Zenodo OER data on views and downloads, calculates key metrics, and generates corresponding visualizations.

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

# data for this script is downloaded using the 04_zenodo_download.R script

# load data
zenodo_records <- read.csv(here("data", "zenodo", "zenodo_records_08-06-2026.csv"))

df_zenodo <- zenodo_records |>
  filter(
    str_starts(title, regex("Module", ignore_case = TRUE)) |
      str_starts(title, regex("Case study", ignore_case = TRUE)) |
      str_starts(title, regex("Introduction Video", ignore_case = TRUE))
  ) |>
  mutate(
    resource_group = case_when(
      str_starts(title, "Module") ~ "module",
      str_starts(title, "Case study") ~ "case_study",
      str_starts(title, "Introduction Video") ~ "intro_video",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(resource_group)) |>
  mutate(
    resource_name = case_when(
      resource_group == "module" ~ str_extract(title, '(?<=Module \\").+?(?=\\")'),
      resource_group == "case_study" ~ str_remove(title, "^Case study animated video for learning module about "),
      resource_group == "intro_video" ~ str_remove(title, "^Introduction Video to "),
      TRUE ~ NA_character_
    ),
    resource_name = case_when(
      resource_name == "Data Management Plan" ~ "Writing Data Management Plans",
      resource_name == "Data Publication and Preservation" ~ "Data Publishing and Long-Term Preservation",
      resource_name == "Open Data, FAIR, and Research Data Management" ~ "Open Research Data, Research Data Management, and FAIR",
      TRUE ~ resource_name
    )
  )

# Prepare data
df_summary <- df_zenodo |>
  group_by(resource_name, resource_group) |>
  summarise(
    views = sum(views),
    downloads = sum(downloads),
    total = views + downloads,
    .groups = "drop"
  )

df_long <- df_summary |>
  pivot_longer(
    cols = c(views, downloads),
    names_to = "metric",
    values_to = "value"
  )

zenodo_fig1 <- ggplot(df_long, aes(
  x = resource_group,
  y = value,
  fill = metric
)) +
  geom_col(position = "stack", width = 0.5) +
  scale_x_discrete(labels = c(
    "module" = "Modules",
    "intro_video" = "Intro Videos",
    "case_study" = "Case Studies"
  )) +
  scale_fill_discrete(labels = c(
    views = "Views",
    downloads = "Downloads"
  )) +
  labs(
    x = "Resource type",
    y = "Count",
    title = "Number of views and downloads by resource type in Zenodo",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
zenodo_fig1

zenodo_fig2 <- ggplot(df_summary, aes(x = reorder(resource_name, total), 
                                      y = total, fill = resource_group)) +
  geom_col() +
  coord_flip() +
  scale_fill_discrete(labels = c(
    module = "Modules",
    intro_video = "Intro Videos",
    case_study = "Case Studies"
  )) +
  labs(
    x = "Module",
    y = "Total (views + downloads)",
    title = "Total engagement by resource type and module in Zenodo",
    fill = "Resource type"
  ) +
  theme(legend.position = "bottom")
zenodo_fig2 
  
zenodo_fig3 <- ggplot(df_long, aes(
  x = resource_group,
  y = value,
  fill = metric
)) +
  geom_col(position = "stack", width = 0.3) +   # narrower bars
  scale_x_discrete(labels = c(
    "module" = "Modules",
    "intro_video" = "Intro Videos",
    "case_study" = "Case Studies"
  )) +
  scale_fill_discrete(labels = c(
    views = "Views",
    downloads = "Downloads"
  )) +
  facet_wrap(~ resource_name, scales = "free_x", ncol = 2) +  # 5 panels per row
  labs(
    x = "Resource type",
    y = "Count",
    title = "Number of views and downloads by resource type and module in Zenodo",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
zenodo_fig3

# Plot other resources ----

df_zenodo_other_resources <- zenodo_records |>
  filter(affiliation != "M3")

df_other_summary <- df_zenodo_other_resources |>
  group_by(title, affiliation) |>
  summarise(
    views = sum(views),
    downloads = sum(downloads),
    total = views + downloads,
    .groups = "drop"
  )

df_other_long <- df_other_summary |>
  pivot_longer(
    cols = c(views, downloads),
    names_to = "metric",
    values_to = "value"
  )

zenodo_fig4 <- ggplot(df_other_long, aes(
  x = affiliation,
  y = value,
  fill = metric
)) +
  geom_col(position = "stack", width = 0.3) +   # narrower bars
  scale_x_discrete(labels = c(
    "empa" = "EMPA",
    "eth_library" = "ETH Library",
    "lib4ri" = "Lib4RI",
    "psi" = "PSI",
    "wsl" = "WSL"
  )) +
  scale_fill_discrete(labels = c(
    views = "Views",
    downloads = "Downloads"
  )) +
  labs(
    x = "Institution",
    y = "Value",
    title = "Number of views and downloads submitted by institution in Zenodo",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
zenodo_fig4
