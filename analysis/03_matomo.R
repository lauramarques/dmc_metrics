# This script processes data from Matomo (OpenLearnity), calculates key metrics, and generates corresponding visualizations.

library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)

# load data ----
matomo_pagetitles_20260101_20260331 <- read.csv(here("data", "matomo", "matomo_pagetitles_20260101_20260331.csv"))

modules <- c(
  "Open Research Data, Research Data Management, and FAIR",
  "Writing Data Management Plans",
  "Data Organization and Management",
  "Sensitive Data",
  "Data Documentation and Metadata",
  "Data Storage, Backup and Versioning",
  "Reproducibility and Code Management",
  "Open Formats and Code",
  "Data and Code Licenses",
  "Data Publishing and Long-Term Preservation"
)

matomo_jan_mar_2026 <- matomo_pagetitles_20260101_20260331 |>
  mutate(Label = str_remove(Label, "\\s*\\|\\s*OpenLearnity")) |>
  mutate(Label = str_trim(Label)) |>
  mutate(Label = case_when(
    #str_detect(Label, "Data Organisation and Management|Data Organization and Management") ~ "Data Organisation and Management",
    str_detect(Label, "Open Science and FAIR Data Principles|Open Research Data") ~ "Open Research Data, Research Data Management, and FAIR",
    TRUE ~ Label
  )) |>
  filter(Label %in% modules) |>
  group_by(Label) |>
  summarise(across(where(is.numeric), sum), .groups = "drop") |>
  rename(module_title = Label,
         unique_pageviews = Unique.Pageviews,
         pageviews = Pageviews,
         total_time_spent = Total.time.spent.by.visitors..in.seconds.,
         entrances = Entrances,
         actions_after_entry = Actions.after.entering.here) |>
  ungroup() |>
  mutate(
    # average time per page view
    # avg_time_on_page = total_time_spent / pageviews,
    # engagement per entrance
    engagement_per_entrance = actions_after_entry / entrances
  )

#select variables to plot ----
matomo_metrics <- matomo_jan_mar_2026 |>
  select(
    module_title,
    unique_pageviews,
    #avg_time_on_page,
    engagement_per_entrance
  )

# reshape for plotting ----
metrics_long_matomo <- matomo_metrics |>
  pivot_longer(
    cols = c(unique_pageviews, engagement_per_entrance),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = factor(metric,
                    levels = c("unique_pageviews",
                               "engagement_per_entrance")),
    
    metric_label = recode(metric,
                          unique_pageviews = "Unique pageviews",
                          engagement_per_entrance = "Engagement per entrance"
    )
  )

# plot facet view ----
ggplot(metrics_long_matomo,
       aes(x = reorder(module_title, value),
           y = value)) +
  
  geom_col(aes(fill = metric)) +
  coord_flip() +
  
  facet_wrap(~metric_label, scales = "free_x") +
  
  scale_fill_manual(values = c(
    "unique_pageviews" = "#1B9E77",
    "engagement_per_entrance" = "#D95F02"
  )) +
  
  labs(
    title = "User behaviour/engagement from Matomo (Open Learnity)",
    x = "",
    y = "",
    fill = ""
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 13),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 13)
  )

# get data from Openlearnity
metrics_long_openlearnity <- df_openlearnity_users |>
  mutate(
    metric = "total_users",
    metric_label = "Logged users",
    value = total_users
  ) |>
  rename(module_title = module_label) |>
  select(module_title, metric, metric_label, value)

# combine datasets OL and Matomo
metrics_ol_matomo <- bind_rows(metrics_long_openlearnity, metrics_long_matomo) |>
  mutate(metric_label = factor(metric_label,
                               levels = c("Logged users",
                                          "Unique pageviews",
                                          "Engagement per entrance")
  ))

# plot combine OL and Matomo
ggplot(metrics_ol_matomo,
       aes(x = reorder(module_title, value),
           y = value)) +
  
  geom_col(aes(fill = metric)) +
  coord_flip() +
  
  facet_wrap(~metric_label, scales = "free_x") +
  
  scale_fill_manual(values = c(
    "total_users" = "#7570B3",
    "unique_pageviews" = "#1B9E77",
    "engagement_per_entrance" = "#D95F02"
  )) +
  labs(
    title = "User behaviour from Open Learnity and Matomo",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )



