# This script processes data from Matomo (OpenLearnity), calculates key metrics, and generates corresponding visualizations.

library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)

# data for this script is manually downloaded from the Matomo platform quaterly.

# load data ----
matomo_pages_20260101_20260331 <- read.csv(here("data", "matomo", "matomo_pages_20260101_20260331.csv"))

pages <- c(
  "/courses/course-v1:open fair data/about",
  "/courses/course-v1:data management plan/about",
  "/courses/course-v1:open data organisation/about",
  "/courses/course-v1:ETH sensitive data/about",
  "/courses/course-v1:ETH documentation metadata/about",
  "/courses/course-v1:storage backup versioning/about",
  "/courses/course-v1:ETH code reproducibility/about",
  "/courses/course-v1:open formats code/about",
  "/courses/course-v1:ETH data licences/about",
  "/courses/course-v1:ETH RDM-01 2024/about"
)

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

module_map <- setNames(modules, pages)

# Extract and rename
matomo_jan_mar_2026 <- matomo_pages_20260101_20260331 |>
  filter(Label %in% pages) |>
  mutate(Label = module_map[Label]) |>
  rename(module_title = Label,
         unique_pageviews = Unique.Pageviews,
         pageviews = Pageviews,
         total_time_spent = Total.time.spent.by.visitors..in.seconds.,
         entrances = Entrances,
         bounces = Bounces,
         actions_after_entry = Actions.after.entering.here) |>
  mutate(
    engagement_per_entrance = actions_after_entry / entrances,
    bounce_rate = bounces / entrances * 100
  )

#select variables to plot ----
matomo_metrics <- matomo_jan_mar_2026 |>
  select(
    module_title,
    unique_pageviews,
    engagement_per_entrance,
    bounce_rate
  )

# reshape for plotting ----
metrics_long_matomo <- matomo_metrics |>
  pivot_longer(
    cols = c(unique_pageviews, engagement_per_entrance, bounce_rate),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = factor(metric,
                    levels = c("unique_pageviews",
                               "engagement_per_entrance",
                               "bounce_rate")),
    
    metric_label = recode(metric,
                          unique_pageviews = "Unique pageviews",
                          engagement_per_entrance = "Engagement per entrance",
                          bounce_rate = "Bounce rate"
    )
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
                                          "Engagement per entrance",
                                          "Bounce rate")
  ))

# plot combine OL and Matomo
ggplot(metrics_ol_matomo,
       aes(x = reorder(module_title, value),
           y = value)) +
  
  geom_col(aes(fill = metric)) +
  coord_flip() +
  
  facet_wrap(~metric_label, scales = "free_x", nrow = 1) +
  
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  
  scale_fill_manual(values = c(
    "total_users" = "#7570B3",
    "unique_pageviews" = "#1B9E77",
    "engagement_per_entrance" = "#D95F02",
    "bounce_rate" = "#E6AB02"
  )) +
  
  labs(
    title = "User behaviour from Open Learnity and Matomo",
    x = "",
    y = "",
    fill = "",
    caption = "<b>Selected time period:</b> Jan-Mar 2026.<br>
       <b>Data source:</b> OpenLearnity and Matomo."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    plot.caption = element_markdown(size = 12, hjust = 1, lineheight = 1.3),
  )
