# This script processes data from Google Analytics (ORD Portal), calculates key metrics, and generates corresponding visualizations.

library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)

# load data ----
google_analytics_20260101_20260331 <- read.csv(here("data", "google_analytics", "google_analytics_20260101_20260331.csv"))

ord_pages <- c(
  "Training Archive",
  "Open Research Data Portal",
  "Research Data Services Explorer",
  "Projects",
  "About",
  "Documents Archive"
)

google_analytics_jan_mar_2026 <- google_analytics_20260101_20260331 |>
  rename(page_title = Page.title.and.screen.class,
         views = Views,
         active_users = Active.users,
         views_per_user = Views.per.active.user,
         average_time_per_user = Average.engagement.time.per.active.user,
         event_count = Event.count,
         key_events = Key.events,
         total_revenue = Total.revenue) |>
  mutate(page_title = str_remove(page_title, " \\| Open Research Data Portal")) |>
  filter(page_title %in% ord_pages)


# views = intensity of use per page
# active_users = reach per page

# calculate and select variables to plot ----
df_ga_metrics <- google_analytics_jan_mar_2026 |>
  select(
    page_title,
    views,
    active_users,
    views_per_user)

# buble chart
ggplot(df_ga_metrics, aes(x = active_users,
               y = views,
               size = views_per_user,
               label = page_title)) +
  geom_point(alpha = 0.6) +
  geom_text(vjust = -0.8, size = 3) +
  scale_size_continuous(range = c(3, 12)) +
  theme_minimal() +
  labs(x = "Active Users",
       y = "Views",
       size = "Views per User",
       title = "Page Engagement: Reach vs Depth")

# reshape for plotting ----
metrics_long_ga <- pivot_longer(df_ga_metrics,
                        cols = c(views, active_users),
                        names_to = "metric",
                        values_to = "value") |>
  mutate(
    metric = factor(metric,
                    levels = c("active_users",
                               "views")),
    
    metric_label = recode(metric,
                          active_users = "Active users",
                          views = "Views"
    )
  )

# plot ----
ggplot(metrics_long_ga, 
       aes(x = reorder(page_title, value),
          y = value,
          fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_discrete(labels = c(
    views = "Views",
    active_users = "Active users"
  )) +
  labs(
    title = "Page views and active users for the ORD pages in Google Analytics",
    y = "Count",
    x = "ORD portal main pages",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )

# Training Archive → highest views (1000) but fewer users (374) -> very deep engagement per user (strong content usage)
