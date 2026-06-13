# This script processes data from Google Analytics (ORD Portal), calculates key metrics, and generates corresponding visualizations.

library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)

# data for this script is manually downloaded from the Google Analytics platform quaterly.

# Direct measures: page_title, views, active_users  

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
  filter(page_title %in% ord_pages) |>
  mutate(page_title = recode(page_title,
                             "Training Archive" = "Training",
                             "Documents Archive" = "Documents",
                             "Research Data Services Explorer" = "Services",
                             "Open Research Data Portal" = "Home")) |>
  mutate(page_title = factor(page_title,
                             levels = c(
                               "Home",
                               "Projects",
                               "Training",
                               "Services",
                               "Documents",
                               "About"
                             )))

# views = intensity of use per page
# active_users = reach per page

# calculate and select variables to plot ----
df_ga_metrics <- google_analytics_jan_mar_2026 |>
  select(
    page_title,
    views,
    active_users)

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

fig3 <- ggplot(metrics_long_ga, 
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
    y = "Metric",
    x = "ORD portal pages",
    caption = "<b>Selected time period:</b> Jan-Mar 2026.<br>
       <b>Data source:</b> Google Analytics.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.caption = element_markdown(size = 12, hjust = 1, lineheight = 1.3)
  )

fig3