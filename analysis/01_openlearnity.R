# This script loads data on enrollment from Open Learnity, calculates key metrics, and generates corresponding visualizations.

library(dplyr)
library(tidyverse)
library(lubridate)
library(here)

# data for this script is downloaded using the 00_openlearnity_download.R script

# load data
folder_path <- here::here("data/openlearnity")

files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)

df_openlearnity <- files |>
  map_dfr(~ read_csv(.x, show_col_types = FALSE) |>
            mutate(
              file_name = basename(.x),
              module = str_extract(basename(.x), "^[^_]+")  # extract prefix before _
            ))

# ensure correct date format
df_openlearnity <- df_openlearnity |>
  mutate(last_login = as.Date(last_login),
         date_joined = as.Date(date_joined)) 

# remove duplicate user_id + last_login combinations
df_openlearnity <- df_openlearnity |>
  distinct(username, name, last_login, module,.keep_all = T) |>
  group_by(module) |>
  arrange(module, desc(last_login)) |>
  select(id, username, name, email, last_login, date_joined, module) |>
  mutate(module_label = recode(module,
                               dmp = "Writing Data Management Plans",
                               documentation = "Data Documentation and Metadata",
                               fair = "Open Research Data, Research Data Management, and FAIR",
                               licences = "Data and Code Licenses",
                               openformats = "Open Formats and Code",
                               organisation = "Data Organization and Management",
                               publishing = "Data Publishing and Long-Term Preservation",
                               reproducibility = "Reproducibility and Code Management",
                               sensitive = "Sensitive Data",
                               storage = "Data Storage, Backup and Versioning"
  )) |>
  ungroup()

# logged users per week ----

# create weekly aggregation
df_openlearnity_weekly_activity <- df_openlearnity |>
  mutate(week = floor_date(last_login, unit = "week", week_start = 1)) |>
  group_by(module, week) |>
  summarise(n_users = n(), .groups = "drop") |>
  arrange(module, week) |>
  ungroup()

# define campaign start
campaign_dates <- tibble(
  module = c("dmp", "documentation", "fair", "licences", "openformats", "organisation", "publishing", "reproducibility", "sensitive", "storage"),
  campaign_start = as.Date(c("2026-03-23", "", "2026-02-23", "", "2026-04-14", "", "", "", "", ""))
)

# plot
weekly_module_activity_plot <- ggplot(df_openlearnity_weekly_activity |>
              filter(week >= as.Date("2026-01-01")), 
            aes(x = week, y = n_users)) +
  geom_col(fill = "#2C7BB6", alpha = 0.8) +
  geom_vline(
    data = campaign_dates,
    aes(xintercept = campaign_start, color = "ETH campaign"),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%b\n%Y"
  ) +
  facet_wrap(~ module, scales = "free_y", nrow = 2, ncol = 5,
             labeller = labeller(module = c(
               "dmp" = "Writing Data \nManagement Plans",
               "documentation" = "Data Documentation \nand Metadata",
               "fair" = "Open Research Data, \nRDM and FAIR",
               "licences" = "Data and \nCode Licences",
               "openformats" = "Open Formats \nand Code",
               "organisation" = "Data Organization \nand Management",
               "publishing" = "Data Publishing and \nLong-Term Preservation",
               "reproducibility" = "Reproducibility and \nCode Management",
               "sensitive" = "Sensitive Data",
               "storage" = "Data Storage, \nBackup and Versioning"
             ))) +
  scale_color_manual(name = "", values = c("ETH campaign" = "red")) + 
  labs(title = "Number of users per week",
       x = "Week",
       y = "Number of users") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

weekly_module_activity_plot

ggsave(
  here::here("figures", "weekly_users_by_module_2026.png"),
  plot = weekly_module_activity_plot,
  width = 16,
  height = 9,
  dpi = 300
)

# Number of users per module ----

# Select the time period: 
# Jan-Mar 2026

df_openlearnity_users <- df_openlearnity |>
  filter(last_login >= as.Date("2026-01-01") &
           last_login <= as.Date("2026-03-31")) |>
  distinct(username, name, module,.keep_all = T) |>
  group_by(module, module_label) |>
  summarise(total_users = n()) |>
  ungroup()
    
# Overall metrics ----

# How long did users stay active?
# user_lifetime: Duration between first and last observed activity within a module (proxy of engagement duration).
# Calculated as: last_login - first_login (not date_joined).

# Overall metrics:

# How many users does each module have (since the start)?
# total_users: nnumber of unique users per module since the beginning.
# Calculated as n() (count of users per module).

# How many users only use the module once?
# one_time_users: Proportion of users who only engaged with a module once (no return activity).
# Calculated as: mean(user_lifetime == 0) or equivalently: sum(user_lifetime == 0) / n().

# How long do returning users stay active (median in days)?
# median_lifetime_returning: Median number of days between first and last activity for users who returned at least once.
# Calculated as: median(user_lifetime[user_lifetime > 0], na.rm = TRUE).

# Select the time period: Jan-Mar 2026

# calculate overall metrics
df_openlearnity_user <- df_openlearnity |>
  filter(last_login >= as.Date("2026-01-01") &
           last_login <= as.Date("2026-03-31"))  |>
  group_by(username, module, module_label) |>
  summarise(
    first_login = min(last_login, na.rm = TRUE),
    last_login  = max(last_login, na.rm = TRUE),
    user_lifetime = as.numeric(last_login - first_login),
    .groups = "drop"
  )

df_openlearnity_agg <- df_openlearnity_user |>
  group_by(module, module_label) |>
  summarise(
    total_users = n(),
    # how many users only interact once
    one_time_users = mean(user_lifetime == 0),
    # among users who returned, how long they stay
    median_lifetime_returning = median(user_lifetime[user_lifetime > 0], na.rm = TRUE),
    
    .groups = "drop"
  )

# calculate overall metrics
metrics_long <- df_openlearnity_agg |>
  pivot_longer(
    cols = c(total_users, one_time_users, median_lifetime_returning),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = factor(
      metric,
      levels = c("total_users", "one_time_users", "median_lifetime_returning")
    ),
    
    metric_label = recode(
      metric,
      total_users = "Number of users per module\n(since start)",
      one_time_users = "Proportion of one-time users\nper module",
      median_lifetime_returning = "Number of days users stay active\n(median, returning users)"
    )
  )

# Plot all panels
ggplot(metrics_long, aes(x = reorder(module_label, value), y = value)) +
  geom_col(aes(fill = metric)) +
  coord_flip() +
  facet_wrap(~metric_label, scales = "free_x") +
  scale_fill_manual(values = c(
    "total_users" = "#7570B3",
    "one_time_users" = "#D95F02",
    "median_lifetime_returning" = "#1B9E77"
  )) +
  labs(
    title = "User behaviour/engagement in Open Learnity",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")
