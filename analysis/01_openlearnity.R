# This script loads data on enrollment from Open Learnity, calculates key metrics, and generates corresponding visualizations.

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(here)

# data for this script is downloaded using the 00_openlearnity_download.R script

# Direct measures: user_id, module_title, last_login
# Derived variables: logged_users, logged_users_week

# load data
folder_path <- here::here("data/openlearnity")

# List all CSV files except id.csv
files <- list.files(
  path = folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Exclude files containing "id.csv"
files <- files[!grepl("id\\.csv", basename(files))]

# keep only the columns id, last_login, date_joined
wanted_cols <- c("id", "last_login", "date_joined")

# Process each file
for (file in files) {
  
  cat("Processing:", basename(file), "\n")
  
  # Read CSV
  df <- read_csv(file, show_col_types = FALSE)
  
  # Keep only selected columns
  df_selected <- df %>%
    select(any_of(wanted_cols))
  
  # Save back with the same name
  write_csv(df_selected, file)
}

# join datasets
df_openlearnity <- files |>
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"), show_col_types = FALSE) |>
            mutate(
              file_name = basename(.x),
              module = str_extract(basename(.x), "^[^_]+"),
              
              # robust parsing (handles your mixed formats)
              last_login = lubridate::parse_date_time(
                last_login,
                orders = c(
                  "ymd HMS", "ymd HM", "ymd",
                  "dmy HMS", "dmy HM",
                  "mdy HMS", "mdy HM"
                ),
                tz = "UTC"
              ),
              
              date_joined = lubridate::parse_date_time(
                date_joined,
                orders = c(
                  "ymd HMS", "ymd HM", "ymd",
                  "dmy HMS", "dmy HM",
                  "mdy HMS", "mdy HM"
                ),
                tz = "UTC"
              )
            )) |>
  
  # convert both to ISO format
  mutate(
    last_login = format(last_login, "%Y-%m-%dT%H:%M:%SZ"),
    date_joined = format(date_joined, "%Y-%m-%dT%H:%M:%SZ")
  )

# ensure correct date format
df_openlearnity <- df_openlearnity |>
  mutate(last_login = as.Date(last_login),
         date_joined = as.Date(date_joined)) 

# remove duplicate user_id + last_login combinations
df_openlearnity <- df_openlearnity |>
  distinct(id, last_login, module,.keep_all = T) |>
  group_by(module) |>
  arrange(module, desc(last_login)) |>
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

fig1 <- ggplot(df_openlearnity_weekly_activity |>
                 filter(week >= as.Date("2026-01-01")), 
               aes(x = week, y = n_users)) +
  geom_col(fill = "#2C7BB6", alpha = 0.8) +
  geom_vline(
    data = campaign_dates,
    aes(xintercept = campaign_start, color = "Marketing campaign"),
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
  scale_color_manual(name = "", values = c("Marketing campaign" = "red")) + 
  labs(x = "Week",
       y = "Number of users",
       caption = "<b>Selected time period:</b> Jan 2026 up to now.<br>
       <b>Data source:</b> OpenLearnity.") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 10),
    #plot.caption = element_text(hjust = 0, face = "bold", size = 12)
    plot.caption = element_markdown(size = 12, hjust = 1, lineheight = 1.3)
  )

fig1 

ggsave(
  here::here("figures", "weekly_users_by_module_2026.png"),
  plot = fig1,
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
  distinct(id, module,.keep_all = T) |>
  group_by(module, module_label) |>
  summarise(total_users = n()) |>
  ungroup()
