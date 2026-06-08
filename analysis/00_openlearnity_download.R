# This script downloads enrollment data from OpenLearnity.
# It requires authentication to access the OpenLearnity data portal.
# Login is performed manually in a browser session (Chromote).
# No username/password is stored or passed in the code.

library(chromote)
library(stringr)
library(readr)

# Define modules ----
modules <- c(
  "module-v1:data+management+plan",
  "module-v1:ETH+sensitive+data",
  "module-v1:open+fair+data",
  "module-v1:open+formats+code",
  "module-v1:storage+backup+versioning",
  "module-v1:ETH+RDM-01+2024",
  "module-v1:ETH+data+licences",
  "module-v1:ETH+code+reproducibility",
  "module-v1:ETH+documentation+metadata",
  "module-v1:open+data+organisation"
)

# Map module IDs → filenames
module_names <- c(
  "module-v1:data+management+plan" = "dmp",
  "module-v1:ETH+sensitive+data" = "sensitive_data",
  "module-v1:open+fair+data" = "fair_data",
  "module-v1:open+formats+code" = "openformats_code",
  "module-v1:storage+backup+versioning" = "storage_backup_versioning",
  "module-v1:ETH+RDM-01+2024" = "publishing",
  "module-v1:ETH+data+licences" = "licences",
  "module-v1:ETH+code+reproducibility" = "reproducibility",
  "module-v1:ETH+documentation+metadata" = "documentation_metadata",
  "module-v1:open+data+organisation" = "organisation"
)

# output folder ----
out_dir <- "data/openlearnity"
#dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# start browser login and authentification ----
b <- ChromoteSession$new()
b$view()

base_url <- "https://courses.openlearnity.org"

b$Page$navigate(paste0(base_url, "/login"))

cat("👉 Log in manually in the browser\n")
readline("Press ENTER after login is complete...")

Sys.sleep(5)

# function ----
get_module <- function(module_id) {
  
  cat("\n====================\n")
  cat("Processing:", module_id, "\n")
  
  # Go to module
  b$Page$navigate(paste0(base_url, "/modules/", module_id, "/module/"))
  Sys.sleep(8)
  
  # Instructor
  b$Runtime$evaluate("
  (() => {
    const el = [...document.querySelectorAll('a,button')]
      .find(e => e.innerText && /instructor/i.test(e.innerText));
    if (el) el.click();
  })()
  ")
  
  Sys.sleep(10)
  
  # Data Downloads tab
  b$Runtime$evaluate("
  (() => {
    const el = [...document.querySelectorAll('a,button,li,div')]
      .find(e =>
        e.innerText &&
        e.innerText.toLowerCase().includes('data') &&
        e.innerText.toLowerCase().includes('download')
      );
    if (el) el.click();
  })()
  ")
  
  Sys.sleep(10)
  
  # Trigger report
  b$Runtime$evaluate("
  (() => {
    const el = [...document.querySelectorAll('a,button')]
      .find(e =>
        e.innerText &&
        /generate|report|export|download/i.test(e.innerText)
      );
    if (el) el.click();
  })()
  ")
  
  # generate csv
  csv_url <- NULL
  
  for (i in 1:10) {
    
    Sys.sleep(8)
    
    html <- b$Runtime$evaluate("document.body.innerHTML")$result$value
    
    urls <- str_extract_all(
      html,
      "https://[^\"']*(report|download)[^\"']*\\.csv[^\"']*"
    )[[1]]
    
    urls <- unique(urls)
    
    if (length(urls) > 0) {
      csv_url <- urls[1]
      cat("✅ CSV found\n")
      break
    }
    
    cat("⏳ waiting...\n")
  }
  
  if (is.null(csv_url)) {
    warning(paste("❌ No CSV for", module_id))
    return(NULL)
  }
  
  # download data
  
  df <- read_csv(csv_url)
  
  clean_name <- module_names[module_id]
  
  file_path <- file.path(
    out_dir,
    paste0(
      clean_name,
      "_",
      format(Sys.time(), "%Y-%m-%d-%H%M"),
      ".csv"
    )
  )
  
  write.csv(df, file_path, row.names = FALSE)
  
  cat("💾 Saved:", file_path, "\n")
  
  return(df)
}

# download data for all modules ----
results <- lapply(modules, get_module)
names(results) <- module_names[modules]
