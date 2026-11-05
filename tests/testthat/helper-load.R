library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

for (f in c("db.R", "read.R", "derive.R", "validate.R")) source(here::here("R", f))

seed_path <- here::here("data", "seed.json")
read_seed <- function() jsonlite::fromJSON(seed_path, simplifyVector = FALSE)

# Fresh in-memory database loaded from `seed` (path or list).
new_db <- function(seed = seed_path) {
  con <- db_connect(":memory:")
  import_seed(con, seed)
  con
}

project_idx <- function(seed, acronym) which(map_chr(seed$projects, "acronym") == acronym)
