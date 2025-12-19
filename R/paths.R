library(here)

data_root <- function() {
  dr <- Sys.getenv("DATA_DEPOT", unset = NA_character_)
  if (!is.na(dr) && nzchar(dr) && dir.exists(dr)) return(dr)
  stop("DATA_DEPOT not set. See README.")
}

depot_path   <- function(...) file.path(data_root(), ...)
derived_path <- function(...) file.path(data_root(), "_derived", "srh_project", ...)

ensure_dirs <- function() {
  dir.create(derived_path(), recursive = TRUE, showWarnings = FALSE)
  dir.create(here::here("outputs"), recursive = TRUE, showWarnings = FALSE)
}
