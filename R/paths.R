# Paths helper

# This function enables us to store large files in a separate data depot
# This function assumes an environment variable DATA_DEPOT and SRH_DATA that points to the root of the data depot (see comments below the code)
# Alternatively, you can just set the data_root() function to a hard-coded path on your system

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

###### How to use this function/process #####
#
# Set up the data depot (once)
# Outside the repo, create:
#   
#   ~/data_depot/
#     surveys/
#       MEPS/
#       NHIS/
#       BRFSS/
#       CPS/
#.      GSS
#       NHANES
#     _derived/
#       srh_ project/
#   
#   
#  Then set the DATA_DEPOT environment variable:
#      install.packages("usethis")
#.     usethis::edit_r_environ()
#   Then add this to ~/.Renviron:
#      SRH_DATA=/Users/yourusername/data_depot/surveys
#      DATA_DEPOT=/Users/yourusername/data_depot
#.  Restart RStudio
#   Test that it works:
#      Sys.getenv("DATA_DEPOT")
#      dir.exists(Sys.getenv("DATA_DEPOT"))
#