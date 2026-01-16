library(dplyr)
library(rlang)

#' Add age-in-reference-year and vital-status variables
#'
#' @param data A data frame containing the relevant columns.
#' @param reference_years A numeric vector of years for which you want 
#'   age_in_X_or_death and vital_status_X columns.
#' @param threshold A numeric threshold for the difference 
#'   (MORTDODY - year_var), default is 15.
#' @param year_var Name of the variable in `data` representing the 
#'   baseline year used in the threshold check (string).
#' @param death_year_var Name of the variable in `data` representing 
#'   the year of death (string).
#' @param baseline_year_var Name of the variable in `data` representing 
#'   the baseline year at which age was measured (string).
#' @param age_var Name of the variable in `data` representing 
#'   baseline age (string).
#'
#' @return The original data frame with two new columns per reference year:
#'   - age_in_{reference_year}_or_death
#'   - vital_status_{reference_year}
#'
#' @details
#' For each reference year in `reference_years`, this function adds:
#' \itemize{
#'   \item `age_in_{reference_year}_or_death`: 
#'        - If death occurred within `threshold` years since `year_var`, 
#'          then it's the age at death (i.e., `(MORTDODY - baseline_year_var) + AGE`). 
#'        - Otherwise, it's age in the reference year 
#'          (i.e., `reference_year - baseline_year_var - AGE`, following your original formula).
#'   \item `vital_status_{reference_year}`: 
#'        - `1` if `MORTDODY - year_var <= threshold`, 
#'        - `0` otherwise.
#' }
#'
#' This reproduces the logic from your example, but parameterizes the 
#' reference years and the threshold.
#'
add_age_and_vital_status <- function(data,
                                     reference_years,
                                     threshold = 15,
                                     year_var = "year",
                                     death_year_var = "MORTDODY",
                                     baseline_year_var = "YEAR",
                                     age_var = "AGE") {
  
  # Make sure variables exist
  required_vars <- c(year_var, death_year_var, baseline_year_var, age_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("The following required variables are missing from 'data':",
               paste(missing_vars, collapse = ", ")))
  }
  
  # Loop over each requested reference_year and add 2 columns
  result <- data
  for (ref_year in reference_years) {
    
    # Construct new column names programmatically
    age_col <- paste0("age_in_", ref_year, "_or_death")
    vital_col <- paste0("vital_status_", ref_year)
    
    result <- result %>%
      mutate(
        # age_in_{ref_year}_or_death
        !!sym(age_col) := case_when(
          .data[[death_year_var]] - .data[[year_var]] <= threshold ~ 
            .data[[death_year_var]] - .data[[baseline_year_var]] + .data[[age_var]],
          TRUE ~ ref_year - .data[[baseline_year_var]] - .data[[age_var]]
        ),
        # vital_status_{ref_year}
        !!sym(vital_col) := case_when(
          .data[[death_year_var]] - .data[[year_var]] <= threshold ~ 1,
          TRUE ~ 0
        )
      )
  }
  
  return(result)
}

######################

data_nhis_mort_filter_more <- data_nhis_mort_filter %>%
  add_age_and_vital_status(reference_years = 1986:2017,
                           threshold = 15,
                           year_var = "year",
                           death_year_var = "MORTDODY",
                           baseline_year_var = "YEAR",
                           age_var = "AGE")

data_nhis_mort_filter_more

#######################


