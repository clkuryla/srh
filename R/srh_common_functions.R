# This script is to source for common needs throughout the SRH analysis project such as:
#    Formatting (tables, etc)
#    Colors for consistency across plots
#    Themes for consistency across ggplots
#    Common functions for sensitivity analyses
#    Other functions that are used in multiple places



##########################
######### Formatting #########
##########################

# # Conditional table output based on format
# if (knitr::is_html_output()) {
#   library(DT)
  
#   datatable(
#     df,
#     rownames = FALSE,
#     caption = if (!is.null(caption)) {
#       htmltools::tags$caption(
#         style = 'caption-side: bottom; text-align: left;',
#         caption
#       )
#     } else {
#       NULL
#     },
#     filter = "top",
#     options = list(
#       pageLength = 10,
#       lengthMenu = c(10, 25, 50, 100, nrow(df)),
#       scrollX = TRUE
#     )
#   )

# } else {
#   knitr::kable(
#     df,
#     caption = "Table. Self-rated health by age group and year (NHIS)")
# }


#' Render a table with format-aware output (DT for HTML, kable otherwise)
#'
#' @param df A data frame to display.
#' @param caption Optional caption string.
#' @return A DT datatable (HTML) or kable (other formats).
table_render <- function(df, caption = NULL) {
  stopifnot(is.data.frame(df))
  
  n <- nrow(df)
  length_menu <- c(10)
  if (n >= 25) length_menu <- c(length_menu, 25)
  if (n >= 50) length_menu <- c(length_menu, 50)
  if (n >= 100) length_menu <- c(length_menu, 100)
  length_menu <- c(length_menu, n)
  
  if (knitr::is_html_output()) {
    DT::datatable(
      df,
      rownames = FALSE,
      caption = if (!is.null(caption)) {
        htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          caption
        )
      } else {
        NULL
      },
      filter = "top",
      options = list(
        pageLength = 10,
        lengthMenu = length_menu,
        scrollX = TRUE
      )
    )
  } else {
    knitr::kable(df, caption = caption)
  }
}



##########################
######### Colors #########
##########################

# Okabe-Ito colorblind-friendly palette
std_colors <- c(      
  "#56B4E9", # sky blue
  "#CC79A7",  # reddish purple
  "#009E73", # bluish green
  "#E69F00", # orange
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00" # vermillion
)

# Okabe-Ito colorblind-friendly palette in rainbow order for age groups (youngest = vermillion, oldest = reddish purple)
rainbow_order <- c(        
  "#D55E00", # vermillion
  "#E69F00", # orange
  "#F0E442", # yellow
  "#009E73", # bluish green
  "#56B4E9", # sky blue
  "#0072B2", # blue
  "#CC79A7"  # reddish purple
)

mental_health_colors <- c(
  "#CC79A7",  # reddish purple
  "#E69F00", # orange  
  "#D55E00", # vermillion
  "#999999", # grey
  "#000000",  # black
  "#F0E442", # yellow 
  "#009E73" # bluish green 
)

physical_health_colors <- c(
  "#56B4E9", # sky blue
  "#0072B2", # blue
  "#999999", # grey
  "#000000",  # black
  "#009E73", # bluish green
  "#F0E442", # yellow 
  "#D55E00" # vermillion
)

overall_health_colors <- c(
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#999999", # grey
  "#000000",  # black
  "#CC79A7",  # reddish purple 
  "#56B4E9", # sky blue
  "#E69F00", # orange  
  "#D55E00", # vermillion
  "#F0E442" # yellow 
)

#################################
######### GGplot Themes #########
#################################

##################################################
######### Sensitivity Analysis Functions #########
##################################################

##########################################
########## Age Binning Function ##########

library(dplyr)
library(rlang)

#' Add an age_group variable from an age variable, using one of 3 binning schemes.
#'
#' @param data A data frame (or tibble).
#' @param age_var Name of the age variable (unquoted), default: age
#' @param scheme One of: "A", "B", "C"
#'   - "A": 18-25, 26-35, 36-45, 46-55, 56-65, 66-75, 76-85
#'   - "B": 18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80-89
#'   - "C": 18-29, 30-39, 40-49, 50-59, 60-69, 70+
#' @param new_var Name of the new variable to create (unquoted), default: age_group
#'
#' @return `data` with a new factor column `age_group` (or `new_var`).
#' @examples
#' df2 <- add_age_group(df, scheme = "B")
#' df2 <- add_age_group(df, age_var = AGE, scheme = "C", new_var = age_band)
add_age_group <- function(data,
                          age_var = age,
                          scheme = c("A", "B", "C"),
                          new_var = age_group) {
  scheme <- match.arg(scheme)

  age_q <- enquo(age_var)
  new_q <- enquo(new_var)

  # Coerce to numeric safely (handles factors/characters). Non-numeric -> NA.
  age_num <- suppressWarnings(as.numeric(as.character(dplyr::pull(data, !!age_q))))

  if (!any(!is.na(age_num))) {
    stop("`age_var` has no non-missing numeric values after coercion. Check the input type/content.")
  }

  if (scheme == "A") {
    labels <- c("18-25","26-35","36-45","46-55","56-65","66-75","76-85")
    grp <- dplyr::case_when(
      age_num >= 18 & age_num <= 25 ~ "18-25",
      age_num >= 26 & age_num <= 35 ~ "26-35",
      age_num >= 36 & age_num <= 45 ~ "36-45",
      age_num >= 46 & age_num <= 55 ~ "46-55",
      age_num >= 56 & age_num <= 65 ~ "56-65",
      age_num >= 66 & age_num <= 75 ~ "66-75",
      age_num >= 76 & age_num <= 85 ~ "76-85",
      TRUE ~ NA_character_
    )
  } else if (scheme == "B") {
    labels <- c("18-29","30-39","40-49","50-59","60-69","70-79","80-89")
    grp <- dplyr::case_when(
      age_num >= 18 & age_num <= 29 ~ "18-29",
      age_num >= 30 & age_num <= 39 ~ "30-39",
      age_num >= 40 & age_num <= 49 ~ "40-49",
      age_num >= 50 & age_num <= 59 ~ "50-59",
      age_num >= 60 & age_num <= 69 ~ "60-69",
      age_num >= 70 & age_num <= 79 ~ "70-79",
      age_num >= 80 & age_num <= 89 ~ "80-89",
      TRUE ~ NA_character_
    )
  } else { # scheme == "C"
    labels <- c("18-29","30-39","40-49","50-59","60-69","70+")
    grp <- dplyr::case_when(
      age_num >= 18 & age_num <= 29 ~ "18-29",
      age_num >= 30 & age_num <= 39 ~ "30-39",
      age_num >= 40 & age_num <= 49 ~ "40-49",
      age_num >= 50 & age_num <= 59 ~ "50-59",
      age_num >= 60 & age_num <= 69 ~ "60-69",
      age_num >= 70                ~ "70+",
      TRUE ~ NA_character_
    )
  }

  data %>%
    mutate(
      !!new_q := factor(grp, levels = labels)
    )
}

# --- Optional: tiny helper for sanity checking the result ---
check_age_group <- function(data, age_var = age, age_group_var = age_group) {
  age_q <- enquo(age_var)
  grp_q <- enquo(age_group_var)

  data %>%
    summarise(
      n = n(),
      n_missing_age = sum(is.na(!!age_q)),
      n_missing_age_group = sum(is.na(!!grp_q)),
      min_age = suppressWarnings(min(as.numeric(as.character(!!age_q)), na.rm = TRUE)),
      max_age = suppressWarnings(max(as.numeric(as.character(!!age_q)), na.rm = TRUE))
    )
}

# --- Examples ---
# df <- add_age_group(df, scheme = "A")
# df <- add_age_group(df, age_var = AGE, scheme = "C")
# df <- add_age_group(df, scheme = "B", new_var = age_band)
# check_age_group(df)



