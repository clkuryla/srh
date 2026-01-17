# These functions describe and explore the relationship between self-rated health (SRH) and age over time.
# Fig. 1 in the SRH over age manuscript is generated using these functions.

# -------------------------------------------------------------------------------
# -------------- File: srh_and_age_over_time.R --------------
# -------------- Function List and Descriptions --------------
# Functions:
#
# summarize_srh_over_time.R 
#        Description: Summarize and plot self-rated health (SRH) over time by age group. 
#        Outputs: a list with 'estimates' data frame and 'plot' ggplot object.
# table_mean_srh.R
#        Description: Display SRH means table with DT.
# -------------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# -------------- Summarize SRH over time by age group --------------
# -------------------------------------------------------------------------------
# Summarize and plot self-rated health (SRH) over time by age group.
# Outputs a list with 'estimates' data frame and 'plot' ggplot object.

library(tidyverse)
library(srvyr)

summarize_srh_over_time <- function(
  data,
  survey_name, # Sstring: name of the survey for titles/labels
  age_group_var = "age_group", 
  srh_var = "srh",
  year_var = "year",
  psu_var = "psu",
  strata_var = "strata",
  wt_var = "wt",
  ci_level = 0.95,
  show_ci = FALSE,  # figure default: NO confidence intervals
  colors = c("#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#0072B2", "#CC79A7"),
  lonely_psu = "adjust",
  title_style = c("descriptive", "dataset")  # Title is just dataset title vs descriptive 
) {

  title_style <- match.arg(title_style)  # Validate input
  
  params <- list(
    survey_name = survey_name,
    age_group_var = age_group_var, srh_var = srh_var, year_var = year_var,
    psu_var = psu_var, strata_var = strata_var, wt_var = wt_var,
    ci_level = ci_level, show_ci = show_ci, colors = colors, lonely_psu = lonely_psu,
    title_style = title_style  # Store in params
  )

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = params$lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  if (inherits(data, "tbl_svy")) {
    vars_present <- dplyr::tbl_vars(data)
  } else if (inherits(data, c("survey.design", "svyrep.design"))) {
    vars_present <- names(data$variables)
  } else if (is.data.frame(data)) {
    vars_present <- names(data)
  } else {
    stop("`data` must be a data.frame, a survey.design/svyrep.design, or a srvyr tbl_svy.")
  }

  if (!params$age_group_var %in% vars_present) {
    candidates <- vars_present[str_detect(vars_present, "age_group")]
    stop(
      "`age_group_var` not found: ", params$age_group_var,
      if (length(candidates)) paste0("\nCandidates: ", paste(candidates, collapse = ", ")) else ""
    )
  }

  stopifnot(
    params$srh_var %in% vars_present,
    params$year_var %in% vars_present,
    params$age_group_var %in% vars_present,
    is.numeric(params$ci_level),
    params$ci_level > 0,
    params$ci_level < 1
  )

  if (is.data.frame(data)) {
    # Check which design elements are available
    has_psu <- !is.null(params$psu_var) && params$psu_var %in% vars_present
    has_strata <- !is.null(params$strata_var) && params$strata_var %in% vars_present
    has_wt <- params$wt_var %in% vars_present
    
    stopifnot(has_wt)  # Weights are required

    stopifnot(all(data[[params$year_var]][!is.na(data[[params$year_var]])] > 0))
    stopifnot(all(data[[params$wt_var]][!is.na(data[[params$wt_var]])] >= 0))
    stopifnot(all(data[[params$srh_var]][!is.na(data[[params$srh_var]])] >= 0))

    # Filter to valid weights before creating survey design
    data <- data %>%
      dplyr::filter(!is.na(.data[[params$wt_var]]), .data[[params$wt_var]] > 0)
    
    if (nrow(data) == 0) {
      rlang::abort(c(
        "No rows remain after filtering to valid weights.",
        "i" = paste0("Check `", params$wt_var, "` for all NA or non-positive values.")
      ))
    }

    # Build rename list based on available design elements
    rename_exprs <- list(
      .age_group = rlang::sym(params$age_group_var),
      .srh = rlang::sym(params$srh_var),
      .year = rlang::sym(params$year_var),
      .wt = rlang::sym(params$wt_var)
    )
    if (has_psu) rename_exprs$.psu <- rlang::sym(params$psu_var)
    if (has_strata) rename_exprs$.strata <- rlang::sym(params$strata_var)

    df_std <- data %>%
      rename(!!!rename_exprs) %>%
      filter(
        !is.na(.srh), is.finite(.srh),
        !is.na(.year), is.finite(.year),
        !is.na(.age_group),
        !is.na(.wt), is.finite(.wt), .wt > 0
      )
    
    # Additional filters for PSU/strata if present
    if (has_psu) df_std <- df_std %>% filter(!is.na(.psu))
    if (has_strata) df_std <- df_std %>% filter(!is.na(.strata))

    stopifnot(nrow(df_std) > 0)

    # Create survey design based on available elements
    if (has_psu && has_strata) {
      svy <- df_std %>%
        srvyr::as_survey_design(ids = .psu, strata = .strata, weights = .wt, nest = TRUE)
    } else if (has_psu) {
      message("Note: strata not available; using PSU + weights only design.")
      svy <- df_std %>%
        srvyr::as_survey_design(ids = .psu, weights = .wt)
    } else if (has_strata) {
      message("Note: PSU not available; using strata + weights only design.")
      svy <- df_std %>%
        srvyr::as_survey_design(ids = 1, strata = .strata, weights = .wt)
    } else {
      message("Note: PSU and strata not available; using weights-only design. SEs may be underestimated.")
      svy <- df_std %>%
        srvyr::as_survey_design(ids = 1, weights = .wt)
    }
  } else if (inherits(data, c("survey.design", "svyrep.design"))) {
    w <- survey::weights(data)
    stopifnot(all(w[!is.na(w)] >= 0))

    svy <- srvyr::as_survey(data) %>%
      mutate(
        .srh = .data[[params$srh_var]],
        .year = .data[[params$year_var]],
        .age_group = .data[[params$age_group_var]],
        .wt = srvyr::cur_svy_wts()
      ) %>%
      filter(
        !is.na(.srh), is.finite(.srh),
        !is.na(.year), is.finite(.year),
        !is.na(.age_group),
        !is.na(.wt), is.finite(.wt), .wt > 0
      )
  } else {
    svy <- data %>%
      mutate(
        .srh = .data[[params$srh_var]],
        .year = .data[[params$year_var]],
        .age_group = .data[[params$age_group_var]],
        .wt = srvyr::cur_svy_wts()
      ) %>%
      filter(
        !is.na(.srh), is.finite(.srh),
        !is.na(.year), is.finite(.year),
        !is.na(.age_group),
        !is.na(.wt), is.finite(.wt), .wt > 0
      )
  }

  checks <- svy %>%
    summarise(
      n = unweighted(n()),
      any_bad_srh = unweighted(any(.srh < 0, na.rm = TRUE)),
      any_bad_year = unweighted(any(.year <= 0, na.rm = TRUE)),
      any_bad_wt = unweighted(any(.wt <= 0, na.rm = TRUE)),
      .groups = "drop"
    )

  stopifnot(checks$n > 0, !checks$any_bad_srh, !checks$any_bad_year, !checks$any_bad_wt)

  estimates <- svy %>%
    mutate(.age_group = as.factor(.age_group)) %>%
    group_by(.age_group, .year) %>%
    summarise(
      mean_srh = survey_mean(.srh, vartype = c("se", "ci"), level = params$ci_level, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      age_group = .age_group,
      year = .year,
      mean_srh = mean_srh,
      se = mean_srh_se,
      ci_lower = mean_srh_low,
      ci_upper = mean_srh_upp
    ) %>%
    arrange(age_group, year)

  stopifnot(nrow(estimates) > 0)
  stopifnot(all(is.finite(estimates$mean_srh)))
  stopifnot(n_distinct(estimates$age_group) <= length(params$colors))

  # Create title based on title_style
  plot_title <- if (params$title_style == "descriptive") {
    paste0(params$survey_name, ": Weighted mean self-rated health by age group")
  } else {
    params$survey_name
  }

  p <- ggplot(estimates, aes(x = year, y = mean_srh, color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    scale_color_manual(values = params$colors) +
    labs(
      title = plot_title,  # Use conditional title
      x = "Survey year",
      y = "Weighted mean SRH",
      color = "Age group"
    ) +
    theme_minimal()

  if (isTRUE(params$show_ci)) {
    p <- p +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age_group), alpha = 0.15, color = NA) +
      scale_fill_manual(values = params$colors, guide = "none")
  }

  list(estimates = estimates, plot = p)
}

# Example (commented):
# out <- summarize_srh_over_time(data_nhis, survey_name = "NHIS", age_group_var = "age_group")
# out$estimates
# out$plot


## ---------------------------------------------------------------
# -------------- Display SRH means table with DT --------------
# ---------------------------------------------------------------
library(DT)

table_mean_srh <- function(df, caption = NULL) {

  datatable(
    df,
    rownames = FALSE,
    caption = if (!is.null(caption)) {
      htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        caption
      )
    } else {
      NULL
    },
    filter = "top",
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100, nrow(df)),
      scrollX = TRUE
    )
  )
}


# -------------------------------------------------------------------------------
# -------------- Chunked version for large datasets (e.g., BRFSS) --------------
# -------------------------------------------------------------------------------
# Process large datasets year-by-year AND age-group-by-age-group to avoid memory limits.
# Returns same structure as summarize_srh_over_time().

summarize_srh_over_time_chunked <- function(
  data,
  survey_name,
  age_group_var = "age_group",
  srh_var = "srh",
  year_var = "year",
  psu_var = "psu",
  strata_var = "strata",
  wt_var = "wt",
  ci_level = 0.95,
  show_ci = FALSE,
  colors = c("#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#0072B2", "#CC79A7"),
  lonely_psu = "adjust",
  title_style = c("descriptive", "dataset"),
  cache_dir = NULL,
  force_recompute = FALSE
) {
  
  title_style <- match.arg(title_style)
  
  # Optionally cache intermediate results
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    cache_file <- file.path(cache_dir, paste0(survey_name, "_srh_means.rds"))
    if (file.exists(cache_file) && !force_recompute) {
      message("Loading cached results from: ", cache_file)
      estimates <- readr::read_rds(cache_file)
      return(.build_srh_plot(estimates, survey_name, title_style, colors, show_ci))
    }
  }

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Get unique years and age groups

  years <- sort(unique(data[[year_var]]))
  age_groups <- sort(unique(data[[age_group_var]]))
  
  message("Processing ", length(years), " years x ", length(age_groups), 
          " age groups for ", survey_name, "...")

  # Check which design elements are available
  vars_present <- names(data)
  has_psu <- !is.null(psu_var) && psu_var %in% vars_present
  has_strata <- !is.null(strata_var) && strata_var %in% vars_present
  has_wt <- wt_var %in% vars_present
  stopifnot(has_wt)

  # Process each year x age_group combination separately
  results_list <- vector("list", length(years) * length(age_groups))
  idx <- 0
  
  for (i in seq_along(years)) {
    yr <- years[i]
    message("  Year ", yr, " (", i, "/", length(years), ")")
    
    for (j in seq_along(age_groups)) {
      ag <- age_groups[j]
      idx <- idx + 1
      
      # Subset to single year AND age group using base R for memory efficiency
      row_mask <- data[[year_var]] == yr & data[[age_group_var]] == ag
      data_chunk <- data[row_mask, , drop = FALSE]
      
      # Skip if no data
      if (nrow(data_chunk) == 0) {
        next
      }

      # Filter to valid observations using base R
      valid_mask <- !is.na(data_chunk[[srh_var]]) & 
                    is.finite(data_chunk[[srh_var]]) &
                    !is.na(data_chunk[[wt_var]]) & 
                    is.finite(data_chunk[[wt_var]]) & 
                    data_chunk[[wt_var]] > 0
      
      if (has_psu) valid_mask <- valid_mask & !is.na(data_chunk[[psu_var]])
      if (has_strata) valid_mask <- valid_mask & !is.na(data_chunk[[strata_var]])
      
      data_chunk <- data_chunk[valid_mask, , drop = FALSE]
      
      if (nrow(data_chunk) == 0) {
        next
      }

      # Create survey design based on available elements
      if (has_psu && has_strata) {
        svy <- survey::svydesign(
          ids = as.formula(paste0("~", psu_var)),
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk,
          nest = TRUE
        )
      } else if (has_psu) {
        svy <- survey::svydesign(
          ids = as.formula(paste0("~", psu_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk
        )
      } else if (has_strata) {
        svy <- survey::svydesign(
          ids = ~1,
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk
        )
      } else {
        svy <- survey::svydesign(
          ids = ~1,
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk
        )
      }

      # Compute mean for this year x age_group using survey package directly
      srh_formula <- as.formula(paste0("~", srh_var))
      mean_result <- survey::svymean(srh_formula, svy, na.rm = TRUE)
      ci_result <- confint(mean_result, level = ci_level)
      
      results_list[[idx]] <- data.frame(
        age_group = ag,
        year = yr,
        mean_srh = as.numeric(coef(mean_result)),
        se = as.numeric(survey::SE(mean_result)),
        ci_lower = ci_result[1],
        ci_upper = ci_result[2],
        stringsAsFactors = FALSE
      )

      # Aggressive cleanup
      rm(data_chunk, svy, mean_result, ci_result, valid_mask, row_mask)
    }
    
    # Force garbage collection after each year
    gc(verbose = FALSE)
  }

  # Combine all results
  estimates <- dplyr::bind_rows(results_list) %>%
    mutate(age_group = factor(age_group, levels = age_groups)) %>%
    arrange(age_group, year)

  stopifnot(nrow(estimates) > 0)

  # Cache if requested
  if (!is.null(cache_dir)) {
    readr::write_rds(estimates, cache_file)
    message("Cached results to: ", cache_file)
  }

  .build_srh_plot(estimates, survey_name, title_style, colors, show_ci)
}

# Helper function to build the plot (shared by both functions)
.build_srh_plot <- function(estimates, survey_name, title_style, colors, show_ci) {
  
  plot_title <- if (title_style == "descriptive") {
    paste0(survey_name, ": Weighted mean self-rated health by age group")
  } else {
    survey_name
  }

  p <- ggplot(estimates, aes(x = year, y = mean_srh, color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    scale_color_manual(values = colors) +
    labs(
      title = plot_title,
      x = "Survey year",
      y = "Weighted mean SRH",
      color = "Age group"
    ) +
    theme_minimal()

  if (isTRUE(show_ci)) {
    p <- p +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age_group), alpha = 0.15, color = NA) +
      scale_fill_manual(values = colors, guide = "none")
  }

  list(estimates = estimates, plot = p)
}
