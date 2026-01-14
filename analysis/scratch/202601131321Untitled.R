# ============================================================
# Packages
# ============================================================
library(tidyverse)
library(srvyr)
library(survey)
library(rlang)
library(stringr)

# ============================================================
# 1) Okabe–Ito palette in "rainbow order" (your specification)
#    Youngest -> Oldest:
#      reddishpurple, orange, yellow, bluishgreen, skyblue, blue,
#      then add grey (7th), then black (8th)
# ============================================================
okabe_ito_rainbow <- function(n) {
  # Okabe–Ito core colors (standard hex codes)
  oi <- c(
    skyblue       = "#56B4E9",
    reddishpurple = "#CC79A7",
    bluishgreen   = "#009E73",
    orange        = "#E69F00",
    blue          = "#0072B2",
    yellow        = "#F0E442",
    vermilion     = "#D55E00",
    black         = "#000000",
    purple = "mediumpurple"  # extra color (not in Okabe–Ito), if needed
  )
  

  
  # Your requested ordering (note: vermilion intentionally unused here)
  base_order <- c("reddishpurple", "orange", "yellow", "bluishgreen", "skyblue", "blue")
  seven_order <-   c("vermilion", "orange", "yellow", "bluishgreen", "skyblue", "blue", "reddishpurple")
  grey_extra <- "#999999"  # common neutral add-on; colorblind-safe
  
  if (n <= length(base_order)) {
    return(unname(oi[base_order[seq_len(n)]]))
  }
  if (n == 7) {
   return(unname(c(oi[base_order], oi["purple"])))
   # return(seven_order)
   # return(unname(c(oi[base_order], grey_extra)))
  }
  if (n == 8) {
    return(unname(c(oi[base_order], grey_extra, oi["black"])))
  }
  
  # Fallback if someone ever uses >8 groups:
  warning("Requested >8 groups; using Okabe–Ito rainbow for first 8, then a fallback qualitative palette for extras.")
  extra <- colorspace::qualitative_hcl(n - 8, palette = "Dark 3")
  return(c(unname(c(oi[base_order], grey_extra, oi["black"])), extra))
}

# ============================================================
# 2) Helper: order age-group labels from youngest -> oldest
#    Assumes labels like "18-29", "70+", "70-79", etc.
# ============================================================
order_age_groups <- function(x, explicit_levels = NULL) {
  x_chr <- as.character(x)
  
  if (!is.null(explicit_levels)) {
    return(factor(x_chr, levels = explicit_levels, ordered = TRUE))
  }
  
  levs <- if (is.factor(x)) levels(x) else sort(unique(x_chr))
  
  starts <- suppressWarnings(as.numeric(str_extract(levs, "^\\d+")))
  if (all(!is.na(starts))) {
    levs <- levs[order(starts)]
  }
  
  factor(x_chr, levels = levs, ordered = TRUE)
}

# ============================================================
# 3) Helper: Convert input (raw df OR survey design OR tbl_svy)
#    into a srvyr tbl_svy object.
# ============================================================
as_tbl_svy_flex <- function(data,
                            weight_var = "wt",
                            psu_var    = "psu",
                            strata_var = "strata",
                            nest       = TRUE,
                            verbose    = TRUE) {
  
  # Already srvyr design?
  if (inherits(data, "tbl_svy")) return(data)
  
  # A survey.design object?
  if (inherits(data, "survey.design") || inherits(data, "survey.design2") || inherits(data, "svyrep.design")) {
    return(srvyr::as_survey(data))
  }
  
  # Otherwise assume a raw data.frame / tibble
  df <- as_tibble(data)
  
  if (!weight_var %in% names(df)) {
    stop(glue::glue("weight_var='{weight_var}' not found in data. Please supply the correct weight variable name."))
  }
  
  has_psu    <- psu_var    %in% names(df)
  has_strata <- strata_var %in% names(df)
  
  if (verbose && !has_psu) {
    message(glue::glue("PSU var '{psu_var}' not found. Building a weighted design with ids = 1 (no clustering)."))
  }
  if (verbose && !has_strata) {
    message(glue::glue("Strata var '{strata_var}' not found. Building a design without stratification."))
  }
  
  # Build design; use ids=1 if PSU missing
  if (has_psu && has_strata) {
    srvyr::as_survey_design(df,
                            ids     = !!sym(psu_var),
                            strata  = !!sym(strata_var),
                            weights = !!sym(weight_var),
                            nest    = nest
    )
  } else if (has_psu && !has_strata) {
    srvyr::as_survey_design(df,
                            ids     = !!sym(psu_var),
                            weights = !!sym(weight_var),
                            nest    = nest
    )
  } else if (!has_psu && has_strata) {
    # This is unusual, but handle it: no PSU, strata present
    srvyr::as_survey_design(df,
                            ids     = 1,
                            strata  = !!sym(strata_var),
                            weights = !!sym(weight_var),
                            nest    = nest
    )
  } else {
    srvyr::as_survey_design(df,
                            ids     = 1,
                            weights = !!sym(weight_var)
    )
  }
}

# ============================================================
# 4) Diagnostics/checks (range checks, missingness, etc.)
#    - Keeps things "scientific-journal safe" by warning loudly
#      when something looks off.
# ============================================================
srh_input_checks <- function(vars_df,
                             srh_var = "srh",
                             year_var = "year",
                             age_group_var = "age_group",
                             age_var = "age",
                             check_sample_n = 200000,
                             verbose = TRUE) {
  
  # Basic existence checks
  needed <- c(srh_var, year_var, age_group_var)
  missing <- setdiff(needed, names(vars_df))
  if (length(missing) > 0) {
    stop(glue::glue("Missing required variables in design/data: {paste(missing, collapse = ', ')}"))
  }
  
  srh <- vars_df[[srh_var]]
  yr  <- vars_df[[year_var]]
  ag  <- vars_df[[age_group_var]]
  
  # Year checks
  if (!is.numeric(yr)) warning("Year variable is not numeric. You said it should be numeric.")
  yr_rng <- range(yr, na.rm = TRUE)
  if (any(is.infinite(yr_rng))) warning("Year range is infinite; check missing/non-finite values.")
  if (yr_rng[1] < 1900 || yr_rng[2] > 2100) warning(glue::glue("Year range looks unusual: {yr_rng[1]}–{yr_rng[2]}"))
  
  # SRH checks
  if (!is.numeric(srh)) warning("SRH variable is not numeric; weighted means require numeric coding.")
  srh_rng <- range(srh, na.rm = TRUE)
  if (any(is.infinite(srh_rng))) warning("SRH range is infinite; check missing/non-finite values.")
  if (srh_rng[1] < 1 || srh_rng[2] > 5) {
    warning(glue::glue("SRH range is outside expected 1–5: {srh_rng[1]}–{srh_rng[2]}."))
  } else if (srh_rng[2] <= 4) {
    # This is fine; just informative
    if (verbose) message("Detected SRH scale max <= 4 (likely a 1–4 instrument).")
  } else {
    if (verbose) message("Detected SRH scale max near 5 (likely a 1–5 instrument).")
  }
  
  # Age group checks
  if (all(is.na(ag))) warning("age_group is all NA.")
  if (verbose) {
    ag_lvls <- if (is.factor(ag)) levels(ag) else sort(unique(as.character(ag)))
    message(glue::glue("Age groups detected ({length(ag_lvls)}): {paste(ag_lvls, collapse = ', ')}"))
  }
  
  # Optional: sanity-check age_group against age (sampled, to avoid heavy compute)
  if (age_var %in% names(vars_df)) {
    age <- vars_df[[age_var]]
    # sample rows for speed if needed
    n <- nrow(vars_df)
    idx <- if (n > check_sample_n) sample.int(n, check_sample_n) else seq_len(n)
    
    df_s <- tibble(
      age       = age[idx],
      age_group = as.character(ag[idx])
    ) %>%
      filter(!is.na(age), !is.na(age_group))
    
    # parse labels like "18-29" or "70+"
    parsed <- df_s %>%
      mutate(
        lo = suppressWarnings(as.numeric(str_extract(age_group, "^\\d+"))),
        hi = case_when(
          str_detect(age_group, "\\+") ~ Inf,
          str_detect(age_group, "-")   ~ suppressWarnings(as.numeric(str_extract(age_group, "(?<=-)\\d+"))),
          TRUE                         ~ NA_real_
        ),
        out_of_range = case_when(
          is.na(lo) ~ NA,
          is.infinite(hi) ~ age < lo,
          !is.infinite(hi) ~ (age < lo | age > hi),
          TRUE ~ NA
        )
      )
    
    # If we can compute, warn if lots of mismatches
    if (any(!is.na(parsed$out_of_range))) {
      prop_bad <- mean(parsed$out_of_range, na.rm = TRUE)
      if (prop_bad > 0.01) {
        warning(glue::glue(
          "Age vs age_group mismatch detected in ~{scales::percent(prop_bad)} of sampled rows. ",
          "This may indicate mis-coded age_group labels or age variable."
        ))
      } else if (verbose) {
        message(glue::glue("Age vs age_group sanity check OK (mismatch ~{scales::percent(prop_bad)} in sample)."))
      }
    }
  } else if (verbose) {
    message(glue::glue("age variable '{age_var}' not found; skipping age-vs-age_group sanity check."))
  }
  
  invisible(list(year_range = yr_rng, srh_range = srh_rng))
}

# ============================================================
# 5) MAIN: Compute weighted mean SRH by year × age_group (+ SE)
#    Returns a tidy tibble suitable for plotting and tables.
# ============================================================
srh_weighted_table <- function(data,
                               srh_var       = "srh",
                               year_var      = "year",
                               age_group_var = "age_group",
                               dataset_label = "Dataset",
                               
                               # If data is raw, these are used to build the design
                               weight_var = "wt",
                               psu_var    = "psu",
                               strata_var = "strata",
                               nest       = TRUE,
                               
                               # Variance/SE behavior
                               lonely_psu = "adjust",   # common choice for domain means
                               ci_level   = 0.95,
                               include_ci = TRUE,
                               
                               # Cleaning + checks
                               drop_na            = TRUE,
                               drop_zero_weights  = TRUE,
                               check_inputs       = TRUE,
                               age_var            = "age",
                               check_sample_n     = 200000,
                               verbose            = TRUE,
                               
                               # engine: "survey" usually faster than group_by+summarize for huge data
                               engine = c("survey", "srvyr")) {
  
  engine <- match.arg(engine)
  
  # Capture variable names (supports strings or bare names)
  srh_sym  <- ensym(srh_var);  srh_nm  <- as_name(srh_sym)
  year_sym <- ensym(year_var); year_nm <- as_name(year_sym)
  age_sym  <- ensym(age_group_var); age_nm <- as_name(age_sym)
  
  # Build/standardize to tbl_svy
  svy <- as_tbl_svy_flex(
    data       = data,
    weight_var = weight_var,
    psu_var    = psu_var,
    strata_var = strata_var,
    nest       = nest,
    verbose    = verbose
  )
  
  # Set lonely PSU behavior *temporarily* (important for domain estimates)
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  
  # Filter missing SRH/year/age_group; optionally filter nonpositive weights if weight_var exists
  # NOTE: This is "analysis dataset" creation — standard in applied survey work.
  vars_df <- svy$variables
  
  if (check_inputs) {
    srh_input_checks(
      vars_df         = vars_df,
      srh_var         = srh_nm,
      year_var        = year_nm,
      age_group_var   = age_nm,
      age_var         = age_var,
      check_sample_n  = check_sample_n,
      verbose         = verbose
    )
  }
  
  # Drop missing analysis vars
  if (drop_na) {
    svy <- svy %>%
      filter(!is.na(!!srh_sym), !is.na(!!year_sym), !is.na(!!age_sym))
  }
  
  # Drop zero/negative weights (if weight var exists in the design variables)
  # This is especially relevant given your NHIS example showing weights of 0 -> prob=Inf.
  if (drop_zero_weights && weight_var %in% names(svy$variables)) {
    svy <- svy %>% filter(!is.na(.data[[weight_var]]), .data[[weight_var]] > 0)
  }
  
  # Re-grab variables after filtering
  vars_df <- svy$variables
  
  # Unweighted N per cell (useful for tables / QC)
  n_tbl <- vars_df %>%
    transmute(
      year      = !!year_sym,
      age_group = !!age_sym,
      srh_tmp   = !!srh_sym
    ) %>%
    filter(!is.na(year), !is.na(age_group), !is.na(srh_tmp)) %>%
    count(year, age_group, name = "n_unweighted")
  
  # Ensure age_group ordered youngest->oldest
  n_levels <- n_distinct(as.character(vars_df[[age_nm]]))
  # We'll order based on labels unless user already set factor levels appropriately
  # (this will still preserve label order if they’re already ordered in a sensible way).
  age_levels_ordered <- {
    ag <- vars_df[[age_nm]]
    ag_ord <- order_age_groups(ag)
    levels(ag_ord)
  }
  
  # -------------------------
  # Compute estimates
  # -------------------------
  if (engine == "survey") {
    # Use svyby for efficiency on big data
    f_mean <- reformulate(srh_nm)
    f_by   <- reformulate(c(year_nm, age_nm))
    
    svyby_res <- survey::svyby(
      formula  = f_mean,
      by       = f_by,
      design   = svy,
      FUN      = survey::svymean,
      na.rm    = TRUE,
      keep.var = TRUE,
      drop.empty.groups = TRUE
    )
    
    # Extract SE robustly
    se_obj <- survey::SE(svyby_res)
    se_vec <- if (is.matrix(se_obj)) {
      # If matrix, pick the SRH column if available
      if (!is.null(colnames(se_obj)) && srh_nm %in% colnames(se_obj)) {
        as.numeric(se_obj[, srh_nm])
      } else {
        as.numeric(se_obj[, 1])
      }
    } else {
      as.numeric(se_obj)
    }
    
    out <- as_tibble(svyby_res) %>%
      # Standardize column names for downstream code:
      rename(
        year      = !!year_sym,
        age_group = !!age_sym,
        srh_mean  = !!srh_sym
      ) %>%
      mutate(srh_se = se_vec)
    
    if (include_ci) {
      df_design <- survey::degf(svy)
      alpha <- 1 - ci_level
      crit <- if (!is.finite(df_design) || df_design <= 0) {
        qnorm(1 - alpha / 2)
      } else {
        qt(1 - alpha / 2, df = df_design)
      }
      
      out <- out %>%
        mutate(
          srh_ci_low  = srh_mean - crit * srh_se,
          srh_ci_high = srh_mean + crit * srh_se
        )
    }
    
  } else {
    # srvyr engine (more "tidy", sometimes slower on huge designs)
    out <- svy %>%
      group_by(!!year_sym, !!age_sym) %>%
      summarize(
        srh_mean = survey_mean(!!srh_sym, na.rm = TRUE),
        srh_se   = survey_mean(!!srh_sym, na.rm = TRUE, vartype = "se"),
        .groups = "drop"
      ) %>%
      rename(
        year      = !!year_sym,
        age_group = !!age_sym
      )
    
    if (include_ci) {
      # srvyr can directly compute CI if you prefer; here we compute t-based CI similarly:
      df_design <- survey::degf(svy)
      alpha <- 1 - ci_level
      crit <- if (!is.finite(df_design) || df_design <= 0) qnorm(1 - alpha / 2) else qt(1 - alpha / 2, df = df_design)
      
      out <- out %>%
        mutate(
          srh_ci_low  = srh_mean - crit * srh_se,
          srh_ci_high = srh_mean + crit * srh_se
        )
    }
  }
  
  # Join unweighted N
  out <- out %>%
    left_join(n_tbl, by = c("year", "age_group"))
  
  # Order age groups and ensure year numeric
  out <- out %>%
    mutate(
      year      = as.numeric(year),
      age_group = order_age_groups(age_group, explicit_levels = age_levels_ordered)
    ) %>%
    arrange(year, age_group)
  
  meta <- list(
    dataset_label = dataset_label,
    srh_range = range(vars_df[[srh_nm]], na.rm = TRUE),
    year_range = range(vars_df[[year_nm]], na.rm = TRUE),
    n_rows_analysis = nrow(vars_df),
    n_age_groups = n_distinct(as.character(vars_df[[age_nm]]))
  )
  
  attr(out, "meta") <- meta
  out
}

# ============================================================
# 6) Plot function (fast; uses the already-computed table)
# ============================================================
plot_srh_over_time <- function(srh_tbl,
                               dataset_label = "Dataset",
                               title_style   = c("descriptive", "dataset_only"),
                               uncertainty   = c("none", "ci", "se"),
                               show_points   = TRUE,
                               show_legend   = TRUE,
                               base_size     = 12) {
  
  title_style <- match.arg(title_style)
  uncertainty <- match.arg(uncertainty)
  
  # Palette based on number of age groups (ordered youngest->oldest)
  age_levels <- levels(srh_tbl$age_group)
  pal <- okabe_ito_rainbow(length(age_levels))
  pal_named <- setNames(pal, age_levels)
  
  # Titles
  plot_title <- if (title_style == "descriptive") {
    "SRH per age group per year over time"
  } else {
    dataset_label
  }
  plot_subtitle <- if (title_style == "descriptive") dataset_label else NULL
  
  p <- ggplot(srh_tbl, aes(x = year, y = srh_mean, color = age_group, group = age_group))
  
  # Uncertainty display (optional)
  if (uncertainty == "ci") {
    if (!all(c("srh_ci_low", "srh_ci_high") %in% names(srh_tbl))) {
      warning("CI columns not found in srh_tbl; set include_ci=TRUE in srh_weighted_table().")
    } else {
      p <- p +
        geom_ribbon(
          aes(ymin = srh_ci_low, ymax = srh_ci_high, fill = age_group),
          alpha = 0.15,
          color = NA,
          show.legend = FALSE
        )
    }
  } else if (uncertainty == "se") {
    p <- p +
      geom_errorbar(
        aes(ymin = srh_mean - srh_se, ymax = srh_mean + srh_se),
        width = 0.25,
        linewidth = 0.4,
        show.legend = FALSE
      )
  }
  
  # Lines + points
  p <- p + geom_line(linewidth = 0.9)
  if (show_points) {
    p <- p + geom_point(size = 1.8)
  }
  
  # Scales + labels + theme
  p <- p +
    scale_color_manual(values = pal_named, drop = FALSE) +
    scale_fill_manual(values = pal_named, drop = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(
      title    = plot_title,
      subtitle = plot_subtitle,
      x        = "Year",
      y        = "Survey-weighted mean self-rated health",
      color    = "Age group"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "right" else "none"
    )
  
  p
}

# ============================================================
# 7) Convenience wrapper: returns both table + plot + metadata
# ============================================================
srh_over_time <- function(data,
                          dataset_label = "Dataset",
                          srh_var       = "srh",
                          year_var      = "year",
                          age_group_var = "age_group",
                          weight_var    = "wt",
                          psu_var       = "psu",
                          strata_var    = "strata",
                          nest          = TRUE,
                          lonely_psu    = "adjust",
                          include_ci    = TRUE,
                          plot_title_style = c("descriptive", "dataset_only"),
                          plot_uncertainty = c("none", "ci", "se"),
                          engine = c("survey", "srvyr"),
                          verbose = TRUE,
                          ...) {
  
  plot_title_style <- match.arg(plot_title_style)
  plot_uncertainty <- match.arg(plot_uncertainty)
  
  tbl <- srh_weighted_table(
    data         = data,
    srh_var      = srh_var,
    year_var     = year_var,
    age_group_var= age_group_var,
    dataset_label= dataset_label,
    weight_var   = weight_var,
    psu_var      = psu_var,
    strata_var   = strata_var,
    nest         = nest,
    lonely_psu   = lonely_psu,
    include_ci   = include_ci,
    engine       = engine,
    verbose      = verbose,
    ...
  )
  
  plt <- plot_srh_over_time(
    srh_tbl       = tbl,
    dataset_label = dataset_label,
    title_style   = plot_title_style,
    uncertainty   = plot_uncertainty
  )
  
  list(
    table = tbl,
    plot  = plt,
    meta  = attr(tbl, "meta")
  )
}

