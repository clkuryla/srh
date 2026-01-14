data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) %>%
    select(srh, age, year, psu, wt, strata)


View(data_nhis %>% head())

data_nhis <- data_nhis |>
    add_age_group(age_var = age, scheme = "A", new_var = age_group_25) |>
    add_age_group(age_var = age, scheme = "B", new_var = age_group)




