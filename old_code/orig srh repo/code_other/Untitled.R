

# INPUTS
annual_rate <- 0.0075          # 0.75 % deaths per year (CDC 2023)
cum5_rate   <- 1 - (1 - annual_rate)^5    # ≈ 0.038 (3.8 % in 5 y)
haz_ratio   <- 0.96            # HR per 0.10 NDVI (meta-analysis)
z_beta      <- qnorm(0.80)     # for 80 % power

# Required number of deaths using Schoenfeld's formula
events_req  <- (z_crit + z_beta)^2 / (log(haz_ratio))^2

# Expected deaths in one wave over 5 y
events_wave <- N_wave * cum5_rate

# Power achieved with one wave
z_mortality <- abs(log(haz_ratio)) * sqrt(events_wave)
power_mort  <- 1 - pnorm(z_crit - z_mortality)

cat("Mortality power per wave =", round(power_mort, 3), "\n")  # ≈ 0.87

# Optional: pooled five-wave scenario
events_pool <- events_wave * 5
z_mort_pool <- abs(log(haz_ratio)) * sqrt(events_pool)
power_mort_pool <- 1 - pnorm(z_crit - z_mort_pool)

cat("Mortality power (5 pooled waves) =", round(power_mort_pool, 3), "\n")  # >0.99