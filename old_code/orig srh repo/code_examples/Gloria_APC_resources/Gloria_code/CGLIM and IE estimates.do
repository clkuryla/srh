
// CGLIM do-loop for single BA measure, can loop through multiple specified constraints; saves results individually and then compiles to combined file - FOR REFERENCE ONLY

	local constraint_spec "a48=a52 p2003=p2007 c1963=c1967"

	// Create an empty dataset to hold the combined results
	clear
	set obs 0
	save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\CGLIM_combined.dta", replace emptyok

	use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\apcmatrix_all.dta", clear

	foreach var of local constraint_spec {
		#delim ;
		apc_cglim phenoageadv_delta_levinenocrp if age <= 80,
		  age(age_4yr) period(period_4yr) cohort(cohort_4yr)
		  agepfx("_A") periodpfx("_P") cohortpfx("_C")
		  family(gaussian) constraint(`"`var'"');
		drop _A* _P* _C*;
		#delim cr

		regsave using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\CGLIM_`var'", pval replace
	}

	// Combine the individual result datasets and add an identifier
	foreach var of local constraint_spec {
		use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\CGLIM_`var'.dta", clear
		gen var_id = "`var'"
		append using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\CGLIM_combined.dta"
		save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\CGLIM_combined.dta", replace
	}

// IE do-loop for multiple BA measures; saves results individually and then compiles to combined file - ALL

	local outcome_vars "phenoageadv_delta_levinenocrp phenoageadv_delta_elasticnet kdmadv_delta_levinenocrp kdmadv_delta_elasticnet hdlog_levinenocrp hdlog_elasticnet"

	// Create an empty dataset to hold the combined results
	clear
	set obs 0
	save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_unstrat.dta", replace emptyok

	use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\apcmatrix_all.dta", clear

	foreach var of local outcome_vars {
		#delim ;
		apc_ie `var' if age <= 80, ///
		  age(age_4yr) period(period_4yr) cohort(cohort_4yr) family(gaussian);
		#delim cr

		regsave using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_unstrat_`var'", pval replace
	}

	// Combine the individual result datasets and add an identifier
	foreach var of local outcome_vars {
		use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_unstrat_`var'.dta", clear
		gen var_id = "`var'"
		append using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_unstrat.dta"
		save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_unstrat.dta", replace
	}
	

// IE do-loop for multiple BA measures; saves results individually and then compiles to combined file - WHITE MEN

	local outcome_vars "phenoageadv_delta_levinenocrp phenoageadv_delta_elasticnet kdmadv_delta_levinenocrp kdmadv_delta_elasticnet hdlog_levinenocrp hdlog_elasticnet"

	// Create an empty dataset to hold the combined results
	clear
	set obs 0
	save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_wm.dta", replace emptyok

	use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\apcmatrix_strat.dta", clear

	foreach var of local outcome_vars {
		#delim ;
		apc_ie `var' if age <= 80 & race == "White" & gender == "Men", ///
		  age(age_4yr) period(period_4yr) cohort(cohort_4yr) family(gaussian);
		#delim cr

		regsave using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_wm_`var'", pval replace
	}

	// Combine the individual result datasets and add an identifier
	foreach var of local outcome_vars {
		use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_wm_`var'.dta", clear
		gen var_id = "`var'"
		append using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_wm.dta"
		save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_wm.dta", replace
	}

	
// IE do-loop for multiple BA measures; saves results individually and then compiles to combined file - WHITE WOMEN

	local outcome_vars "phenoageadv_delta_levinenocrp phenoageadv_delta_elasticnet kdmadv_delta_levinenocrp kdmadv_delta_elasticnet hdlog_levinenocrp hdlog_elasticnet"

	// Create an empty dataset to hold the combined results
	clear
	set obs 0
	save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_ww.dta", replace emptyok

	use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\apcmatrix_strat.dta", clear

	foreach var of local outcome_vars {
		#delim ;
		apc_ie `var' if age <= 80 & race == "White" & gender == "Women", ///
		  age(age_4yr) period(period_4yr) cohort(cohort_4yr) family(gaussian);
		#delim cr

		regsave using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_ww_`var'", pval replace
	}

	// Combine the individual result datasets and add an identifier
	foreach var of local outcome_vars {
		use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_ww_`var'.dta", clear
		gen var_id = "`var'"
		append using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_ww.dta"
		save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_ww.dta", replace
	}

// IE do-loop for multiple BA measures; saves results individually and then compiles to combined file - BLACK MEN

	local outcome_vars "phenoageadv_delta_levinenocrp phenoageadv_delta_elasticnet kdmadv_delta_levinenocrp kdmadv_delta_elasticnet hdlog_levinenocrp hdlog_elasticnet"

	// Create an empty dataset to hold the combined results
	clear
	set obs 0
	save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_bm.dta", replace emptyok

	use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\apcmatrix_strat.dta", clear

	foreach var of local outcome_vars {
		#delim ;
		apc_ie `var' if age <= 80 & race == "Black" & gender == "Men", ///
		  age(age_4yr) period(period_4yr) cohort(cohort_4yr) family(gaussian);
		#delim cr

		regsave using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_bm_`var'", pval replace
	}

	// Combine the individual result datasets and add an identifier
	foreach var of local outcome_vars {
		use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_bm_`var'.dta", clear
		gen var_id = "`var'"
		append using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_bm.dta"
		save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_bm.dta", replace
	}
	
	
// IE do-loop for multiple BA measures; saves results individually and then compiles to combined file - BLACK WOMEN

	local outcome_vars "phenoageadv_delta_levinenocrp phenoageadv_delta_elasticnet kdmadv_delta_levinenocrp kdmadv_delta_elasticnet hdlog_levinenocrp hdlog_elasticnet"

	// Create an empty dataset to hold the combined results
	clear
	set obs 0
	save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_bw.dta", replace emptyok

	use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\apcmatrix_strat.dta", clear

	foreach var of local outcome_vars {
		#delim ;
		apc_ie `var' if age <= 80 & race == "Black" & gender == "Women", ///
		  age(age_4yr) period(period_4yr) cohort(cohort_4yr) family(gaussian);
		#delim cr

		regsave using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_bw_`var'", pval replace
	}

	// Combine the individual result datasets and add an identifier
	foreach var of local outcome_vars {
		use "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\Temp stata output\IE_bw_`var'.dta", clear
		gen var_id = "`var'"
		append using "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_bw.dta"
		save "C:\Users\glory\OneDrive - cumc.columbia.edu\Dissertation\Data\Aim 2 outputs\IE_combined_bw.dta", replace
	}


