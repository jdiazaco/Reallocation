
clear 
set more off 

local nmc = real("$mc_draws")
if missing(`nmc') | `nmc' <= 0 | floor(`nmc') != `nmc' {
	di as err "Invalid global mc_draws: $mc_draws"
	exit 198
}

tempfile mc_table
postfile posth int draw int stat_id str40 stat_name double value using `mc_table', replace

forvalues mc = 1/`nmc' {
	local suffix : di %02.0f `mc'
	local src "$input/simulation_productlevel_`suffix'.csv"
	capture confirm file "`src'"
	if _rc {
		di as err "Missing required Monte Carlo file: `src'"
		exit 601
	}

	import delimited "`src'", clear
	gen n_c = numprodadded
	gen N = nprod
	duplicates drop firmid2 year, force 
	xtset firmid2 year
	keep if year <= 11

	gen lag_9_N = l9.N
	gen Nchange_lag9 = N - lag_9_N

	bysort firmid2: egen total_n_c = total(n_c)
	gen flag = n_c > 0.5*total_n_c & n_c != .
	bysort firmid2: egen flag_total = total(flag)
	replace flag_total = (flag_total > 0 & flag_total != .)

	quietly summarize flag_total if Nchange_lag9 != . & total_n_c/lag_9_N > 20
	post posth (`mc') (1) ("ratio_gt20") (r(mean))

	quietly summarize flag_total if Nchange_lag9 != . & total_n_c/lag_9_N > 10
	post posth (`mc') (2) ("ratio_gt10") (r(mean))

	quietly summarize flag_total if Nchange_lag9 != . & total_n_c/lag_9_N > 5
	post posth (`mc') (3) ("ratio_gt5") (r(mean))

	quietly summarize flag_total if Nchange_lag9 != . & total_n_c > 20 & lag_9_N == 1
	post posth (`mc') (4) ("total_gt20_lag1") (r(mean))

	quietly summarize flag_total if Nchange_lag9 != . & total_n_c > 10 & lag_9_N == 1
	post posth (`mc') (5) ("total_gt10_lag1") (r(mean))

	quietly summarize flag_total if Nchange_lag9 != . & total_n_c > 5 & lag_9_N == 1
	post posth (`mc') (6) ("total_gt5_lag1") (r(mean))
}

postclose posth
use `mc_table', clear
collapse (mean) value=value (sd) sd=value, by(stat_id stat_name)
sort stat_id

export delimited using "$output/table_8_model.csv", replace

