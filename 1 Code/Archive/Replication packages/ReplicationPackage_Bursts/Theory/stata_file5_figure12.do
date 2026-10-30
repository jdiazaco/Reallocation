
** this file estimates product lifecycle revenue growth averaged across Monte Carlo draws

local nmc = real("$mc_draws")
if missing(`nmc') | `nmc' <= 0 | floor(`nmc') != `nmc' {
	di as err "Invalid global mc_draws: $mc_draws"
	exit 198
}

tempfile mc_series

forvalues mc = 1/`nmc' {
	local suffix : di %02.0f `mc'
	local src "$input/simulation_productlevel_`suffix'.csv"
	capture confirm file "`src'"
	if _rc {
		di as err "Missing required Monte Carlo file: `src'"
		exit 601
	}

	import delimited "`src'", clear

	* make variable names data consistent (CSV imports are lowercase)
	gen siren = firmid2
	gen id    = prodindex
	gen product=id

	** create variables
	sort id year
	xtset id year
	gen lrevenue  = log(revenue)
	gen dlrevenue = d.lrevenue

	** control for left cut-off
	bysort id: egen minyear = min(year)
	bysort siren: egen firmmaxyear = max(year)
	bysort siren: egen firmminyear = min(year)
	gen tenure = year - minyear
	replace tenure = . if minyear == $year_start
	replace tenure = . if minyear == firmminyear

	* calculate variables
	xtset id year
	gen revenue_g = (revenue-l.revenue)/(0.5*revenue+0.5*l.revenue)
	bysort id: egen revenue_g_m = mean(revenue_g)
	bysort id: egen dlrevenue_m = mean(dlrevenue)
	gen revenue_g_dm = revenue_g- revenue_g_m
	gen dlrevenue_dm = dlrevenue- dlrevenue_m

	* year 'fe'
	bysort year: egen revenue_g_my = mean(revenue_g_dm)
	bysort year: egen dlrevenue_my = mean(dlrevenue_dm)
	gen revenue_g_dmy = revenue_g_dm- revenue_g_my
	gen dlrevenue_dmy = revenue_g_dm- dlrevenue_my

	* exit adjustment
	xtset id year
	tsfill, full
	bysort id: carryforward siren, gen(firm2)
	gsort id - year
	bysort id: carryforward firm2, gen(firmfinal)
	drop siren firm2
	rename firmfinal siren
	sort id (year)
	bysort id: carryforward product, gen(prod2)
	gsort id - year
	bysort id: carryforward prod2, gen(prodfinal)
	drop product prod2
	rename prodfinal product
	sort id (year)
	order id year siren product
	xtset id year
	gen revenue_g_exitadj =  revenue_g
	replace revenue_g_exitadj = -2 if revenue == . & l.revenue != . & year < firmmaxyear
	replace tenure = l.tenure +1 if revenue == . & l.revenue != . & year < firmmaxyear
	bysort id: egen revenue_g_exitadj_m = mean(revenue_g_exitadj)
	gen revenue_g_exitadj_dm = revenue_g_exitadj- revenue_g_exitadj_m

	* drop first year
	drop if tenure == 1

	* aggregate revenue growth over the sample
	bysort year: egen aggregate_output = sum(revenue)
	xtset id year
	gen revenue_g_agg = (aggregate_output-l.aggregate_output)/(0.5*aggregate_output+0.5*l.aggregate_output)
	gen dlrevenue_agg = log(aggregate_output) - log(l.aggregate_output)
	egen revenue_g_agg_mean = mean(revenue_g_agg)
	egen dlrevenue_agg_mean = mean(dlrevenue_agg)

	*** demeaned coefficients
	matrix published_coef = .
	forval i = 2/8 {
		su revenue_g_dm if tenure == `i'
		matrix published_coef = published_coef \ r(mean)
	}

	clear
	svmat published_coef
	gen hor = _n
	keep hor published_coef1
	gen draw = `mc'

	if `mc' == 1 {
		save `mc_series', replace
	}
	else {
		append using `mc_series'
		save `mc_series', replace
	}
}

use `mc_series', clear
collapse (mean) published_coef1 (sd) published_coef_sd = published_coef1, by(hor)
gen published_coef_se1 = published_coef_sd / sqrt(`nmc')
drop published_coef_sd

** data (manually pasted)
gen hor2 = hor + 0.1
gen data = .
replace data = 0 if hor == 2
replace data = -0.015196 if hor == 3
replace data = -0.027284 if hor == 4
replace data = -0.035065 if hor == 5
replace data = -0.042647 if hor == 6
replace data = -0.057033 if hor == 7
replace data = -0.057940 if hor == 8

gen data_se = .
replace data_se = 0 if hor == 2
replace data_se = 0.009699 if hor == 3
replace data_se = 0.009751 if hor == 4
replace data_se = 0.010561 if hor == 5
replace data_se = 0.011943 if hor == 6
replace data_se = 0.013278 if hor == 7
replace data_se = 0.016946 if hor == 8

gen ubound_data = data + 1.64*data_se
gen lbound_data = data - 1.64*data_se

** with se
su published_coef1 if hor == 2
gen ubound = published_coef1 + 1.64*published_coef_se1 - r(mean)
gen lbound = published_coef1 - 1.64*published_coef_se1 - r(mean)
replace ubound = 0 if hor == 2
replace lbound = 0 if hor == 2
replace published_coef1 = published_coef1 - r(mean)
drop if hor < 2

graph twoway rcap lbound ubound hor, lcolor(blue) xlabel(2(1)8) || ///
		scatter published_coef1 hor, mcolor(blue) msize(3) msymbol(oh) xlabel(2(1)8) || ///
		rcap lbound_data ubound_data hor2, lcolor(midgreen) xlabel(2(1)8) || ///
		scatter data hor2, msize(2) msymbol(Dh) mcolor(midgreen) xlabel(2(1)8) ///
		legend(order(2 4) label(2 "Model") label(4 "Data") position(1) ring(0)) ///
		xtitle("Years since Production Started") ytitle("Growth - Deviation Firm-Product Average") ylabel(-0.08 "-8%" -0.06 "-6%" -0.04 "-4%" -0.02 "-2%" 0 "0%")
		graph export "$output/figure_12.pdf", replace
