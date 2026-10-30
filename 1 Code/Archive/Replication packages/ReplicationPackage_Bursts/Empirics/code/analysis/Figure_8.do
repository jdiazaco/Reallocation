
*** open data 
use "$path/data/processed/EAPprodfra10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

** create variables 
sort id year
xtset id year 
gen lrevenue  = log(revenue) 
gen dlrevenue = d.lrevenue 


* drop first year for each firm-prod
bysort id: egen minyear = min(year)
drop if year == minyear
drop minyear


** control for left cut-off
bysort id: egen minyear = min(year)
bysort siren: egen firmmaxyear = max(year)
bysort siren: egen firmminyear = min(year)
gen tenure = year - minyear 
replace tenure = . if minyear == $yr_start
replace tenure = . if minyear == firmminyear

* gen vars
xtset id year 
gen revenue_g = (revenue-l.revenue)/(0.5*revenue+0.5*l.revenue)


* Regression method 
reghdfe revenue_g i.tenure, absorb(id) vce(cluster id)
sum lagN if e(sample), d
matrix coef = e(b)
matrix se   = e(V)
matrix se_base   = . 
matrix coef_base = . 
forval hor = 1/7 {
	matrix se_base 		= se_base 	\ 	 (se[`hor',`hor'])^0.5
	matrix coef_base	= coef_base  \ coef[1,`hor'] 
}


clear
foreach bin_type in base {
	svmat coef_`bin_type'
	svmat se_`bin_type'
	gen lbound_`bin_type' = coef_`bin_type' - 1.64*se_`bin_type'
	gen ubound_`bin_type' = coef_`bin_type' + 1.64*se_`bin_type'
}

drop if coef_base == .
gen hor = _n +1


graph twoway line coef_base hor, lcolor(gs10) || scatter coef_base hor, mcolor(blue) msymbol(smsquare_hollow) msize(medlarge) || rcap lbound_base ubound_base hor, lcolor(blue) , /// eltblue
	xlabel(2(1)8, labsize(medium)) xscale(range(2(1)8)) ylabel(`extra_y_scale' -0.08 "-8%" -0.06 "-6%" -0.04 "-4%" -0.02 "-2%" 0.00 "0%", labsize(medium) format(%9.2fc)) xtitle(Years since Production Started, size(medlarge)) legend(off)

graph export "$path/output/figure_8.pdf", replace
