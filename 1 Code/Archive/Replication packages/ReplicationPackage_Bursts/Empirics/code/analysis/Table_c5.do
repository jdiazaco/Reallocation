
* open dataset  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

* control for left cut-off
bysort id: egen minyear = min(year)
bysort siren: egen firmmaxyear = max(year)
bysort siren: egen firmminyear = min(year)
gen tenure = year - minyear 
gen tenureflag = (minyear == 2010 | minyear == firmminyear) 
replace tenure = 10 if tenureflag == 1

* merge firm-level n_c at t+1
preserve
	use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
	drop if revenue == .
	keep id year siren n_c
	replace year = year - 1
	drop if year == 2009
	tempfile temp
	save `temp'
restore
keep id year siren product lagN age N tenure tenureflag nace4_rev2
merge m:1 year siren using `temp', nogen keep(3) keepusing(n_c)

* Gen product exit
bys siren product: egen year_exit = max(year)
gen prod_exit = (year_exit == year)


// Table
gen explanvar = n_c/N
reghdfe prod_exit explanvar  	, noabsorb vce(cluster siren year)
outreg2 using "$path/output/table_c5", replace label noobs nocon sdec(3) bdec(3)
reghdfe prod_exit explanvar  	, absorb(N) vce(cluster siren year)
outreg2 using "$path/output/table_c5", append label noobs nocon sdec(3) bdec(3)
reghdfe prod_exit explanvar  	, absorb(N product) vce(cluster siren year)
outreg2 using "$path/output/table_c5", append label noobs nocon sdec(3) bdec(3)
reghdfe prod_exit explanvar  	, absorb(N product siren) vce(cluster siren year)
outreg2 using "$path/output/table_c5", append label noobs nocon sdec(3) bdec(3)
reghdfe prod_exit explanvar  	, absorb(N product siren age tenure tenureflag	) vce(cluster siren year)
outreg2 using "$path/output/table_c5", append label noobs nocon sdec(3) bdec(3)
reghdfe prod_exit explanvar  	, absorb(N id age i.nace4_rev2#i.year tenure tenureflag	) vce(cluster siren year) 
outreg2 using "$path/output/table_c5", append label noobs nocon sdec(3) bdec(3) tex(frag)

erase "$path/output/table_c5.txt"

