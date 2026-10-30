
** firm age 

* open data 
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear

* keep last year 
keep if year == 2019 


* drop older firms, distribution looks horrible 
drop if age >= 70 


* average age for firms in the tail 
su age if N > 5 
matrix collect_1 = r(mean) 
su age if N > 10 
matrix collect_1 = collect_1  \r(mean) 
su age if N > 20 
matrix collect_1 = collect_1  \r(mean) 

* export
putexcel set "$path/output/table_intextnumbers" , replace 
putexcel A1 =  matrix(collect_1)

 

 


/*
	Second moment: relative variance of new vs old products  
*/ 

quiet { 
** prepare the data

* open the data
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

* identify new products 

// control for left cut-off
bysort id: egen minyear = min(year)
bysort siren: egen firmmaxyear = max(year)
bysort siren: egen firmminyear = min(year)
gen tenure = year - minyear 
replace tenure = . if minyear == $yr_start
replace tenure = . if minyear == firmminyear

* create variables
gen lrevenue = log(revenue)
xtset id year 
gen newprod 	= 1 if year == minyear 		& year != firmminyear
replace newprod = 0 if year != minyear  	& year != firmminyear

** obtain variances, means 

* adjust for partial year effect and take FEs  
reghdfe lrevenue   	    , resid absorb( i.nace4_rev2#i.year)  
predict lrevenue_fit	, resid 

* check relative variances 
su lrevenue_fit  if newprod ==1 
matrix storage = r(Var) 
matrix storage_mean = r(mean) 
su lrevenue_fit  //if newprod ==0
matrix storage = storage \  r(Var)   
matrix storage_mean = storage_mean \ r(mean) 

* relative variance 
matrix new_object = round((storage[1,1]-storage[2,1])/(storage_mean[1,1]-storage_mean[2,1])^2 ,0.01)
}
matrix list new_object 
putexcel A5 =  matrix(new_object)





quiet {
** prepare the data

* open the data
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

* identify new products 
// control for left cut-off

bysort id: egen minyear = min(year)
bysort siren: egen firmmaxyear = max(year)
bysort siren: egen firmminyear = min(year)
gen tenure = year - minyear 
replace tenure = . if minyear == $yr_start
replace tenure = . if minyear == firmminyear

* create variables
gen lrevenue = log(revenue)
xtset id year 
gen newprod 	= 1 if year == minyear 		& year != firmminyear
replace newprod = 0 if year != minyear  	& year != firmminyear

* create pairs of products with purged revenue 
reghdfe lrevenue   	    , resid absorb(age product i.nace4_rev2#i.year )  
predict lrevenue_fit	, resid 
keep if newprod ==1 
egen firm = group(siren)  
keep firm year product lrevenue_fit  
preserve 
	rename product product1 
	gen year1 = year
	
	rename lrevenue_fit lrevenue1 
	tempfile left 
	save `left'
restore 
preserve 
	rename product product2 
	gen year2 = year
	rename lrevenue_fit lrevenue2 
	tempfile right  
	save `right'
restore 
joinby firm  using `left'
rename product product2
rename year year2 
rename lrevenue_fit lrevenue2 
}

* regression  
reghdfe lrevenue2 lrevenue1   	    , resid absorb(product2 product1)  
matrix new_object = e(b)
putexcel A7 =  matrix(new_object[1,1])





// calibration target (exact same regression as in the model; entire distribution)
foreach var in  n_c  { 
local var `var' 

	* use raw data
	use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
	gen rev_per_N = revenue/N
	keep if lagN == 1 
	keep if year == 2019 
	
	* prep data
	keep `var' 
	drop if `var' == .  
	gsort -`var'
	gen rank = _n 

	* rescaling
	qui: su `var' 
	replace `var'  = `var'/1
	replace rank = rank //r(N)
	gen rank_m = rank 


	* create scatter vars
	gen log_`var' = log(`var')
	gen log_rank = log(rank_m-0.5)

	* prepare regression 
	gen log_rank_min05 =  log(rank_m-0.5)
	reg log_rank_min05 log_`var' , r


}

* regression  
matrix new_object = e(b)
putexcel A9 =  matrix(new_object[1,1])



