/*	============================================================
	This do file creates a correspondance table between PC8 and BEC
	============================================================	*/
	
	
// prep PC8 <--> CN8 correspondance

do "$path/code/data/corresp_pc8_bec/cn8_pc8_input_files.do"


// load PC8 <--> CN8

use "$path/data/processed/cn8_pc8_corres20102019.dta", clear
drop if year < $yr_start
drop if year > $yr_end
tempfile temp_pc8_cn8
save `temp_pc8_cn8'


// import CN8 <--> BEC

import excel "$path/data/external/corresp_pc8_bec/cn8_bec_2020/CN_BEC_2020.xlsx", sheet("CN8-BEC") firstrow case(lower) allstring clear
gen yr_start = substr(start,-4,.)
gen yr_end   = substr(end,-4,.)
replace yr_end = "2020" if yr_end == "2500"
destring yr_end yr_start, replace
gen diff = yr_end - yr_start + 1
gen id = _n
expand diff
bys id: gen count = _n
gen year = yr_start + count - 1
drop if year < $yr_start
drop if year > $yr_end
keep year cn bec
duplicates drop year cn bec, force
tempfile temp_cn8_bec
save `temp_cn8_bec'


// merge PC8 <--> CN8 <--> BEC

use `temp_pc8_cn8', clear
merge m:m year cn using `temp_cn8_bec'
keep if _m == 3
drop _m


// save correspondence PC8 <--> BEC

keep year prc bec
order year prc bec
duplicates drop year prc bec, force
save "$path/data/processed/corresp_pc8_bec_${yr_start}${yr_end}.dta", replace


