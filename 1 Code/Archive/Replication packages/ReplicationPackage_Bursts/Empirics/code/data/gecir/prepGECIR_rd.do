/*	============================================================
	This do file creates a panel based on GECIR raw files
	============================================================	*/

/* ******* use GECIR files ******* */

*** 2010-2013 period
// from 2011 started using a different questionnaire

forvalues year = 2010/2013 {

	use "$path/data/raw/gecir/gecir`year'.dta", clear
	
	rename *, lower

	rename cir_ventile cir_value
	rename effectif_rd empl_rd
		
	if `year' == 2010 {
		gen rd_exp_value = l1+l2+l3+l4+l5+l6+l7+l8+l9+l10+l11
		rename l2 wagebill_rd
	}
	else if `year' != 2010 {
		rename l3 wagebill_rd
		rename l14 rd_exp_value
	}
	
	gen year = `year'
	
	keep year siren rd_exp_value empl_rd wagebill_rd cir_value 
	order year siren rd_exp_value empl_rd wagebill_rd cir_value
	
	* clean vars
	foreach var in rd_exp_value empl_rd wagebill_rd cir_value {
			replace `var' = . if `var' < 0
	}
	
	save "$path/data/processed/temp_gecir`year'.dta", replace

}

clear
forvalues year = 2010/2013 {

	append using "$path/data/processed/temp_gecir`year'.dta"
	erase "$path/data/processed/temp_gecir`year'.dta" 

}

save "$path/data/processed/temp_gecir_2010_2013.dta", replace


*** 2014-2018 period

use "$path/data/raw/gecir/gecirv2020.dta", clear

rename *, lower

rename cir_millesime year
destring year, replace
rename ez empl_rd
rename cir_calcule cir_value
rename l3 wagebill_rd
rename l14 rd_exp_value

keep year siren rd_exp_value empl_rd wagebill_rd cir_value
order year siren rd_exp_value empl_rd wagebill_rd cir_value 

* clean vars
foreach var in rd_exp_value cir_value empl_rd wagebill_rd {
		replace `var' = . if `var' < 0
}

* drop years covered in more recent files
drop if year == 2019

save "$path/data/processed/temp_gecir_2014_2018.dta", replace


*** 2019

use "$path/data/raw/gecir/gecirv2023.dta", clear

rename *, lower

rename cir_millesime year
destring year, replace
rename ez empl_rd
rename cir_fille cir_value
rename l3 wagebill_rd
rename l14 rd_exp_value

keep year siren rd_exp_value empl_rd wagebill_rd cir_value
order year siren rd_exp_value empl_rd wagebill_rd cir_value 

* clean vars
foreach var in rd_exp_value cir_value empl_rd wagebill_rd {
		replace `var' = . if `var' < 0
}

* only keep 2019 as more exhaustive
keep if year == 2019

* deal with duplicates
bys year siren: gen n = _N
bys year siren: egen rd_exp_value_max = max(rd_exp_value)
drop if n > 1 & rd_exp_value != rd_exp_value_max // keep duplicates with highest R&D expenditures
drop n rd_exp_value_max
duplicates drop year siren rd_exp_value, force // drop duplicates with same R&D expenditures

save "$path/data/processed/temp_gecir_2019.dta", replace



* append all years
clear
append using "$path/data/processed/temp_gecir_2019.dta"
append using "$path/data/processed/temp_gecir_2014_2018.dta"
append using "$path/data/processed/temp_gecir_2010_2013.dta"
erase "$path/data/processed/temp_gecir_2019.dta"
erase "$path/data/processed/temp_gecir_2010_2013.dta"
erase "$path/data/processed/temp_gecir_2014_2018.dta"

* label vars
label var year		   				"Year"
label var wagebill_rd  				"Personnel expenditure related to researchers and technicians in R&D"
label var empl_rd 	   				"Number of researchers and technicians who participated in projects declared to CIR"
label var cir_value    				"Value of the tax credit"
label var rd_exp_value				"R&D total expenditure realised by the company"

* save working dta
order year siren
sort year siren
compress
save "$path/data/processed/gecir_rd_20102019.dta", replace
