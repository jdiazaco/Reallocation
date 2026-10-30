* ==============================================================================
* This do file import the raw PC8 - CN8 concordance files and append them as dta
* ==============================================================================

// Clean raw files

forvalue year = 2010/2019 {
	
	import delimited "$path/data/external/corresp_pc8_bec/cn8_pc8corresp/CN `year' - PRODCOM `year'", delimiter(",") varname(1) stringc(1 2) clear
	drop if prc == ""
	drop if cn == ""
		
	replace prc = subinstr(prc,".","",.)
	replace prc = subinstr(prc," ","",.)
	replace prc = substr(prc,1,8)
	
	replace cn = subinstr(cn,".","",.)
	replace cn = subinstr(cn," ","",.)
	replace cn = substr(cn,1,8)
	
	duplicates drop
	
	gen year = `year'
	
	save "$path/data/external/corresp_pc8_bec/cn8_pc8corresp/temp`year'.dta", replace
	
}

clear

forvalue year = 2010/2019 {
	
	append using "$path/data/external/corresp_pc8_bec/cn8_pc8corresp/temp`year'.dta"
	erase "$path/data/external/corresp_pc8_bec/cn8_pc8corresp/temp`year'.dta"
}

save "$path/data/processed/cn8_pc8_corres20102019.dta", replace


