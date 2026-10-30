
// Clean raw files

forvalue year = 2009/2019 {
	
	local next_year = `year' + 1
	import excel "$path/data/external/concord_pc10/tablescorresp/pc10_`next_year'-pc10_`year'", firstrow case(lower) clear
	drop if prodfra`year' == ""
	drop if prodfra`next_year' == ""
		
	replace prodfra`year' = subinstr(prodfra`year',".","",.)
	replace prodfra`next_year' = subinstr(prodfra`next_year',".","",.)
	replace prodfra`year' = substr(prodfra`year',1,10)
	replace prodfra`next_year' = substr(prodfra`next_year',1,10)
		
	keep prodfra`year' prodfra`next_year'
	duplicates drop
	
	drop if prodfra`year' == prodfra`next_year'
	
	export delimited using "$path/data/external/concord_pc10/tablescorresp/corres`year'.csv", delimiter(",") replace
	
}





