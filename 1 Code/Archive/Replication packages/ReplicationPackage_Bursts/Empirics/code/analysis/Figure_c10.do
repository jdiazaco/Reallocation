
* options graphs
local y_scale_All -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medium)
local y_scale_1 -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medium)
local y_scale_5 -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}", labsize(medium)
local y_scale_10 0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}", labsize(medium)
local y_scale_15 0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}", labsize(medium)
local y_scale_20 0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}", labsize(medium)
local x_scale 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32" \`extra_xscale', labsize(medium)

local y_title `""Rank", size(medlarge)"'
local x_title `""Number of new products {it:n}", size(medlarge)"'

local conf_var 
local extra_conf_vars
if ${conf_check} == 1 {		
	local conf_var _av
	local extra_conf_vars conf bin lsh_av lnew_prod_av
}

local data_types_list correspc10


** Prepare data for the plots

foreach data_type in `data_types_list' {
	
	* use raw data
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
		
	noi di _n "File `data_type': lagN "
	
	local N_list All 1
	
	foreach N in `N_list' {
		
		noi di _continue "`N' "
		
		preserve
		
		local extra_xscale
		if "`N'" != "All" ///
			keep if lagN == `N'
		else ///
			local extra_xscale 4.1588831 "64" 4.8520303 "128"

		rename n_c new_prod // Number of new products
		
		_rank_byvar id, by_var(new_prod)
		
		replace conf = id_count if conf == .
		drop id_count dem num sh
		drop if new_prod == 0
		foreach var of varlist lnew_prod lsh `extra_conf_vars' {
		rename `var' `var'_base
			}
					
		save "$path/data/processed/temp_`data_type'_`N'_base.dta", replace
		
		restore 
			
	}
	
}


** Prepare data for the plots - EXCLUDE RETURNING PRODUCT

foreach data_type in `data_types_list' {
	
	* use raw data
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_noreturnprod_firm_clean.dta", clear
		
	noi di _n "File `data_type': lagN "
	
	local N_list All 1
	
	foreach N in `N_list' {
		
		noi di _continue "`N' "
		
		preserve
		
		local extra_xscale
		if "`N'" != "All" ///
			keep if lagN == `N'
		else ///
			local extra_xscale 4.1588831 "64" 4.8520303 "128"

		rename n_c new_prod // Number of new products
		
		_rank_byvar id, by_var(new_prod)
		
		replace conf = id_count if conf == .
		drop id_count dem num sh
		drop if new_prod == 0
		foreach var of varlist lnew_prod lsh `extra_conf_vars' {
			rename `var' `var'_noreturnprod
		}
					
		save "$path/data/processed/temp_`data_type'_`N'_noreturnprod.dta", replace
		 
		restore 
			
	}
} 




quietly {
	
	foreach data_type in `data_types_list' {
	
	local N_list All 1 

	foreach N in `N_list' {
		
		noi di _continue "`N' "
		
		preserve
		
		local extra_xscale
		if "`N'" != "All" ///
			keep if lagN == `N'
		else ///
			local extra_xscale 4.1588831 "64" 4.8520303 "128"
				
				
		// Figure: Baseline versus exclude returning products
		
		use $path/data/processed/temp_`data_type'_`N'_base , clear
		merge 1:1 new_prod using $path/data/processed/temp_`data_type'_`N'_noreturnprod, nogen

		sort new_prod
		
		foreach group_type in base noreturnprod {
		
			egen temp_`group_type' = max(lnew_prod_`group_type')
			gen max_`group_type' = lnew_prod_`group_type' == temp_`group_type' if lnew_prod_`group_type'  !=.
			drop temp_`group_type' 

		}			
			
		twoway 	(scatter lsh`conf_var'_base lnew_prod`conf_var'_base, mcolor(blue)  msize(medlarge)) 										///
				(lfit lsh_base lnew_prod_base if max_base != 1, lc(blue))	 	///		
				(scatter lsh`conf_var'_noreturnprod lnew_prod`conf_var'_noreturnprod, mcolor(red) ms(smdiamond_hollow) msize(medlarge)) 										///
				(lfit lsh_noreturnprod lnew_prod_noreturnprod if max_noreturnprod != 1, lp(dash) lc(red)), 	///
				legend(order(1 "Baseline" 3 "Exclude returning products") ring(0) pos(2) size(*1.5)) ///
				xtitle(`x_title')  						///
				ytitle(`y_title')					///
				xlabel(`x_scale') ///
				ylabel(`y_scale_`N'')
				
			graph export "$path/output/figure_c10_lagN`N'.pdf", replace

			restore
		}
		
	}
	
}
	
foreach data_type in `data_types_list' {	
	foreach N in `N_list' {		
		erase $path/data/processed/temp_`data_type'_`N'_base.dta
		erase $path/data/processed/temp_`data_type'_`N'_noreturnprod.dta
	}
} 


	
