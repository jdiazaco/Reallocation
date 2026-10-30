
// Options graphs
local y_scale_All -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medium)
local y_scale_1 -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medium)
local y_scale_5 -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}", labsize(medium)
local y_scale_10 0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}", labsize(medium)
local y_scale_15 0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}", labsize(medium)
local y_scale_20 0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}", labsize(medium)
local x_scale 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32" \`extra_xscale', labsize(medium)

local y_title `""Rank", size(medlarge)"' // Share of firms with products {it:{&ge} n}
local x_title `""Number of new products {it:n}", size(medlarge)"'

local conf_var // variable names for the scatter graph (different in case of confidentiality checks)
local extra_conf_vars
if ${conf_check} == 1 {		
	local conf_var _av
	local extra_conf_vars conf bin lsh_av lnew_prod_av
}


// Preparation of BEC files
use "$path/data/processed/corresp_pc8_bec_20102019.dta", clear
gen bec1d = substr(bec,1,1)
keep if bec1d == "6" // keep only consumer goods
drop bec bec1d
duplicates drop
rename prc product
save "$path/data/processed/temp_bec.dta", replace




local data_types_list pc8 // 


** Prepare data for the plots - BASELINE

foreach data_type in `data_types_list' {
	
	* use raw data
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
		
	noi di _n "File `data_type': lagN "
	
	local N_list All 1
	if substr("`data_type'", 1, 5) == "pc10_" ///
		local N_list All 1 5 10
	
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
		
		if "`N'" != "20" {
			replace conf = id_count if conf == .
			drop id_count dem num sh
			drop if new_prod == 0
			foreach var of varlist lnew_prod lsh `extra_conf_vars' {
				rename `var' `var'_base
			}
					
			save "$path/data/processed/temp_`data_type'_`N'_base.dta", replace
		} 
		
		restore 
			
	}
	
}


** Prepare data for the plots - ONLY CONSUMER GOODS (BEC == 6)


foreach data_type in `data_types_list' {
	
	* use raw data
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firmprod_clean.dta", clear
	
	// merge products in BEC == 6
	merge m:1 year product using "$path/data/processed/temp_bec.dta", keep(3) nogen
	
	// gen new vars
	
	* gen firm's number of products
	drop N
	gen index = 0
	replace index = 1 if revenue != .
	bys siren year: egen N = total(index)
	
	* gen lagN
	xtset id year
	drop lagN
	gen lagN  = l1.N if l1.N != .
		
	* gen firm's number of new products
	drop n_c
	bys siren year: egen n_c = total(index_new)
	
	keep year siren N lagN n_c 
	duplicates drop
	
	egen id = group(siren)
		
	noi di _n "File `data_type': lagN "
	
	local N_list All 1
	if substr("`data_type'", 1, 5) == "pc10_" ///
		local N_list All 1 5 10 
	
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
		
		if "`N'" != "20" {
			replace conf = id_count if conf == .
			drop id_count dem num sh
			drop if new_prod == 0
			foreach var of varlist lnew_prod lsh `extra_conf_vars' {
				rename `var' `var'_bec6
			}
					
			save "$path/data/processed/temp_`data_type'_`N'_bec6.dta", replace
		} 
		
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
				
					
		use $path/data/processed/temp_`data_type'_`N'_base , clear
		merge 1:1 new_prod using $path/data/processed/temp_`data_type'_`N'_bec6, nogen

		sort new_prod
		
		foreach group_type in base bec6 {
		
			egen temp_`group_type' = max(lnew_prod_`group_type')
			gen max_`group_type' = lnew_prod_`group_type' == temp_`group_type' if lnew_prod_`group_type'  !=.
			drop temp_`group_type' 
			
		}			
			
		twoway 	(scatter lsh`conf_var'_base lnew_prod`conf_var'_base, mcolor(blue)  msize(medlarge)) 										///
				(lfit lsh_base lnew_prod_base if max_base != 1, lc(blue))	 	///		
				(scatter lsh`conf_var'_bec6 lnew_prod`conf_var'_bec6, mcolor(red) ms(smdiamond_hollow) msize(medlarge)) 										///
				(lfit lsh_bec6 lnew_prod_bec6 if max_bec6 != 1, lp(dash) lc(red)), 	///
				legend(order(1 "Baseline" 3 "Only Consumer Goods") ring(0) pos(2) size(*1.5)) ///
				xtitle(`x_title')  						///
				ytitle(`y_title')					///
				xlabel(`x_scale') ///
				ylabel(`y_scale_`N'')
				
			graph export "$path/output/figure_c9_lagN`N'.pdf", replace

			restore
		}
		
	}
	
}
	
foreach data_type in `data_types_list' {	
	foreach N in `N_list' {		
		erase $path/data/processed/temp_`data_type'_`N'_base.dta
		erase $path/data/processed/temp_`data_type'_`N'_bec6.dta
	}
} 

erase $path/data/processed/temp_bec.dta

	
