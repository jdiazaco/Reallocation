
* options graphs
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

// Preparation of additional datasets: keeping independend firms only
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
merge m:1 year siren using "$path/data/processed/LiFi.dta", keepusing(control no_change_mother no_change_Ngroup no_change_mother_Ngroup) keep(1) nogen
save "$path/data/processed/EAPIndep_20102019_OnlyEAP1_firm_clean.dta", replace

// Preparation of additional datasets: no new plants
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
merge 1:1 year siren using "$path/data/processed/dads_noplants.dta", keep(1 3) nogen
drop if new_plants > 0 & new_plants !=.
save "$path/data/processed/EAPNoNewPlants_20102019_OnlyEAP1_firm_clean.dta", replace


local data_types_list correspc10 Indep NoNewPlants



quietly {
	
	foreach data_type in `data_types_list' {
		
	* use raw data
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
	
	local N_list 1
	
	foreach N in `N_list' {
		
		noi di _continue "`N' "
		
		preserve
		
		local extra_xscale
		if "`N'" != "All" ///
			keep if lagN == `N'
		else ///
			local extra_xscale 4.1588831 "64" 4.8520303 "128"

		rename n_c new_prod // Number of new products
		
		* gen vars
		_rank_byvar id, by_var(new_prod)
		
		replace conf = id_count if conf == .
		drop id_count dem num sh
		drop if new_prod == 0
		foreach var of varlist lnew_prod lsh `extra_conf_vars' {
			rename `var' `var'_`data_type'
		}
					
		tempfile temp_`data_type'_`N'
		save `temp_`data_type'_`N'', replace
		
		restore
		
	}
	
	}
	
	foreach N in `N_list' {
		
		local extra_xscale
		if "`N'" == "All" ///
			local extra_xscale 4.1588831 "64" 4.8520303 "128"
		
		// Plot Figure
		local lbl_Indep "Only independent firms"
		local lbl_NoNewPlants "Exclude firms adding new plants"
		
		
		
		foreach group_type in Indep {
			
			use `temp_correspc10_`N'', clear
			merge 1:1 new_prod using `temp_`group_type'_`N'', nogen
			sort new_prod
			foreach data_type in correspc10 `group_type' {
				egen temp = max(lnew_prod_`data_type')
				gen max_`data_type' = lnew_prod_`data_type' == temp if lnew_prod_`data_type'  !=.
				drop temp			
			}
			
		
		twoway 	(scatter lsh`conf_var'_correspc10 lnew_prod`conf_var'_correspc10, mcolor(blue)  msize(medlarge)) 										///
				(lfit lsh_correspc10 lnew_prod_correspc10 if max_correspc10 != 1, lc(blue))	 	///			
				(scatter lsh`conf_var'_`group_type' lnew_prod`conf_var'_`group_type', mcolor(red) ms(smdiamond_hollow) msize(medlarge))					///
					(lfit lsh_`group_type' lnew_prod_`group_type' if max_`group_type' != 1, lp(dash) lc(red)), 	///
					legend(order(1 "Baseline" 3 "`lbl_`group_type''") ring(0) pos(2) size(*1.5)) ///
					xtitle(`x_title')  						///
					ytitle(`y_title')					///
					xlabel(`x_scale') ///
					ylabel(`y_scale_`N'') 
			
			graph export "$path/output/figure_5a.pdf", replace
		
		}
		
		
		
		
		foreach group_type in NoNewPlants { 
			
			use `temp_correspc10_`N'', clear
			merge 1:1 new_prod using `temp_`group_type'_`N'', nogen
			sort new_prod
			foreach data_type in correspc10 `group_type' {
				egen temp = max(lnew_prod_`data_type')
				gen max_`data_type' = lnew_prod_`data_type' == temp if lnew_prod_`data_type'  !=.
				drop temp			
			}
			
		
		twoway 	(scatter lsh`conf_var'_correspc10 lnew_prod`conf_var'_correspc10, mcolor(blue)  msize(medlarge)) 										///
				(lfit lsh_correspc10 lnew_prod_correspc10 if max_correspc10 != 1, lc(blue))	 	///			
				(scatter lsh`conf_var'_`group_type' lnew_prod`conf_var'_`group_type', mcolor(red) ms(smdiamond_hollow) msize(medlarge))					///
					(lfit lsh_`group_type' lnew_prod_`group_type' if max_`group_type' != 1, lp(dash) lc(red)), 	///
					legend(order(1 "Baseline" 3 "`lbl_`group_type''") ring(0) pos(2) size(*1.5)) ///
					xtitle(`x_title')  						///
					ytitle(`y_title')					///
					xlabel(`x_scale') ///
					ylabel(`y_scale_`N'') 
			
			graph export "$path/output/figure_5b.pdf", replace
		}
			
	}
	
}

erase "$path/data/processed/EAPIndep_20102019_OnlyEAP1_firm_clean.dta"
erase "$path/data/processed/EAPNoNewPlants_20102019_OnlyEAP1_firm_clean.dta"
