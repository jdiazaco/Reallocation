
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

// Preparation of additional datasets: innovative vs non innovative firms
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
sum lagn_c, d
gen innov = (lagn_c > 0)
replace innov = . if lagn_c == .
drop if innov == .
preserve
keep if innov == 1
save "$path/data/processed/EAPcorrespc10_Innov1_20102019_OnlyEAP1_firm_clean.dta", replace
restore
keep if innov == 0
save "$path/data/processed/EAPcorrespc10_Innov0_20102019_OnlyEAP1_firm_clean.dta", replace


local data_types_list Innov1 Innov0



quietly {
	
	foreach data_type in `data_types_list' {
		
	* use raw data
	use "$path/data/processed/EAPcorrespc10_`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
	
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
		
		// Plot figure
		local lbl_Innov0 "Non Innovative firms in t-1"
		local lbl_Innov1 "Innovative firms in t-1"

		foreach group_type in Innov {
			
		use `temp_`group_type'0_`N'', clear
		merge 1:1 new_prod using `temp_`group_type'1_`N'', nogen
		sort new_prod
		
		foreach data_type in `group_type'0 `group_type'1 {
			
			egen temp = max(lnew_prod_`data_type')
			gen max_`data_type' = lnew_prod_`data_type' == temp if lnew_prod_`data_type'  !=.
			drop temp
			
		}
	
		twoway 	(scatter lsh`conf_var'_`group_type'0 lnew_prod`conf_var'_`group_type'0, mcolor(blue)  msize(medlarge)) 										///
				(lfit lsh_`group_type'0 lnew_prod_`group_type'0 if max_`group_type'0 != 1, lc(blue))	 	///			
				(scatter lsh`conf_var'_`group_type'1 lnew_prod`conf_var'_`group_type'1, mcolor(red) ms(smdiamond_hollow) msize(medlarge))					///
				(lfit lsh_`group_type'1 lnew_prod_`group_type'1 if max_`group_type'1 != 1, lp(dash) lc(red)), 	///
				legend(order(1 "`lbl_`group_type'0'" 3 "`lbl_`group_type'1'") ring(0) pos(2) size(*1.5)) ///
				xtitle(`x_title')  						///
				ytitle(`y_title')					///
				xlabel(`x_scale') ///
				ylabel(`y_scale_`N'')
		
		graph export "$path/output/figure_c7_lagN`N'.pdf", replace
		
		}
			
	}
	
}

erase "$path/data/processed/EAPcorrespc10_Innov0_20102019_OnlyEAP1_firm_clean.dta"
erase "$path/data/processed/EAPcorrespc10_Innov1_20102019_OnlyEAP1_firm_clean.dta"