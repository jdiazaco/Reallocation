
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

local data_types_list correspc10 prodfra10 pc8 pc6


quietly {
	
	foreach data_type in `data_types_list' {
		
	* use raw data
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
	
	local N_list 5 10 
	
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
		
		// Figure: different level of aggregation
		use `temp_correspc10_`N'', clear
		merge 1:1 new_prod using `temp_prodfra10_`N'', nogen
		merge 1:1 new_prod using `temp_pc8_`N'', nogen
		merge 1:1 new_prod using `temp_pc6_`N'', nogen
		sort new_prod
		foreach data_type in correspc10 prodfra10 pc8 pc6 {
			egen temp = max(lnew_prod_`data_type')
			gen max_`data_type' = lnew_prod_`data_type' == temp if lnew_prod_`data_type'  !=.
			drop temp 
			if "`option'" == "slopes" {
				reg lsh_`data_type' lnew_prod_`data_type' if max_`data_type' != 1 
				local beta_`data_type': di %12.2f _b[lnew_prod]
				local se_`data_type': di %12.2f _se[lnew_prod]	
				local beta_`data_type' = `beta_`data_type''
				local se_`data_type' = `se_`data_type''					
			}			
		}

		twoway 	(scatter lsh`conf_var'_correspc10 lnew_prod`conf_var'_correspc10, mcolor(blue)  msize(medlarge)) 										///
				(lfit lsh_correspc10 lnew_prod_correspc10 if max_correspc10 != 1, lc(blue))	 															///		
				(scatter lsh`conf_var'_prodfra10 lnew_prod`conf_var'_prodfra10, mcolor(sienna) ms(smsquare_hollow) msize(medlarge))						///
				(lfit lsh_prodfra10 lnew_prod_prodfra10 if max_prodfra10 != 1, lp(shortdash) lc(sienna))	 											///
				(scatter lsh`conf_var'_pc8 lnew_prod`conf_var'_pc8, mcolor(red) ms(smdiamond_hollow) msize(medlarge))									///
				(lfit lsh_pc8 lnew_prod_pc8 if max_pc8 != 1, lp(dash) lc(red)) 																			///
				(scatter lsh`conf_var'_pc6 lnew_prod`conf_var'_pc6, mcolor(green) ms(triangle_hollow) msize(medlarge))									///
				(lfit lsh_pc6 lnew_prod_pc6 if max_pc6 != 1, lp(dash_dot) lc(green)), 																	///
				legend(order(1 "10c-digit" 3 "10-digit" 5 "8-digit" 7 "6-digit") ring(0) pos(2) size(*1.5)) 											///
				xtitle(`x_title')  																														///
				ytitle(`y_title')																														///
				xlabel(`x_scale') 																														///
				ylabel(`y_scale_`N'')
			
		graph export "$path/output/figure_c3_lagN`N'.pdf", replace
	
	
			
	}
	
}
