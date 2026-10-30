
* options graphs
local y_scale 0 "10{superscript:-0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medium)
local x_scale 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.1588831 "64" 4.8520303 "128" 5.5451775 "256", labsize(medium)
local y_title `""Rank", size(medlarge)"' // Share of firms with products {it:{&ge} n}
local x_title `""Number of products {it:n}", size(medlarge)"'

local conf_var // variable names for the scatter graph (different in case of confidentiality checks)
local extra_conf_vars
if ${conf_check} == 1 {		
	local conf_var _av
	local extra_conf_vars conf bin lsh_av lprod_av
}

local data_types_list correspc10 


quietly {
	
foreach data_type in `data_types_list' {
	
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
	
	foreach year of numlist 2019 {
		
		preserve
		
		keep if year == `year'
		rename N prod // Number of products
		
		_rank_byvar id, by_var(prod)
		
		if "`data_type'" == "correspc10" {
			
			twoway 	(scatter lsh`conf_var' lprod`conf_var' if lprod != ., mcolor(blue) msize(medlarge)) /// 						
					(lfit lsh lprod if _n!=_N, lcolor(gs10)), 	/// 
					legend(off) xtitle(`x_title') 						///
					ytitle(`y_title')					/// 
					xlabel(`x_scale') ///
					ylabel(`y_scale') `slope'	
			
			graph export "$path/output/figure_1a.pdf", replace
		
		}

		restore
		
	}

}
	
}