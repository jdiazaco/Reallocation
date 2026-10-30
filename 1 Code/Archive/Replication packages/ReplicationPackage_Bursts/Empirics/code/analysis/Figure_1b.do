
* options graphs
local y_scale 0 "10{superscript:-0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medium)
local y_title `""Rank", size(medlarge)"' // Share of firms with products {it:{&ge} n}

local conf_var // variable names for the scatter graph (different in case of confidentiality checks)
local extra_conf_vars
if ${conf_check} == 1 {		
	local conf_var _av
}

local data_type correspc10
local year 2019

quietly {
		
	use "$path/data/processed/EAP`data_type'_20102019_OnlyEAP1_firm_clean.dta", clear
	
	keep if year == `year'	
	gen rev_prod = revenue/N 	
	
	/***********************/
	/* Revenue per product */
	/***********************/
	
	preserve
	
	_rank_byvar id, by_var(rev_prod)		

	twoway 	(scatter lsh`conf_var' lrev_prod`conf_var' if lrev_prod != ., mcolor(blue)  msize(medlarge)), /// 						
			legend(off) xtitle("Revenue per product", size(medlarge)) 						///  
			ytitle(`y_title')					/// 
			xlabel(0 "10{superscript:-0}" 2.3025851  "10{superscript:1}" 4.6051702 "10{superscript:2}" 6.9077553 "10{superscript:3}" 9.2103404 "10{superscript:4}" 11.512925 "10{superscript:5}" 13.81551 "10{superscript:6}" 16.1181 "10{superscript:7}", labsize(medium)) ///
			ylabel(`y_scale')
	
	graph export "$path/output/figure_1b.pdf", replace		
	
	restore
	
}