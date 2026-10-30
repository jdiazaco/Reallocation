
foreach var in revenue employees_fte N n_c rev_per_N { 
	
	* use raw data
	use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
	
	keep if year == 2019
	gen rev_per_N = revenue/N
	gen x_var = `var' 
		
	* tail 
	qui: su x_var, d
	drop if x_var < r(p90)
	 
	* prep data
	keep x_var 
	drop if x_var == . 
	gsort -x_var
	gen rank = _n 
	
	* rescaling
	replace x_var = round(x_var)
	//replace rank = rank
	bysort x_var: egen rank_m = max(rank)
	duplicates drop x_var, force 

	* create reg vars
	gen log_x_var = log(x_var)
	gen log_rank_min05 =  log(rank_m-0.5)
	gen neg_log_rank_min05 = -log_rank_min05
	
	* gen tail index
	reg neg_log_rank_min05 log_x_var
	
	* label var
	label var log_x_var "log(Size)"
	label var log_rank_min05 "log(Rank)"
	label var neg_log_rank_min05 "-log(Rank)"
	local label_y_var "log(`var')"
	
	* export table
	outreg2 using "$path/output/table_c3", append label tex(frag) nocons noobs ctitle("`label_y_var'")

}

erase "$path/output/table_c3.txt"