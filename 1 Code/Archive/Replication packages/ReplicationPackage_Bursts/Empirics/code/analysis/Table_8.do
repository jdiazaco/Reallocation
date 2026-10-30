
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear

* lagged product count 
gen lag_9_N  = l9.N

* cumulative increase in size 
gen Nchange_lag9 = N-lag_9_N

** total number of products added 
bysort siren: egen total_n_c= total(n_c)
gen flag = n_c> 0.5*total_n_c & n_c != . 
bysort siren: egen flag_total = total(flag)
replace flag_total = (flag_total>0 & flag_total !=. ) 

su flag_total if Nchange_lag9 != . & total_n_c/lag_9_N> 5 
matrix collect = r(mean)  
su flag_total if Nchange_lag9 != . & total_n_c/lag_9_N> 10 
matrix collect = collect  \r(mean) 
su flag_total if Nchange_lag9 != . & total_n_c/lag_9_N> 20 
matrix collect = collect  \r(mean) 

su flag_total if Nchange_lag9 != . & total_n_c> 5   & lag_9_N == 1  
matrix collect_1 = r(mean)  
su flag_total if Nchange_lag9 != . & total_n_c> 10  & lag_9_N == 1 
matrix collect_1 = collect_1  \r(mean) 
su flag_total if Nchange_lag9 != . & total_n_c> 20  & lag_9_N == 1 
matrix collect_1 = collect_1  \r(mean) 

matrix collect = collect , collect_1 

putexcel set "$path/output/table_8" , replace 
putexcel A1 =  matrix(collect)

 





