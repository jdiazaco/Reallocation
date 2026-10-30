
** open data  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean", replace

** create standard variable names 
rename lagN  lagN_1
rename n_c   n_c_1 
rename n_d   n_d_1 
forval i = 1/1 {
	gen n_c_`i'_rate  = n_c_`i'/lagN_`i'   /`i'
	gen n_d_`i'_rate  = n_d_`i'/lagN_`i'  // `i'
}

** merge in the R&D data 
merge 1:1 year siren using "$path/data/processed/gecir_rd_20102019.dta"
drop if _merge == 2 //drop firms outside of manufacturing 
replace rd_exp_value = 0 if _merge  == 1 //admin data 
drop _merge 
merge 1:1 year siren using "$path/data/processed/dads_rd_20102019.dta", keep(3) keepusing(s_brut_rd)

* calculate R&D rates 
replace rd_exp_value = rd_exp_value/1000000
replace s_brut_rd = s_brut_rd/1000000

* calculate R&D rates
forval i = 1/1 /*5*/ {
	gen rd_`i'_rate  = rd_exp_value / lagN_`i'
	gen dads_`i'_rate  = s_brut_rd / lagN_`i'
}

* demeaning by industry 
foreach var in  rd_1_rate dads_1_rate laglN {
	//winsor `var', g(temp1) p(0.01)
	qui: reghdfe `var' , absorb(nace4_rev2#year) resid 
	predict residual, resid 
	gen  `var'_fit =  residual 
	qui: su `var'
	drop   residual 
}



*** Produce output 

	
* run ppml regression - DADS
ppmlhdfe s_brut_rd lN , cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", replace tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe s_brut_rd lN if lN> 1, cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')	
ppmlhdfe s_brut_rd lN, absorb(nace4_rev2#year) cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe s_brut_rd lN if lN> 1, absorb(nace4_rev2#year) cluster(siren)  
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')		


* run ppml regression - product count weighted DADS
ppmlhdfe s_brut_rd lN [pweight=lN], cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe s_brut_rd lN [pweight=lN] if lN> 1, cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')	
ppmlhdfe s_brut_rd lN [pweight=lN], absorb(nace4_rev2#year) cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe s_brut_rd lN [pweight=lN] if lN> 1, absorb(nace4_rev2#year) cluster(siren)  
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_top", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')		

	

	
	
* run ppml regression 
ppmlhdfe rd_exp_value lN , cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", replace tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe rd_exp_value lN if lagN> 1, cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')	
ppmlhdfe rd_exp_value lN, absorb(nace4_rev2#year) cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe rd_exp_value lN if lagN> 1, absorb(nace4_rev2#year ) cluster(siren)  
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')		


* run ppml regression - product count weighted 
ppmlhdfe rd_exp_value lN [pweight=lN], cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe rd_exp_value lN [pweight=lN] if lN> 1, cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')	
ppmlhdfe rd_exp_value lN [pweight=lN], absorb(nace4_rev2#year) cluster(siren year) 
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe rd_exp_value lN [pweight=lN] if lN> 1, absorb(nace4_rev2#year) cluster(siren)  
test _b[lN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_d1_bottom", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')		


erase "$path/output/table_d1_top.txt"	
erase "$path/output/table_d1_bottom.txt"
