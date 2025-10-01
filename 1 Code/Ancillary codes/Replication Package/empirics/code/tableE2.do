***Table E2 repeats table 4 for BB  


***** 1. Preamble ******

*specifications 
global speclist FSQ  BFSR NoFSQ NoFSR


***** 2. Load the data ******

*load the markups data and merge to be compared 
use "analysisdata\data_smallest_for_python_p37_XTABOND", clear 

destring naf2d, replace

* merge in all of the markup estimates 
merge 1:1 firmsId year using "analysisdata\dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p37_XTABOND_andSTATA_DGM"


** drop non-manufacturing industries 
drop if naf2d_num >= 40 | naf2d_num < 10 



********************************************************************* Select the markup
foreach specs in $speclist  {

*raw data, no outlier treatment
gen mu_cd_m_`specs' = mu_sepcal_CD_s`specs'_t1
gen mu_tl_m_`specs' = mu_sepcal_TL_s`specs'_t1

*outlier treatment
gen mu_cd_m_tr_`specs' = mu_sepcal_CD_s`specs'_t8
gen mu_tl_m_tr_`specs' = mu_sepcal_TL_s`specs'_t8

}



*computing log
foreach specs in $speclist  {

*raw data, no outlier treatment
gen l_mu_cd_m_`specs' = ln(mu_cd_m_`specs')	
gen l_mu_tl_m_`specs' = ln(mu_tl_m_`specs')

*outlier treatment	
gen l_mu_cd_m_tr_`specs' = ln(mu_cd_m_tr_`specs')	
gen l_mu_tl_m_tr_`specs' = ln(mu_tl_m_tr_`specs')


}



*** MAIN ANALYSIS Create a balanced panel (needed because of the logs) 

* balancing - exclude only the relevant series 
foreach var in l_mu_tl_m_tr_FSQ 	l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_FSQ  == . | l_mu_tl_m_BFSR == . | l_mu_tl_m_NoFSQ == . | l_mu_tl_m_NoFSR == . 
}

foreach var in l_mu_tl_m_tr_FSQ 	l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_tr_FSQ  == . | l_mu_tl_m_tr_BFSR == . | l_mu_tl_m_tr_NoFSQ == . | l_mu_tl_m_tr_NoFSR == . 
}


* obtain sector level information 
bysort naf2d_num: egen mean_q = mean(l_mu_tl_m_tr_NoFSQ)
bysort naf2d_num: egen mean_r = mean(l_mu_tl_m_tr_NoFSR) 
gen wedge = l_mu_tl_m_tr_NoFSQ-l_mu_tl_m_tr_NoFSR 
gen wedge_norm =  (l_mu_tl_m_tr_NoFSQ-mean_q) - (l_mu_tl_m_tr_NoFSR-mean_r)
gen l_mu_tl_m_tr_NoFSQ_norm = l_mu_tl_m_tr_NoFSQ-mean_q
gen l_mu_tl_m_tr_NoFSR_norm  = l_mu_tl_m_tr_NoFSR-mean_r

* get some measure of uncertainty 
egen indcount = group(naf2d_num)
qui: su indcount
local max = r(max)
forval ind = 1/`max' { 
	//run bootstrap 
	bootstrap r(mean) r(sd) r(p50) r(p25) r(p75), reps(100): summarize l_mu_tl_m_tr_NoFSQ if indcount == `ind' ,d
	//save results
	matrix results = r(table)
	preserve
		clear
		svmat results
		keep if _n < 3
		gen var = 1
		replace var = 2 if _n ==2 
		gen industry = `ind'
		rename results1 mean
		rename results2 sd
		rename results3 p50
		rename results4 p25
		rename results5 p75
		save temp/moments_ind_`ind', replace 
	restore 
	
	//run bootstrap 
	bootstrap r(mean) r(sd) r(p50) r(p25) r(p75), reps(100): summarize l_mu_tl_m_tr_NoFSR if indcount == `ind' ,d
	//save results
	matrix results = r(table)
	preserve
		clear
		svmat results
		keep if _n < 3
		gen var = 1
		replace var = 2 if _n ==2 
		gen industry = `ind'
		rename results1 mean
		rename results2 sd
		rename results3 p50
		rename results4 p25
		rename results5 p75
		save temp/moments_ind_`ind'_r, replace 
	restore 
}

* combine the datasets
qui: su indcount
local max = r(max) 
preserve 
	use temp/moments_ind_1, clear 
	forval ind = 2/`max' {
		append using temp/moments_ind_`ind'
	}
	* produce table 
	reshape wide mean sd p50 p25 p75 , j(var) i(industry)

	** produce main moments 
	su mean1  
	local mean= r(mean)
	matrix results = round(r(mean), 0.001)
	su sd1 
	matrix results = results, round(r(mean), 0.001)
	su p501 
	matrix results = results ,round(r(mean)-`mean', 0.001)
	su p251 
	matrix results = results ,round(r(mean)-`mean', 0.001)
	su p751 
	matrix results = results ,round(r(mean)-`mean', 0.001)
	matrix results_quant = results 
	su mean2  
	local mean= r(mean)
	matrix results = round(r(mean), 0.001)
	su sd2 
	matrix results = results, round(r(mean), 0.001)
	su p502 
	matrix results = results ,round(r(mean), 0.001)
	su p252 
	matrix results = results ,round(r(mean), 0.001)
	su p752 
	matrix results = results ,round(r(mean), 0.001)
	matrix results_quant_sd = results 

restore 


qui: su indcount
local max = r(max) 
preserve 
	use temp/moments_ind_1_r, clear 
	forval ind = 2/`max' {
		append using temp/moments_ind_`ind'_r
	}
	* produce table 
	reshape wide mean sd p50 p25 p75 , j(var) i(industry)

	** produce main moments 
	su mean1  
	local mean= r(mean)
	matrix results = round(r(mean), 0.001)
	su sd1 
	matrix results = results, round(r(mean), 0.001)
	su p501 
	matrix results = results ,round(r(mean)-`mean', 0.001)
	su p251 
	matrix results = results ,round(r(mean)-`mean', 0.001)
	su p751 
	matrix results = results ,round(r(mean)-`mean', 0.001)
	matrix results_rev = results 
	su mean2  
	local mean= r(mean)
	matrix results = round(r(mean), 0.001)
	su sd2 
	matrix results = results, round(r(mean), 0.001)
	su p502 
	matrix results = results ,round(r(mean), 0.001)
	su p252 
	matrix results = results ,round(r(mean), 0.001)
	su p752 
	matrix results = results ,round(r(mean), 0.001)
	matrix results_rev_sd = results 

restore 

** export 
matrix results = results_quant\results_quant_sd\results_rev\results_rev_sd
putexcel set output/tableE2, replace
putexcel A1 = matrix(results)








	
