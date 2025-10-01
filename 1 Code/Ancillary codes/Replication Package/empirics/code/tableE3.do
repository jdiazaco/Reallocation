*** table E3

***** 1. Preamble ******

*specifications 
global speclist FSQ  BFSR NoFSQ NoFSR

*trimming level
global top_tr 985
global bot_tr 15
 

 

***** 2. Load the data ******

*load the markups data and merge to be compared 
use "analysisdata\data_smallest_for_python_p37_XTABOND", clear 
destring naf2d, replace

* merge in all of the markup estimates 
merge 1:1 firmsId year using "analysisdata\dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p37_XTABOND_andSTATA_DGM"


** drop non-manufacturing industries 
drop if naf2d_num >= 40 | naf2d_num < 10 

** specify markup series 
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



*** Create a balanced panel (needed because of the logs) 

* balancing - exclude only the relevant series 
foreach var in l_mu_tl_m_tr_FSQ l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_tr_FSQ  == . | l_mu_tl_m_tr_BFSR == . | l_mu_tl_m_NoFSQ == . | l_mu_tl_m_NoFSR == . 
}

foreach var in l_mu_tl_m_tr_FS l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_tr_FSQ  == . | l_mu_tl_m_tr_BFSR == . | l_mu_tl_m_tr_NoFSQ == . | l_mu_tl_m_tr_NoFSR == . 
}


*** calculate FD incl same outlier treatment 
xtset firmsId year
local top_tr $top_tr
local bot_tr $bot_tr
foreach specs in $speclist  {
	foreach pf in  tl {

		gen FD_mu_`pf'_m_tr_`specs' = mu_`pf'_m_tr_`specs' -L.mu_`pf'_m_tr_`specs'
		gen FD_mu_`pf'_m_`specs' = mu_`pf'_m_`specs' -L.mu_`pf'_m_`specs'
		
		gen FD_l_mu_`pf'_m_tr_`specs' = l_mu_`pf'_m_tr_`specs' -L.l_mu_`pf'_m_tr_`specs'
		gen FD_l_mu_`pf'_m_`specs' = l_mu_`pf'_m_`specs' -L.l_mu_`pf'_m_`specs'
		
		fastxtile mtile_FD_mu_`pf'_m_`specs' =  FD_mu_`pf'_m_`specs', nq(1000)
		fastxtile mtile_FD_l_mu_`pf'_m_`specs' =  FD_l_mu_`pf'_m_`specs', nq(1000)
		
		gen FD_mu_`pf'_m_`specs'_tr = . 
		replace FD_mu_`pf'_m_`specs'_tr= FD_mu_`pf'_m_`specs' if mtile_FD_mu_`pf'_m_`specs'<`top_tr'  & mtile_FD_mu_`pf'_m_`specs'>`bot_tr'
		
		gen FD_l_mu_`pf'_m_`specs'_tr = . 
		replace FD_l_mu_`pf'_m_`specs'_tr= FD_l_mu_`pf'_m_`specs' if mtile_FD_l_mu_`pf'_m_`specs'<`top_tr'  & mtile_FD_l_mu_`pf'_m_`specs'>`bot_tr'
		
		qui: reg l_mu_`pf'_m_tr_`specs' i.naf2d 
		predict Secpurge_l_mu_`pf'_m_tr_`specs'  ,r  
		gen FD_Secpurge_l_mu_`pf'_m_tr_`specs'  = Secpurge_l_mu_`pf'_m_tr_`specs'  - L.Secpurge_l_mu_`pf'_m_tr_`specs' 
		
	}
}



*** produce new table
pwcorr l_mu_tl_m_tr_NoFSQ   l_mu_tl_m_tr_NoFSR   
local corr1 = round(r(rho),.01)
pwcorr FD_l_mu_tl_m_tr_NoFSQ   FD_l_mu_tl_m_tr_NoFSR  
local corr2 = round(r(rho),.01)
spearman l_mu_tl_m_tr_NoFSQ   l_mu_tl_m_tr_NoFSR   
local corr3 = round(r(rho),.01)
spearman FD_l_mu_tl_m_tr_NoFSQ   FD_l_mu_tl_m_tr_NoFSR   
local corr4 = round(r(rho),.01)
statsby, by(naf2d_num) saving("temp\correlation.dta", replace): pwcorr 	   l_mu_tl_m_tr_NoFSQ      l_mu_tl_m_tr_NoFSR  
statsby, by(naf2d_num) saving("temp\correlation_FD.dta", replace): pwcorr 	FD_l_mu_tl_m_tr_NoFSQ   FD_l_mu_tl_m_tr_NoFSR
statsby, by(naf2d_num) saving("temp\correlation_spearman.dta", replace): spearman 	   l_mu_tl_m_tr_NoFSQ      l_mu_tl_m_tr_NoFSR  
statsby, by(naf2d_num) saving("temp\correlation_spearman_FD.dta", replace): spearman   FD_l_mu_tl_m_tr_NoFSQ   FD_l_mu_tl_m_tr_NoFSR 


*** combine estimates and produce table
use temp\correlation.dta , clear 
qui: su rho ,d 
matrix results =  [round(r(mean),0.01), round(r(sd),0.01), round(r(p50),0.01), round(r(p25),0.01), round(r(p75),0.01), `corr1'] 
use temp\correlation_FD.dta , clear 
qui: su rho ,d 
matrix results =  results\ [round(r(mean),0.01), round(r(sd),0.01), round(r(p50),0.01), round(r(p25),0.01), round(r(p75),0.01), `corr2'] 
use temp\correlation_spearman.dta , clear 
qui: su rho ,d 
matrix results = results\ [round(r(mean),0.01), round(r(sd),0.01), round(r(p50),0.01), round(r(p25),0.01), round(r(p75),0.01), `corr3'] 
use temp\correlation_spearman_FD.dta , clear 
qui: su rho ,d 
matrix results =  results\[round(r(mean),0.01), round(r(sd),0.01), round(r(p50),0.01), round(r(p25),0.01), round(r(p75),0.01),`corr4'] 



matrix rownames results = "Pearson" "PearsonFD" "Spearman" "SpearmanFD"
matrix colnames results = "Avg" "StDev" "Median" "25p" "75p" "Pooled"

*** export
putexcel set output/tableE3, replace 
putexcel A1 = matrix(results)    , names 

 
	
