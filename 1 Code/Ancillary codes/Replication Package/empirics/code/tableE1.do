*** table E1 


** Preamble

global speclist FSQ  BFSR  NoFSQ NoFSR


** Open dataset 
use analysisdata/dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p37_XTABOND_andSTATA_DGM, clear 
	

** keep sectors with positive markups: create a balanced panel (needed because of the logs) 

foreach specs in $speclist  {

*raw data, no outlier treatment
gen mu_cd_m_`specs' = mu_sepcal_CD_s`specs'_t1
gen mu_tl_m_`specs' = mu_sepcal_TL_s`specs'_t1

*outlier treatment
gen mu_cd_m_tr_`specs' = mu_sepcal_CD_s`specs'_t8
gen mu_tl_m_tr_`specs' = mu_sepcal_TL_s`specs'_t8

}


** drop non-manufacturing industries 
drop if naf2d >= 40 | naf2d < 10 | naf2d == 30 //drop 30: outlier (confidentiality)

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
foreach var in l_mu_tl_m_tr_FSQ 	l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_FSQ  == . | l_mu_tl_m_BFSR == . | l_mu_tl_m_NoFSQ == . | l_mu_tl_m_NoFSR == . 
}

foreach var in l_mu_tl_m_tr_FSQ 	l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_tr_FSQ  == . | l_mu_tl_m_tr_BFSR == . | l_mu_tl_m_tr_NoFSQ == . | l_mu_tl_m_tr_NoFSR == . 
}



** build table 
qui: su elast_tl_sFSQ_t8   
matrix results_temp1 = round(r(mean),0.01)
matrix results_temp2 = round(r(sd)  ,0.01)	

qui: su elast_tl_sNoFSQ_t8   
matrix results_temp1 = results_temp1, round(r(mean),0.01)
matrix results_temp2 = results_temp2, round(r(sd)  ,0.01)	

qui: su elast_tl_sBFSR_t8   
matrix results_temp1 = results_temp1, round(r(mean),0.01)
matrix results_temp2 = results_temp2, round(r(sd)  ,0.01)	
 
qui: su elast_tl_sNoFSR_t8   
matrix results_temp1 = results_temp1, round(r(mean),0.01)
matrix results_temp2 = results_temp2, round(r(sd)  ,0.01)	

matrix results_materials = results_temp1 
matrix results_materials = results_materials\ results_temp2 

//industry-specific results
egen ind2 = group(naf2d) 
qui: su ind2 
forval ind = 1/`r(max)' {

qui: su elast_tl_sFSQ_t8       if ind2==`ind' 
matrix results_temp1 = round(r(mean),0.01)
matrix results_temp2 = round(r(sd)  ,0.01)	

qui: su elast_tl_sNoFSQ_t8    if ind2==`ind'   
matrix results_temp1 = results_temp1, round(r(mean),0.01)
matrix results_temp2 = results_temp2, round(r(sd)  ,0.01)	
 
qui: su elast_tl_sBFSR_t8      if ind2==`ind' 
matrix results_temp1 = results_temp1, round(r(mean),0.01)
matrix results_temp2 = results_temp2, round(r(sd)  ,0.01)	

qui: su elast_tl_sNoFSR_t8    if ind2==`ind'  
matrix results_temp1 = results_temp1, round(r(mean),0.01)
matrix results_temp2 = results_temp2, round(r(sd)  ,0.01)	

matrix results_materials = results_materials\ results_temp1 
matrix results_materials = results_materials\ results_temp2

}

//assign names to matrix 
matrix colnames results_materials =  "FSQ (TL)" "NoFSQ (TL)"  "BFSR (TL)" "NoFSR (TL)" 
preserve 
	duplicates drop naf2d, force
	sort naf2d 
	qui: su ind2 
	forval i= 1/`r(max)' {
		local c`i' = naf2d[`i']
	} 
restore
disp `c1'
qui: su ind2
if "`r(max)'"  == "8" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " 
} 
if "`r(max)'"  == "9" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " "  
} 
if "`r(max)'"  == "10" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" 
} 
if "`r(max)'"  == "11" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " "
} 
if "`r(max)'"  == "12" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " 
} 
if "`r(max)'"  == "13" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" 
} 
if "`r(max)'"  == "14" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " 
} 
if "`r(max)'"  == "15" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " " "`c15'" " "
} 
if "`r(max)'"  == "16" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " " "`c15'" " " "`c16'" " " 
} 
if "`r(max)'"  == "17" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " " "`c15'" " " "`c16'" " " "`c17'" " " 
} 
if "`r(max)'"  == "18" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " " "`c15'" " " "`c16'" " " "`c17'" " " "`c18'" " " 
} 
if "`r(max)'"  == "19" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " " "`c15'" " " "`c16'" " " "`c17'" " " "`c18'" " " "`c19'" " " 
} 
if "`r(max)'"  == "20" { 
	matrix rownames results_materials = "All (average)" ""  "`c1'" " " "`c2'" " " "`c3'" " " "`c4'" " " "`c5'" " " "`c6'" " " "`c7'" " " "`c8'" " " "`c9'" " " "`c10'" "" "`c11'" " " "`c12'" " " "`c13'" "" "`c14'" " " "`c15'" " " "`c16'" " " "`c17'" " " "`c18'" " " "`c19'" " " "c`20'" " " 
}  


//place in excel 
putexcel set output/tableE1, replace 
putexcel A1 = matrix(results_materials) , names   






















