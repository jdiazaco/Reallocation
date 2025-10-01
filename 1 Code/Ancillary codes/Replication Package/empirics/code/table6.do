*** table 6


** preamble
global speclist FSQ BFSR NoFSQ NoFSR
ssc install outreg2, replace 
ssc install winsor 

** prep dataset 

* open data
use analysisdata/dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p37_XTABOND_andSTATA_DGM ,clear 

* merge with big data	
global dataset ficusfare_reduced_9419_sec2_year2_firm_va_sevyears_v0222
merge 1:1 firmsId year  using analysisdata/$dataset , force 

** drop non-manufacturing industries 
drop if naf2d_num >= 40 | naf2d_num < 10 

* clean up, keep treatment 2 
keep year firmsId naf2d *mu*8  *mu*1  naf_single catotal emp sal invcorp age siren immocor acha4 naf_single ms5d 

* merge with full panel (for profitability calculation) 
merge 1:1 firmsId year using rawdata/ficusfare_profil_wof_19942019_v0222 , keepusing(redi_r201)  
drop if _merge ==2 
drop _merge 
 
* other markup-relation variables 
xtset firmsId year 
gen laborshare   = sal    /catotal 
gen costshare    = acha4  /catotal
gen invintensity = invcorp/catotal 
gen invrate 	 = invcorp/l.immocor 
gen profit 		 = (catotal -redi_r201)/catotal 
		 
* winsorization
foreach var in   laborshare invintensity invrate profit costshare   ms5d age {
	winsor `var' , g(`var'_w) p(0.015)
}
replace ms5d_w   = ms5d_w * 100 

 
* balanced sample 
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
	
** Create a balanced panel (needed because of the logs) 

* balancing - exclude only the relevant series 
foreach var in l_mu_tl_m_tr_FSQ 	l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_FSQ  == . | l_mu_tl_m_BFSR == . | l_mu_tl_m_NoFSQ == . | l_mu_tl_m_NoFSR == . 
}

foreach var in l_mu_tl_m_tr_FSQ 	l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ  l_mu_tl_m_tr_NoFSR { 
replace `var' = . if l_mu_tl_m_tr_FSQ  == . | l_mu_tl_m_tr_BFSR == . | l_mu_tl_m_tr_NoFSQ == . | l_mu_tl_m_tr_NoFSR == . 
}

* standardize markups 
foreach var in l_mu_cd_m_tr_FSQ   l_mu_cd_m_tr_BFSR l_mu_cd_m_tr_NoFSQ l_mu_cd_m_tr_NoFSR /// 
			   l_mu_tl_m_tr_FSQ   l_mu_tl_m_tr_BFSR l_mu_tl_m_tr_NoFSQ l_mu_tl_m_tr_NoFSR ///
			   mu_cd_m_tr_FSQ 	    mu_cd_m_tr_BFSR   mu_cd_m_tr_NoFSQ   mu_cd_m_tr_NoFSR   ///
			   mu_tl_m_tr_FSQ       mu_tl_m_tr_BFSR   mu_tl_m_tr_NoFSQ   mu_tl_m_tr_NoFSR 	{ 
	qui: su `var'
	gen `var'_s = `var'/r(sd)  
 }
	
	

** perform analysis and export table  
* quantity row 
qui: xtreg profit_w 	l_mu_tl_m_tr_FSQ_s  i.year, fe robust
qui: outreg2  using output/table6_q 		, replace keep(l_mu_tl_m_tr_FSQ_s)
qui: xtreg laborshare_w l_mu_tl_m_tr_FSQ_s  i.year, fe robust
qui: outreg2  using output/table6_q 		, append keep(l_mu_tl_m_tr_FSQ_s)
qui: xtreg costshare_w 	l_mu_tl_m_tr_FSQ_s  i.year, fe robust
qui: outreg2  using output/table6_q 		, append keep(l_mu_tl_m_tr_FSQ_s)
qui: xtreg ms5d_w 		l_mu_tl_m_tr_FSQ_s  i.year, fe robust
qui: outreg2  using output/table6_q 		, append keep(l_mu_tl_m_tr_FSQ_s)
* revenue row 
qui: xtreg profit_w 	l_mu_tl_m_tr_BFSR_s  i.year, fe robust
qui: outreg2  using output/table6_r 		, replace keep(l_mu_tl_m_tr_BFSR_s)
qui: xtreg laborshare_w l_mu_tl_m_tr_BFSR_s  i.year, fe robust
qui: outreg2  using output/table6_r 		, append keep(l_mu_tl_m_tr_BFSR_s)
qui: xtreg costshare_w 	l_mu_tl_m_tr_BFSR_s  i.year, fe robust
qui: outreg2  using output/table6_r 		, append keep(l_mu_tl_m_tr_BFSR_s)
qui: xtreg ms5d_w 		l_mu_tl_m_tr_BFSR_s  i.year, fe robust
qui: outreg2  using output/table6_r 		, append keep(l_mu_tl_m_tr_BFSR_s)







