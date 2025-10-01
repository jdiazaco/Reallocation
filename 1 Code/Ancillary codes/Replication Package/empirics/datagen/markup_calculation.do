*** calculate markups from production function estimates 


*** preamble
* select specifications 
global speclist FSQ BFSR NoFSQ NoFSR 

*trimming level (thousands)
global toptr 985
global bottr 15
global winlevel 020

* select treatment list 
global treatlist  1 8  10 
local code 37

* choose dataset 
global dataset ficusfare_reduced_9419_sec2_year2_firm_va_sevyears_v0222
	
* open original firm panel 
use analysisdata/$dataset , clear 
destring naf2d, replace 

* define production variable, clean missing obs 
qui: gen s = ln(catotalR)
qui: gen m = ln(acha4R)	//m will be the variable input 
qui: gen v = ln(salR)
qui: gen k = ln(immocorR)
qui: gen o = ln(autachaR)
qui: gen ratio = catotal/acha4 
xtset firmsId year
gen drop = 0 
foreach var in s m v k o { 
	replace drop = 1 if `var' ==.  
	replace drop = 1 if l.`var' ==. 
}
drop if drop == 1 

* winsorize the production variables 
foreach var in  s m v k o catotal acha4 ratio {
	qui: gen `var'_w_full = .
	egen ind2 = group(naf2d) if `var' != . 
	qui: su ind2 
	forval ind = 1/`r(max)' { 
	//foreach ind in 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 {
		disp "winsorizing `var' industry count `ind' "
		qui: winsor `var' if ind2==`ind', p(.$winlevel) gen(`var'_temp)
		qui: replace `var'_w_full  = `var'_temp if ind2==`ind'
		qui: drop `var'_temp
	}
	drop ind2 
	label var `var'_w_full "`var' win $winlevel full sample"
}

*** obtain estimated coefficients 
capture drop naf2d_num
gen naf2d_num  = naf2d
foreach specs in $speclist  {
	//merge coefficients 
	if  "`specs'" == "NoFSQ" {
		merge m:1 naf2d using  "analysisdata\coefficients_byind_Python_BBQ_p`code'.dta", nogen
	}
	else if "`specs'" == "NoFSR" {
		merge m:1 naf2d using  "analysisdata\coefficients_byind_Python_BBR_p`code'.dta", nogen
	}
	else if "`specs'" == "FSQ" {
		merge m:1 naf2d_num using  "analysisdata\coefficients_byind_dgm_outputfirm_obs.dta", nogen
	}
	else if "`specs'" == "BFSR" {
		merge m:1 naf2d_num using "analysisdata\coefficients_byind_dgm_revenuefirm_obs.dta", nogen
	}
	//rename coefficients 
	foreach beta in  m_cd v_cd o_cd k_cd  m_tl v_tl o_tl k_tl m2_tl mv_tl mo_tl mk_tl v2_tl vo_tl vk_tl o2_tl ok_tl k2_tl {
		rename beta_`beta' beta_`beta'_`specs'
	}
	//drop redundant variables
	capture drop N med_CD med_TL iqr_CD iqr_TL  rho_cd  rho_tl 
}



*** Calculate the markups
foreach specs in $speclist {
	foreach treatment in $treatlist {
		//say where we are 
		disp("spec `specs' and treatment `treatment'")
		//define variables for elasticity calculation 
		if "`treatment'" == "1" | "`treatment'" == "2" | "`treatment'" == "4" | "`treatment'" == "5"   { 
			foreach var in m v o k {
				qui: gen `var'_s`specs'_t`treatment' = `var'
			}
		}
		else { //these take winsorized variables for input elasticity
			foreach var in m v o k {
				qui: gen `var'_s`specs'_t`treatment' = `var'_w_full 
			}
		}
		
		//calculate elasticity - TL 
		qui: gen elast_tl_s`specs'_t`treatment' = beta_m_tl_`specs'	 				 	 	     + 	/// 
												2*beta_m2_tl_`specs'*m_s`specs'_t`treatment'  	 + 	///
												  beta_mv_tl_`specs'*v_s`specs'_t`treatment' 	 +	///
												  beta_mo_tl_`specs'*o_s`specs'_t`treatment' 	 +	///
												  beta_mk_tl_`specs'*k_s`specs'_t`treatment'	 
		
		qui: gen elast_o_tl_s`specs'_t`treatment' = beta_o_tl_`specs'	 				 	 	 + 	/// 
												2*beta_o2_tl_`specs'*o_s`specs'_t`treatment'  	 + 	///
												  beta_vo_tl_`specs'*v_s`specs'_t`treatment' 	 +	///
												  beta_mo_tl_`specs'*m_s`specs'_t`treatment' 	 +	///
												  beta_ok_tl_`specs'*k_s`specs'_t`treatment'	
												  
		qui: gen elast_v_tl_s`specs'_t`treatment' = beta_v_tl_`specs'	 				 	 	 + 	/// 
												2*beta_v2_tl_`specs'*v_s`specs'_t`treatment'  	 + 	///
												  beta_mv_tl_`specs'*m_s`specs'_t`treatment' 	 +	///
												  beta_vo_tl_`specs'*o_s`specs'_t`treatment' 	 +	///
												  beta_vk_tl_`specs'*k_s`specs'_t`treatment'	
												  
		qui: gen elast_k_tl_s`specs'_t`treatment' = beta_k_tl_`specs'	 				 	 	 + 	/// 
												2*beta_k2_tl_`specs'*k_s`specs'_t`treatment'  	 + 	///
												  beta_vk_tl_`specs'*v_s`specs'_t`treatment' 	 +	///
												  beta_ok_tl_`specs'*o_s`specs'_t`treatment' 	 +	///
												  beta_mk_tl_`specs'*m_s`specs'_t`treatment'	
												  
												  
		
		//define elasticity - CD 									
		qui: gen elast_cd_s`specs'_t`treatment'   = beta_m_cd_`specs'
		qui: gen elast_o_cd_s`specs'_t`treatment' = beta_o_cd_`specs'
		qui: gen elast_v_cd_s`specs'_t`treatment' = beta_v_cd_`specs'
		qui: gen elast_k_cd_s`specs'_t`treatment' = beta_k_cd_`specs'

		//calculate ratio of sales over expenditure on the input 
		qui: gen ratio_s`specs'_t`treatment'    = ratio //will be identical across specifications 
		if "`treatment'" == "4"     { //winsorize the cost ratio 
			qui: replace ratio_s`specs'_t`treatment' = ratio_w_full  
		} //winsorize the cost ratio - seperately for sales and for input expenditure 
		if "`treatment'" == "5"  |  "`treatment'" == "6"  | "`treatment'" == "7"  { 
			qui: replace ratio_s`specs'_t`treatment' = catotal_w_full/acha4_w_full 
		}
		
		//calculate markups 
		qui: gen mu_sepcal_TL_s`specs'_t`treatment' =  elast_tl_s`specs'_t`treatment' * ratio_s`specs'_t`treatment' 
		qui: gen mu_sepcal_CD_s`specs'_t`treatment' =  elast_cd_s`specs'_t`treatment' * ratio_s`specs'_t`treatment' 
		
		//calculate log markups
		qui: gen l_mu_sepcal_TL_s`specs'_t`treatment' = log(mu_sepcal_TL_s`specs'_t`treatment') 
		qui: gen l_mu_sepcal_CD_s`specs'_t`treatment' = log(mu_sepcal_CD_s`specs'_t`treatment')
		
		if "`treatment'" == "2" | "`treatment'" == "7"     {
			//make industry variable, needed because some ind. may have all negative markups / log-markup missing 
			egen ind2 = group(naf2d) if l_mu_sepcal_TL_s`specs'_t`treatment' != . 
			qui: su ind2 
			forval ind = 1/`r(max)' { // 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 {
				qui: winsor  l_mu_sepcal_TL_s`specs'_t`treatment'  if ind2==`ind', p(.$winlevel) gen(temp1) 
				qui: replace l_mu_sepcal_TL_s`specs'_t`treatment'  = temp1 if ind2==`ind'
				qui: drop temp1  
			}
			drop ind2 
			egen ind2 = group(naf2d) if l_mu_sepcal_CD_s`specs'_t`treatment' != .  
			qui: su ind2 
			forval ind = 1/`r(max)' { // 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 {
				qui: winsor  l_mu_sepcal_CD_s`specs'_t`treatment' if ind2==`ind', p(.$winlevel) gen(temp2) 
				qui: replace l_mu_sepcal_CD_s`specs'_t`treatment'  = temp2 if ind2==`ind' 
				qui: drop temp2 
			}
			drop ind2 
		}
		else if "`treatment'" == "10" {
			qui: winsor  l_mu_sepcal_TL_s`specs'_t`treatment'  , p(.$winlevel) gen(temp1) 
			qui: replace l_mu_sepcal_TL_s`specs'_t`treatment'  = temp  
			qui: winsor  l_mu_sepcal_CD_s`specs'_t`treatment' , p(.$winlevel) gen(temp2) 
			qui: replace l_mu_sepcal_CD_s`specs'_t`treatment'  = temp2  
			qui: drop temp1 temp2 		
		}
		else if "`treatment'" == "8" {
			fastxtile mtile_l_mu_sepcal_CD_s`specs'_t`treatment' = l_mu_sepcal_CD_s`specs'_t`treatment', nq(1000)
			fastxtile mtile_l_mu_sepcal_TL_s`specs'_t`treatment' = l_mu_sepcal_TL_s`specs'_t`treatment', nq(1000)
			
			
			gen temp=.
			replace temp= l_mu_sepcal_CD_s`specs'_t`treatment' if mtile_l_mu_sepcal_CD_s`specs'_t`treatment'<$toptr   & mtile_l_mu_sepcal_CD_s`specs'_t`treatment'>$bottr 
			drop l_mu_sepcal_CD_s`specs'_t`treatment' 
			rename temp l_mu_sepcal_CD_s`specs'_t`treatment'
		
			gen temp=.
			replace temp= l_mu_sepcal_TL_s`specs'_t`treatment' if mtile_l_mu_sepcal_TL_s`specs'_t`treatment'<$toptr   & mtile_l_mu_sepcal_TL_s`specs'_t`treatment'>$bottr
			drop l_mu_sepcal_TL_s`specs'_t`treatment' 
			rename temp l_mu_sepcal_TL_s`specs'_t`treatment' 			
		}
		
		
		if "`treatment'" == "2"| "`treatment'" == "7" | "`treatment'" == "8" | "`treatment'" == "10" {
			replace mu_sepcal_TL_s`specs'_t`treatment' = exp(l_mu_sepcal_TL_s`specs'_t`treatment')
			replace mu_sepcal_CD_s`specs'_t`treatment' = exp(l_mu_sepcal_CD_s`specs'_t`treatment')					
		}
	}
}

** Clean up  and save 
keep *mu_* firmsId naf2d year o v k m elast_* ratio* 
drop if year == . | firmsId == . 
save analysisdata/dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win${winlevel}_p`code'_XTABOND_andSTATA_DGM.dta , replace 




clear 















