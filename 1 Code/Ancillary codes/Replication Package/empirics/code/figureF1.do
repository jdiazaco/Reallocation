*** produce figure F1 

*** dependencies
ssc install blindschemes 
set scheme plotplainblind


*** open the panel
use analysisdata/data_smallest_for_python_p37_XTABOND, clear  


*** merge with production function coefficients 
destring naf2d, replace 
merge m:1 naf2d_num using  "analysisdata\coefficients_byind_dgm_outputfirm_obs.dta", nogen

*** merge with the markup
merge 1:1 firmsId year using "analysisdata\dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p37_XTABOND_andSTATA_DGM"
drop if _merge==1
drop _merge

*** Drop non manuf sectors
drop if naf2d_num<12 & naf2d_num>33

***Set up panel
xtset firmsId year 

*** define production variables
qui: gen v2 = v^2
qui: gen k2 = k^2
qui: gen m2 = m^2
qui: gen o2 = o^2
qui: gen mv = m*v  
qui: gen mo = m*o
qui: gen mk = m*k
qui: gen vo = v*o
qui: gen vk = v*k
qui: gen ok = o*k

*** Compute instruments
qui: gen v_lag  		= l.v
qui: gen k_lag  		= l.k
qui: gen m_lag  		= l.m
qui: gen o_lag  		= l.o

qui: gen v_lag2 		= v_lag^2
qui: gen k_lag2 		= k_lag^2
qui: gen m_lag2 		= m_lag^2
qui: gen o_lag2 		= o_lag^2

qui: gen m_lagv_lag 	= m_lag*v_lag  
qui: gen m_lago_lag 	= m_lag*o_lag  
qui: gen m_lagk_lag 	= m_lag*k_lag  
qui: gen v_lago_lag 	= v_lag*o_lag  
qui: gen v_lagk_lag 	= v_lag*k_lag  
qui: gen o_lagk_lag 	= o_lag*k_lag  

* Generate additional instrument variables 
qui: gen m_lagv 	= m_lag*v  
*qui: gen m_lago_lag 	= m_lag*o_lag  
qui: gen m_lagk 	= m_lag*k  
qui: gen vo_lag 	= v*o_lag  
*qui: gen vk 	= v*k  
qui: gen o_lagk 	= o_lag*k 


**Generate price adjusted by pdty persistence
xtset firmsId year 
gen p_tilde = p - 0.6 *l.p


*Correlation for each sector
preserve
	foreach var in v k m_lag o_lag v2 k2 m_lag2 o_lag2 m_lagv m_lago_lag m_lagk vo_lag vk o_lagk{
		
		statsby corr_p_`var'= r(rho) ,  by(naf2d_num) saving(temp\correlation_price_instrument_ind_`var'.dta, replace): corr p_tilde `var'
	}

	use temp\correlation_price_instrument_ind_v.dta, clear
	label var corr_p_v "corr_p_v"

	foreach var in v k m_lag o_lag v2 k2 m_lag2 o_lag2 m_lagv m_lago_lag m_lagk vo_lag vk o_lagk{
		merge 1:1 naf2d_num using "temp\correlation_price_instrument_ind_`var'.dta"
		drop _merge
		
		label var corr_p_`var' "corr_p_`var'"
		
	}
	save analysisdata\correlation_price_instrument_ind.dta, replace

restore



*** open and plot the data

use analysisdata\correlation_price_instrument_ind.dta, replace
drop if naf2d_num >= 40 | naf2d_num < 10 

* generate index for industries
gen line1 = -0.17
gen line2 = 0.133
gen ind = _n 
graph twoway scatter corr_p_v ind ||  /// 
			 scatter corr_p_m_lag  ind ||  ///
			 scatter corr_p_o_lag  ind ||    ///
			 scatter corr_p_k ind || ///
			 line line1 ind , lpattern(dot) || /// 
			 line line2 ind , lpattern(dot)  ||, xtitle("Two-Digit Industry") xlabel(1 "13" 2 "14" 3 "15" 4 "16" 5 "17" 6 "18" ///
																			7 "20" 8 "22" 9 "23" 10 "24" 11 "25" 12 "26" ///
																			13 "27" 14 "28" 15 "29" 16 "30" 17 "31" 18 "32" 19 "33") ///  
																			legend(label(1 "Labor") label(2 "Materials") label(3 "Services") label(4 "Capital")  label(5 "Sim: variable input")  label(6 "Sim: fixed input") ) ytitle("Correlation Price-Instrument")
graph export output/figureF1a.pdf, replace 

																			* generate index for industries
gen line3 = -0.126
gen line4 = 0.169
graph twoway scatter corr_p_v2 ind ||  /// 
			 scatter corr_p_m_lag2  ind ||  ///
			 scatter corr_p_o_lag2  ind ||    ///
			 scatter corr_p_k ind || ///
			 line line3 ind , lpattern(dot) || /// 
			 line line4 ind , lpattern(dot)  ||, xtitle("Two-Digit Industry") xlabel(1 "13" 2 "14" 3 "15" 4 "16" 5 "17" 6 "18" ///
																			7 "20" 8 "22" 9 "23" 10 "24" 11 "25" 12 "26" ///
																			13 "27" 14 "28" 15 "29" 16 "30" 17 "31" 18 "32" 19 "33") ///  
																			legend(label(1 "Labor (squared)") label(2 "Materials (squared)") label(3 "Services (squared)") label(4 "Capital (squared)")  label(5 "Sim: variable input (squared)")  label(6 "Sim: fixed input (squared)") ) ytitle("Correlation Price-Instrument")
graph export output/figureF1b.pdf, replace 

gen line5 = 0.041
graph twoway scatter corr_p_m_lagv ind ||  /// 
			 scatter corr_p_m_lago_lag  ind ||  ///
			 scatter corr_p_vk  ind ||  ///
			 scatter corr_p_o_lagk  ind ||    ///
			 scatter corr_p_m_lagk ind || ///
			 scatter corr_p_vo_lag ind || ///
			 line line5 ind , lpattern(dot)  ||, xtitle("Two-Digit Industry") xlabel(1 "13" 2 "14" 3 "15" 4 "16" 5 "17" 6 "18" ///
																			7 "20" 8 "22" 9 "23" 10 "24" 11 "25" 12 "26" ///
																			13 "27" 14 "28" 15 "29" 16 "30" 17 "31" 18 "32" 19 "33") ///  
																			legend(label(1 "Labor*Materials") label(2 "Labor*Services") label(3 "Labor*Capital") label(4 "Services*Capital")  label(5 "Materials*Capital")  label(6 "Materials*Services") label(7 "Sim: variable*capital")  ) ytitle("Correlation Price-Instrument")
graph export output/figureF1c.pdf, replace 
																			
																			



















	
