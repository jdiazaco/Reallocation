*** figure 5 


********************************************* preamble
* clean up 
	clear all 
	macro drop _all
	set max_memory 50g
	set matsize 11000


*Outlier treatment level	
local winlevel_l    015   

* winsorization level 
global winlevel_gl `winlevel_l'

*trimming level (thousands)
global toptr_gl 985
global bottr_gl 15
		


********************************************* prepare/load data 


*load the firm-level data:
	
	
use analysisdata/ficusfare_reduced_9419_sec2_year2_firm_va_sevyears_v0222, clear 

**drop some useless variables
capture drop acha1 acha2 acha3 acha5 acha6 cogs  acha1R acha2R acha3R acha5R acha6R  invcorp catotal_tot

**destring the sector code before the merge
destring naf2d, replace

	
*Compute Markup 

** define production variables
gen s = ln(catotalR)
gen m = ln(acha4R)	//m will be the variable input 
gen v = ln(salR)
gen k = ln(immocorR)
gen o = ln(autachaR)
gen lcogs = ln(cogs_newdef) 

** define cost ratio 
gen ratio = catotal/acha4 
gen ratio_cogs = catotal/cogs_newdef


	
********************************************* conmpute markups based on materials
foreach code in _p37 {
	**Loop over the quantity and revenu markup
	foreach specs in FSQ BFSR  {
		***merge coefficients 
		if "`specs'" == "FSQ" {
			//merge m:1 naf2d using "data\coefficients_byind_Python_`specs'`code'", nogen
			merge m:1 naf2d_num using  "analysisdata\coefficients_byind_dgm_outputfirm_obs.dta", nogen
		}
		if "`specs'" == "BFSR" {
			//merge m:1 naf2d using "data\coefficients_byind_Python_`specs'`code'", nogen
			merge m:1 naf2d_num using "analysisdata\coefficients_byind_dgm_revenuefirm_obs.dta", nogen
		}
		
		
		***rename coefficients 
		foreach beta in const_cd m_cd v_cd o_cd k_cd const_tl m_tl v_tl o_tl k_tl m2_tl mv_tl mo_tl mk_tl v2_tl vo_tl vk_tl o2_tl ok_tl k2_tl {
			rename beta_`beta' beta_`beta'_`specs'
		}
		
		***drop redundant variables
		//drop N med_CD med_TL iqr_CD iqr_TL  rho_cd  rho_tl 
		
		
		***calculate elasticity - TL 
		gen elast_tl_s`specs'_t1 = beta_m_tl_`specs'	 				 	 + 	/// 
													2*beta_m2_tl_`specs'*m   + 	///
													  beta_mv_tl_`specs'*v 	 +	///
													  beta_mo_tl_`specs'*o 	 +	///
													  beta_mk_tl_`specs'*k	 
												
		***define elasticity - CD 									
		gen elast_cd_s`specs'_t1 = beta_m_cd_`specs'
		
		
		***calculate markups 		
		gen mu_TL_s`specs'`code'_t1 =  elast_tl_s`specs'_t1 * ratio 
		gen mu_CD_s`specs'`code'_t1 =  elast_cd_s`specs'_t1 * ratio
		
		
			
		***calculate log markups
		gen l_mu_TL_s`specs'`code'_t1 = log(mu_TL_s`specs'`code'_t1) 
		gen l_mu_CD_s`specs'`code'_t1 = log(mu_CD_s`specs'`code'_t1)
	}
	
	**drop some useless variables
	capture drop ml_* 
	capture drop elast_* 
	capture drop beta_*	
	
}

su mu_CD_s* l_mu_CD_s*
su mu_TL_s* l_mu_TL_s*
	


********************************************* outlier treatment and balanced panel

**trimmed the data
	local top_tr $toptr_gl
	local bot_tr $bottr_gl


	foreach markup in  l_mu_TL_sFSQ_p37  l_mu_TL_sBFSR_p37 {
		fastxtile mtile_`markup'= `markup'_t1, nq(1000)
		
		gen temp=.
		replace temp= `markup'_t1 if mtile_`markup'<$toptr_gl   & mtile_`markup'>$bottr_gl 
		rename temp `markup'_tr
		
		
	}


**Balanced panel for the trimmed 	
	foreach var in l_mu_TL_sFSQ_p37_tr   l_mu_TL_sBFSR_p37_tr  	{ 
		replace `var' = . if l_mu_TL_sFSQ_p37_tr == .  == . | l_mu_TL_sBFSR_p37_tr == .  == .
	}
		

**Keep ony the balance panel
	drop if l_mu_TL_sFSQ_p37_tr == . 
	


********************************************* Compute Aggregate Markup

*get back to level of markups
	foreach var in  l_mu_TL_sFSQ_p37_tr   l_mu_TL_sBFSR_p37_tr { 
		gen level_`var'=exp(`var') 
	}

	
**Winsorized revenue share
local winlevel $winlevel_gl
		
winsor  catotal , p(.`winlevel') gen(catotal_w) 

	
***If yu want to winsorized the revenue
gen catotal_uw = catotal/acha4
replace  catotal=catotal_w


** compute revenue share  
bysort year: egen catotal_tot 	= total(catotal) 
gen catotal_share  				= (catotal/catotal_tot)

** compute cost share
gen costs = acha4R 
winsor costs , g(costs_w)  p(0.015)
replace costs= costs_w 
bysort year: egen costs_tot 	= total(costs) 
gen costs_share  				= (costs/costs_tot)





** Actually compute aggregate markups 

foreach var in l_mu_TL_sFSQ_p37_tr   l_mu_TL_sBFSR_p37_tr  { 
	foreach sample in  all{
	
		
			gen weight_var = catotal_share
	
		**harmonic average
		gen temp						= weight_var*level_`var'^(-1)
		bysort year: egen h_`sample'_`var'     = total(temp)
		replace h_`sample'_`var' 				= h_`sample'_`var'^(-1)
		
		**cost weighted average 
		gen temp3						= costs_share*level_`var'
		bysort year: egen cw_`sample'_`var'     = total(temp3)		
		
		**simple average
		gen temp2 						= weight_var*level_`var'
		bysort year: egen s_`sample'_`var'   = total(temp2) //s weighted average, for comparison 
		
		**clean afteryourself
		drop temp temp3 temp2 weight_var
	}
}



********************************************* Normalized by 2010 values
foreach var in cw_all_l_mu_TL_sFSQ_p37_tr   h_all_l_mu_TL_sFSQ_p37_tr  s_all_l_mu_TL_sFSQ_p37_tr 	  s_all_l_mu_TL_sBFSR_p37_tr	h_all_l_mu_TL_sBFSR_p37_tr			  {				 	
					
				gen temp_2010 = .
				replace temp_2010 = `var' if year==2010
				egen `var'_2010  = min(temp_2010) 
				drop temp_2010

				gen `var'_norm = `var'/`var'_2010	
					
 }

********************************************* Graph


*readable graphs
		twoway line  h_all_l_mu_TL_sFSQ_p37_tr_norm year if year>2009,  lwidth(medthick)  || line s_all_l_mu_TL_sFSQ_p37_tr_norm year if year>2009, lcolor(green) lpattern(dash) lwidth(medthick)  || /// 
		line cw_all_l_mu_TL_sFSQ_p37_tr_norm year if year>2009, lcolor(blue) lpattern(dash_dot) lwidth(medthick)  ,  ///
		legend(order(1 "Aggregate" 2 "Average"   ))  graphregion(margin(0.5 0.5 0.5 4)  lcolor(white)) xsize(5) ysize(4.5) aspectratio(1)  xlab(,grid glcolor(gs12))  ylab(0.97[0.01]1.03,grid glcolor(gs12)) graphregion(color(white))   plotregion(lcolor(black)) legend(off) xlabel(2010(5)2020,labsize(vlarge)) ylabel(,labsize(vlarge)) xtitle("") 
		graph export "output\figure5b.pdf", replace

	**Quantity vs Revenue

		*twoway connected  h_all_l_mu_TL_sFSQ_p37_tr_norm h_all_l_mu_TL_sBFSR_p37_tr_norm year if year>2009		
		twoway line  h_all_l_mu_TL_sFSQ_p37_tr_norm year if year>2009,  lcolor(green) lwidth(medthick) lpattern(dash)  || line h_all_l_mu_TL_sBFSR_p37_tr_norm year  if year>2009,  lwidth(medthick) legend(order(1 "Quantity" 2 "Revenue"   ))  lcolor(dknavy) graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) xsize(5) ysize(4.5) aspectratio(1)  xlab(,grid glcolor(gs12))  ylab(0.97[0.01]1.03,grid glcolor(gs12)) graphregion(color(white))   plotregion(lcolor(black)) legend(off) xlabel(2010(5)2020,labsize(vlarge)) ylabel(,labsize(vlarge)) xtitle("") 
		graph export "output\figure5a.pdf", replace
		
	
		**Average Level		
		su h_all_l_mu_TL_sFSQ_p37_tr s_all_l_mu_TL_sFSQ_p37_tr h_all_l_mu_TL_sBFSR_p37_tr  
		


























