
//settings locals and globals
{
clear
set more off
set scheme s2mono
set emptycells drop


local Capital KMRSoft
local Revenue Y_allF
local Proxy M_Proxy


global models   base  
global estimation_models    base  
global DM_spec     base

  
global out_MP =  "YES"



log using "$savepath\Freigabe\\Log_KDFV_FREIGABE.log", replace name(Log_KDFV_FREIGABE)
log close Log_KDFV_FREIGABE


cap noisily mkdir "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\"
}
**********************************************************
********************************************************
*******Start first chapter - prepares data, estimates production functions, derives key measures, and produces results for Figure D1 and D2 and Table A5
*******************************************************
*******************************************************

if "$technology_LMP" == "YES" {






//generate age proxy
***********************************************************************
{
	
display "generate age variable"	
use  unr jahr  ieu ieu_ef46 jbu_ef16 jbu_ef15 jbu_ef14 kse pe  mb ieb  if jahr >= 1995  using "$Firmendaten" , clear
drop if unr ==.
duplicates drop

//gen first year firm is 20e
bys unr: egen birth_20e_year = min(jahr)

gen age_20e = jahr - birth_20e_year
gen dummy_young = 1 if age <=5  & age !=.
replace dummy_young = 0 if age >5  & age !=.
replace dummy_young =. if jahr <2002
replace age = . if jahr <2002
keep if jahr >= 2002

keep unr jahr age dummy_young  jbu_ef16 jbu_ef14  jbu_ef15
duplicates drop 
xtset unr jahr

gen lprod_rev = ln(jbu_ef16/jbu_ef14)
gen ln_employ = ln(jbu_ef14)

			gen l_firm = 0.5*(l.jbu_ef14+jbu_ef14)
            gen gr_firm = (jbu_ef14-l.jbu_ef14)/l_firm
			
			gen gr_log_firm = ln_employ - l.ln_employ
			
			
			
gen f_gr_firm = gr_firm

	
//merge  data on wz classification
**************************************


keep if jahr >= 2004
keep unr jahr age dummy_young  jbu_ef16 jbu_ef14  jbu_ef15    gr_firm  l_firm  f_gr_firm  lprod_rev   ln_employ
duplicates drop 
xtset unr jahr

save  "$savepath\techno_LMP\age_firms_20e.dta" , replace




}
//Take additional KSE variables
***********************************************************************
{
// use data
use "$Firmendaten", clear


							// This is for the fake data to run through
							if  "$IWH_run" == "YES"  {
    cap drop land_u
gen land_u_hlp = runiform()* 99
replace land_u_hlp = round(land_u)
replace land_u_hlp = 10 if land_u < 10
bys unr jahr: egen land_u = max(land_u_hlp)
tostring(land_u), replace
drop land_u_hlp



					gen ags_b_hlp = runiform()* 99999999
					replace ags_b_hlp = round(ags_b)
					replace ags_b_hlp = 10000000 if ags_b < 10000000
					bys unr jahr: egen ags_b = max(ags_b_hlp)
					tostring(ags_b), replace
					drop ags_b_hlp



					gen ags_u_hlp = runiform()* 99999999
					replace ags_u_hlp = round(ags_u)
					replace ags_u_hlp = 10000000 if ags_u < 10000000
					bys unr jahr: egen ags_u = max(ags_u_hlp)
					tostring(ags_u), replace
					drop ags_u_hlp
					
					

							}






keep kse_ef53 kse_ef63  kse_ef66 kse_ef68  kse_ef64  kse_ef65  kse_ef59/*       //Mb_E
*/		kse_ef60  kse_ef61  kse_ef62 /*											//Wage
*/		kse_ef55	kse_ef78 kse_ef46  	/*						// 53 raw mats 55 energy								//Energy
*/ unr jahr	land_u  											//Other Stuff
duplicates drop
drop if unr ==.


// to fix the ivreg2 bug
version 11
cap noisily reghdfe, compile
reghdfe  kse_ef60 ( kse_ef61 = kse_ef53), absorb (jahr) $iwh_run_reghdfe

version 13
cap noisily reghdfe, compile
reghdfe  kse_ef60 ( kse_ef61 = kse_ef53), absorb (jahr)   $iwh_run_reghdfe


drop if unr ==.
drop if jahr ==.
duplicates drop


save "$savepath\Nominal_vars_technology_LMP.dta"  , replace



}
// merge and drop outliers prepare data
******************************************************************
{


display "Check the merges"
display "****************"

		display "use ProductionfunctionVariablesFirm.dta"
	


use "$savepath\ProductionfunctionVariablesFirm.dta", clear


merge 1:1 unr jahr using "$savepath\Capital Measures.dta"
drop if _merge ==2
drop _merge


display "merge with proximity measures"
merge 1:1 unr jahr using "$savepath\Proximity Measures"
drop if _merge ==2
drop _merge



merge 1:1 unr jahr using "$savepath\WZ08 assigned" 
drop if _merge == 2
drop _merge


display "merge nominal vars for markup"
merge 1:1 unr jahr using "$savepath\Nominal_vars_technology_LMP.dta"
drop if _merge ==2
drop _merge

display "merge age data"
merge 1:1 unr jahr using "$savepath\techno_LMP\age_firms_20e.dta"
drop if _merge ==2
drop _merge






preserve 
use "$savepath\Augmented_Product_Data_Unaggregated_Purged"  , clear


keep jahr unr final_gp02 pe_15  Q measurement_unit


gen prod_id = substr(final_gp02,3,9) + "_" + measurement_unit 
replace prod_id = substr(final_gp02,3,9) + "_" + measurement_unit

bys prod_id  unr  jahr: egen sales = total(pe_15)
by prod_id  unr  jahr: egen quantity = total(Q)

gen flag_quant = 1 if Q !=. & Q !=0
replace  flag_quant = 0 if Q ==. | Q ==0

by prod_id  unr  jahr: egen salesQ = total(pe_15 * flag_quant)


keep prod_id unr jahr salesQ  sales quantity
duplicates drop 
egen id = group(prod_id unr)
xtset id jahr


//market vars
bys jahr prod_id: egen market_sales = total(sales)
by jahr prod_id: egen market_salesQ = total(salesQ)
by jahr prod_id: egen market_quantity = total(quantity)

gen market_price = market_salesQ / market_quantity


//firm vars
gen Price = salesQ/quantity
gen PriceComp = Price / market_price
gen ms_firmproduct =  sales/ market_sales
gen msQ_firmproduct =  quantity/ market_quantity



bys unr jahr: egen firmsales = total(sales)
by unr jahr: egen firmsalesQ = total(salesQ)



by unr jahr: egen Firm_price_p = total(Price * salesQ/firmsalesQ)
by unr jahr: egen Firm_price_Comp = total(PriceComp * salesQ/firmsalesQ)

by unr jahr: egen Firm_ms = total(ms_firmproduct * sales/firmsales)
by unr jahr: egen Firm_msQ = total(msQ_firmproduct * salesQ/firmsalesQ)



by unr jahr: egen checkQ = total(salesQ/firmsalesQ)
by unr jahr: egen checkR = total(sales/firmsales)


keep unr jahr Firm_price_p  Firm_price_Comp  Firm_ms  Firm_msQ 
duplicates drop 


save "$savepath\prices_and_market_shares_firm_prodfun.dta"  , replace

restore





sort unr jahr
merge 1:1 unr jahr using "$savepath\prices_and_market_shares_firm_prodfun"

drop if _merge ==2
drop _merge




drop if unr==.
drop if jahr ==.
duplicates drop
xtset unr jahr




//fix industry 
//2dig
destring  wz_unr_like_statistical2 , replace force


bys unr : egen mode_wz = mode(wz_unr_like_statistical2)
replace wz_unr_like_statistical2 =  mode_wz if mode_wz !=.

by unr : egen max_year_wz = max(jahr) if  wz_unr_like_statistical2 !=.


gen wz_lastH= wz_unr_like_statistical2 if jahr == max_year_wz
by unr : egen wz_last = mean(wz_lastH) 


replace wz_unr_like_statistical2 =  wz_last if mode_wz ==.

drop   mode_wz   max_year_wz   wz_last   wz_lastH    



//4dig
destring(wz_unr_like_statistical4), gen(wz_4_numeric_help) force


bys unr : egen mode_wz = mode(wz_4_numeric_help)
replace wz_4_numeric_help =  mode_wz if mode_wz !=.

by unr : egen max_year_wz = max(jahr) if  wz_4_numeric_help !=.


gen wz_lastH= wz_4_numeric_help if jahr == max_year_wz
by unr : egen wz_last = mean(wz_lastH) 


replace wz_4_numeric_help =  wz_last if mode_wz ==.






keep L_FTE RD_I RD_L_I RD_Ex RD_Ex_L Mb*  `Capital' `Capital'_nom  `Revenue' `Revenue'_nom `Proxy'    Wagekse_nom D_Exp   D_RD NumP ms_Qf PriceWage  PriceInvSonst PriceFirm /*
*/   Mb_E  Mb_E_nom   kse_ef55 kse_ef53  L_FTE   FirmP ms_f ms_Qf    Firmp   Y_allE       /*
*/ wz  l_BL_n  kse_ef78 kse_ef46        Tex         /*
*/ unr jahr wz* kse_* M_Proxy     KMR  l_BL_n BL_n   land_u      dummy_young age   /*
*/ Firm_price_p  Firm_price_Comp  Firm_ms  Firm_msQ 
duplicates drop


}

// exclude some industries, outliers, and strange values
*********************************************************
{
drop if jahr < $start_year_prodvars
cap noisily drop if wz_unr_like_statistical2 == 45
cap noisily drop if wz_unr_like_statistical2 == "45"
cap noisily drop if wz_unr_like_statistical2 == 37
cap noisily drop if wz_unr_like_statistical2 == "37"
*cap noisily drop if wz_unr_like_statistical2 == 22
*cap noisily drop if wz_unr_like_statistical2 == "22"
cap noisily drop if wz_unr_like_statistical2 == 23
cap noisily drop if wz_unr_like_statistical2 == "23"
cap noisily drop if wz_unr_like_statistical2 == 14
cap noisily drop if wz_unr_like_statistical2 == "14"
cap noisily drop if wz_unr_like_statistical2 == 16
cap noisily drop if wz_unr_like_statistical2 == "16"
cap noisily drop if wz_unr_like_statistical2 == 10
cap noisily drop if wz_unr_like_statistical2 == "10"

xtset unr jahr
gen wages = kse_ef60 + kse_ef61 + kse_ef62
gen l_revenue = l.`Revenue'
gen NomVA =   (`Revenue'_nom - Mb_E_nom)

gen Y_per_K = `Revenue' / `Capital'
gen Y_per_M = `Revenue' / Mb_E
gen Y_per_L = `Revenue'/ L_FTE
gen Y_per_W = `Revenue'_nom / wages
gen VA_Y =   NomVA /`Revenue'_nom



replace L_FTE =. if L_FTE <=0
replace `Revenue'_nom =. if `Revenue'_nom <=0
replace M_Proxy =. if M_Proxy <=0
replace `Capital' = . if `Capital' <=0
replace Mb_E_nom =. if Mb_E_nom <=0
replace FirmP =. if FirmP <=0





//Y_per_W    VA_Y 

display "outlier routine"
sort jahr wz_unr_like_statistical2

local drop_local = ""
local count = 0
foreach dropvar in   Y_per_M   Y_per_K  Y_per_L  Y_per_W  VA_Y     {  
local count = `count' + 1
 by jahr : egen per99 =   pctile(`dropvar') , p(98)  
 by jahr :  egen per1 = 	pctile(`dropvar') , p(2) 
gen INOutlier`dropvar' = 1 if  `dropvar' !=. &  (`dropvar' > per99 | `dropvar' < per1)
replace INOutlier`dropvar' = 0 if INOutlier`dropvar' !=1

if `count' == 1 {
local drop_local "`drop_local'   INOutlier`dropvar' == 1  "
}


if `count' != 1 {
local drop_local "`drop_local'  |  INOutlier`dropvar' == 1  "
}


drop per99 per1

}
//

drop if `drop_local'  
drop INOutlier*

xtset unr jahr

cap drop Y_per_M 
cap drop Y_per_L 
cap drop  Y_per_K 
cap drop  Y_per_W 
cap drop NomVA 
cap drop l_revenue  
cap drop  VA_Y


}


//Fiure D2 decomposition results 
************************************************************************************************
{





//compute job reallocation rates
{	
xtset unr jahr

					
					
					
			gen l_firm = 0.5*(l.L_FTE+L_FTE)
            		gen gr_firm = (L_FTE-l.L_FTE)/l_firm
				

			//industry
			****************	

			bysort wz_unr_like_statistical2 jahr: egen l_sectorSam = total(l_firm) if  gr_firm <.
	

			gen firm_weight1 = l_firm/l_sectorSam 
			gen gr_firm_weight1 =  firm_weight1*gr_firm if gr_firm <.
            

            bysort wz_unr_like_statistical2 jahr: egen jcr_wz_H = total(gr_firm_weight1) if gr_firm >0 & gr_firm <.
			
            by wz_unr_like_statistical2 jahr: egen jdr_wz_H = total(-gr_firm_weight1) if gr_firm <=0 & gr_firm <.
					
				by wz_unr_like_statistical2 jahr: egen   jcr_wz   = mean(jcr_wz_H)
				by wz_unr_like_statistical2 jahr: egen   jdr_wz    = mean(jdr_wz_H)
			
			
			replace jcr_wz =. if  wz_unr_like_statistical2 ==.
			replace jdr_wz =. if  wz_unr_like_statistical2 ==.

			
				
			
				//size 
			****************
			xtset unr jahr
gen smalllarge100 = 1 if  l.L_FTE <100   & l.L_FTE !=.  & l.L_FTE !=0
replace  smalllarge100 = 0 if   l.L_FTE >=100   & l.L_FTE !=.  & l.L_FTE !=0


			
				bysort smalllarge100 jahr: egen l_sectorSamSL = total(l_firm) if  gr_firm <.
			**total average employment per sector and year 

			gen firm_weight1SL = l_firm/l_sectorSamSL 
			gen gr_firm_weight1SL =  firm_weight1SL*gr_firm if gr_firm <.
            
	    **calculating the weighted growth rates with regard to the two-digit level 

            bysort smalllarge100 jahr: egen jcr_wz_HSL = total(gr_firm_weight1SL) if gr_firm >0 & gr_firm <.
            **JCR at two digit-level: sum of weighted (positive) firm-level growth rates for each sector
			
            by smalllarge100 jahr: egen jdr_wz_HSL = total(-gr_firm_weight1SL) if gr_firm <=0 & gr_firm <.
					
				by smalllarge100 jahr: egen   jcr_SL   = mean(jcr_wz_HSL)
				by smalllarge100 jahr: egen   jdr_SL    = mean(jdr_wz_HSL)
			
			
			replace jcr_SL =. if  smalllarge100 ==.
			replace jdr_SL =. if  smalllarge100 ==.		
			
			
			
			//age 
			****************
			xtset unr jahr



			
				bysort dummy_young jahr: egen l_sectorSamYO = total(l_firm) if  gr_firm <.
			**total average employment per sector and year 

			gen firm_weight1YO = l_firm/l_sectorSamYO 
			gen gr_firm_weight1YO =  firm_weight1YO*gr_firm if gr_firm <.
            
	    **calculating the weighted growth rates with regard to the two-digit level 

            bysort dummy_young jahr: egen jcr_wz_HYO = total(gr_firm_weight1YO) if gr_firm >0 & gr_firm <.
            **JCR at two digit-level: sum of weighted (positive) firm-level growth rates for each sector
			
            by dummy_young jahr: egen jdr_wz_HYO = total(-gr_firm_weight1YO) if gr_firm <=0 & gr_firm <.
					
				by dummy_young jahr: egen   jcr_YO   = mean(jcr_wz_HYO)
				by dummy_young jahr: egen   jdr_YO    = mean(jdr_wz_HYO)
			
			
			replace jcr_YO =. if  dummy_young ==.
			replace jdr_YO =. if  dummy_young ==.	
			
			
			
			
			
			//GERMANY
			
			bysort  jahr: egen l_countrySam = total(l_firm) if  gr_firm <.
			*total average employment per country and year 

			gen firm_weight4 = l_firm/l_countrySam
			gen gr_firm_weight4 = firm_weight4*gr_firm if gr_firm <.
			*calculating the weighted growth rates at country level

			bysort  jahr: egen jcr_H = total(gr_firm_weight4) if gr_firm >0 & gr_firm <.
			*JCR at country level 

			by  jahr: egen jdr_H = total(-gr_firm_weight4) if gr_firm <=0 & gr_firm <.
			*JDR at county level in absolute values
			
			
			
			by  jahr: egen   jcr  = mean(jcr_H)
			by  jahr: egen   jdr    = mean(jdr_H)
			
			
			gen job_dyn =  jdr +  jcr
			gen job_dyn_wz =  jdr_wz +  jcr_wz		
			gen job_dyn_YO =  jdr_YO +  jcr_YO
			gen job_dyn_SL =  jdr_SL +  jcr_SL
			foreach x in job_dyn   job_dyn_wz  job_dyn_YO  job_dyn_SL  {
			replace `x' = . if `x' ==0
			}

			cap drop jcr_* jdr_*
			
			
}

		
			
					
//Figure D2 results 			
{				
	//within between decomposition 		
	********************
	*
	**
	*
	*
	*
	//size 
	******
	**
	*
	
	preserve
	{
	keep  job_dyn_SL  job_dyn  smalllarge100  jahr  l_sectorSamSL
	keep if jahr >= 1996 | jahr <= 2017

	rename jahr year
	rename  smalllarge100   mac_sector
	rename l_sectorSamSL  lab
	rename job_dyn_SL job_dyn_mac 
	keep if job_dyn_mac !=.
	keep if lab !=.
	keep if mac_sector !=.
	gen country = 1
	
	gen flag_fallzahlen = 1 if job_dyn_mac !=.
	bys country year: egen Fallhep = total(flag_fallzahlen)
	egen Fallzahlen = min(Fallhep)
		duplicates drop
		xtset mac_sector  year
	//gen weight
	bys country year : egen total_lab = total(lab)
	gen lab_weight = lab / total_lab
	
	//gen first and last year
	bys country : egen min_year = min(year) if lab_weight !=. 
	by country : egen max_year = max(year) if lab_weight !=.

	//gen job dyn in first and last year
	bys country mac_sector: g fy_jd_h = job_dyn_mac if year == min_year
	by country mac_sector: egen fy_jd = mean(fy_jd_h) 
	by country mac_sector: g ly_jd_h = job_dyn_mac if year == max_year
	by country mac_sector: egen ly_jd = mean(ly_jd_h)
	
	//gen weight in first year
	by country mac_sector: gen lab_weight_help = lab_weight if year == min_year
	by country mac_sector: egen lab_weight_base = mean(lab_weight_help)
	by country mac_sector: gen lab_weight_help2 = lab_weight if year == max_year
	by country mac_sector: egen lab_weight_end = mean(lab_weight_help2)
	
	
	
	//macro sector change
	g g_jd = (ly_jd - fy_jd)
	g g_weight_jd = (lab_weight_end - lab_weight_base)
	* generate AGGREGATE change - at the country level 
	bys country year: egen jr_country = total(job_dyn_mac*lab_weight)
	g jr_country_fy_h = jr_country if year == min_year
	bys country: egen jr_country_fy = mean(jr_country_fy_h) 
	g jr_country_ly_h = jr_country if year == max_year
	bys country: egen jr_country_ly = mean(jr_country_ly_h) 
	
	
	g job_dyn_fy_h = job_dyn if year == min_year
	bys country: egen job_dyn_fy = mean(job_dyn_fy_h) 
	g job_dyn_ly_h = job_dyn if year == max_year
	bys country: egen job_dyn_ly = mean(job_dyn_ly_h) 
	
	
	*generate the change of job reallocation. This will be decomposed
	g g_jr_country = jr_country_ly - jr_country_fy

	
	
	* Generate the within term. 2 steps: mutiply by initial weight, then sum over
	*step 1 
	g with_jd_help = g_jd * lab_weight_base
	g betw_jd_help = g_weight_jd * fy_jd
	g cross_jd_help = g_weight_jd * g_jd


keep if year == min_year  |  year == max_year 
	*step 2 - Sum to the country level
	bys country year: egen with_jd = total(with_jd_help)
	by country year: egen betw_jd = total(betw_jd_help)	
	by country year: egen cross_jd = total(cross_jd_help)

	
	sort country mac_sector

	
	
	
	

		keep country  Fallzahlen  jr_country_ly jr_country_fy job_dyn_fy job_dyn_ly  with_jd    cross_jd betw_jd  min_year   max_year   year
		duplicates drop

		
		
		cap log close Log_FREI_DEstats
		log using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\Log_FREI_DEstats.log", append name(Log_FREI_DEstats)
	display "Size-decomposition"
		tabstat Fallzahlen jr_country_fy jr_country_ly  job_dyn_fy job_dyn_ly  with_jd    cross_jd betw_jd min_year   max_year , stat(mean sd) by(year)
	log close Log_FREI_DEstats
	
	}
	restore
			
	//industry 
	******
	**
	*
	
preserve
	{
	keep  job_dyn_wz  job_dyn  wz_unr_like_statistical2  jahr  l_sectorSam
	keep if jahr >= 1996 | jahr <= 2017

	rename jahr year
	rename  wz_unr_like_statistical2   mac_sector
	rename l_sectorSam  lab
	rename job_dyn_wz job_dyn_mac 
	keep if job_dyn_mac !=.
	keep if lab !=.
	keep if mac_sector !=.
	gen country = 1
	
	gen flag_fallzahlen = 1 if job_dyn_mac !=.
	bys country year: egen Fallhep = total(flag_fallzahlen)
	egen Fallzahlen = min(Fallhep)
		duplicates drop
		xtset mac_sector  year
		
	bys country : egen min_year = min(year) if lab !=. 
	by country : egen max_year = max(year) if lab !=.
		
		levelsof mac_sector if year == min_year
		local min_y_local = "`r(levels)'"
		levelsof mac_sector if year == max_year
		local max_y_local = "`r(levels)'"
		
		
		local drop_1 = ""
		foreach x in `min_y_local'  {
		local counttest = 0	
		foreach y in `max_y_local'  {
			
		if  `x' == `y'	{
		local counttest = `counttest' + 1 
		}
			
		}	
		
		if  `counttest' == 0  	{
		local drop_1 =  "`drop_1'" + " `x'"
		
		}
		}
		display "`drop_1'"
		
		
		
		
		local drop_2 = ""
		foreach x in `max_y_local'  {
		local counttest = 0	
		foreach y in `min_y_local'  {
			
		if  `x' == `y'	{
		local counttest = `counttest' + 1 
		}
			
		}	
		
		if  `counttest' == 0  	{
		local drop_2 =  "`drop_2'" + " `x'"
		
		}
		}
		display "`drop_2'"
		
		
		
		foreach zz in  `drop_1' `drop_2' {
			display `zz'
			drop if mac_sector == `zz'
			
		}
		
		
		
drop min_year max_year
		
	//gen weight
	bys country year : egen total_lab = total(lab)
	gen lab_weight = lab / total_lab
	
	//gen first and last year
	bys country : egen min_year = min(year) if lab_weight !=.   //I changed here and in other spects from bys country mac sectors to by country
	by country : egen max_year = max(year) if lab_weight !=.

	//gen job dyn in first and last year
	bys country mac_sector: g fy_jd_h = job_dyn_mac if year == min_year
	by country mac_sector: egen fy_jd = mean(fy_jd_h) 
	by country mac_sector: g ly_jd_h = job_dyn_mac if year == max_year
	by country mac_sector: egen ly_jd = mean(ly_jd_h)
	
	//gen weight in first year
	by country mac_sector: gen lab_weight_help = lab_weight if year == min_year
	by country mac_sector: egen lab_weight_base = mean(lab_weight_help)
	by country mac_sector: gen lab_weight_help2 = lab_weight if year == max_year
	by country mac_sector: egen lab_weight_end = mean(lab_weight_help2)
	
	
	
	//macro sector change
	g g_jd = (ly_jd - fy_jd)
	g g_weight_jd = (lab_weight_end - lab_weight_base)
	* generate AGGREGATE change - at the country level 
	bys country year: egen jr_country = total(job_dyn_mac*lab_weight)
	g jr_country_fy_h = jr_country if year == min_year
	bys country: egen jr_country_fy = mean(jr_country_fy_h) 
	g jr_country_ly_h = jr_country if year == max_year
	bys country: egen jr_country_ly = mean(jr_country_ly_h) 
	
	
	g job_dyn_fy_h = job_dyn if year == min_year
	bys country: egen job_dyn_fy = mean(job_dyn_fy_h) 
	g job_dyn_ly_h = job_dyn if year == max_year
	bys country: egen job_dyn_ly = mean(job_dyn_ly_h) 
	
	
	*generate the change of job reallocation. This will be decomposed
	g g_jr_country = jr_country_ly - jr_country_fy

	
	
	* Generate the within term. 2 steps: mutiply by initial weight, then sum over
	*step 1 
	g with_jd_help = g_jd * lab_weight_base
	g betw_jd_help = g_weight_jd * fy_jd
	g cross_jd_help = g_weight_jd * g_jd


keep if year == min_year  |  year == max_year 
	*step 2 - Sum to the country level
	bys country year: egen with_jd = total(with_jd_help)
	by country year: egen betw_jd = total(betw_jd_help)	
	by country year: egen cross_jd = total(cross_jd_help)

	
	sort country mac_sector

	
	
	
	

		keep country  year  Fallzahlen  jr_country_ly jr_country_fy job_dyn_fy job_dyn_ly  with_jd    cross_jd betw_jd  min_year   max_year 
		duplicates drop

		
		
		cap log close Log_FREI_DEstats
		log using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\Log_FREI_DEstats.log", append name(Log_FREI_DEstats)
	display "Industry-decomposition"
		tabstat Fallzahlen jr_country_fy jr_country_ly  job_dyn_fy job_dyn_ly  with_jd    cross_jd betw_jd min_year   max_year , stat(mean sd) by(year)
	log close Log_FREI_DEstats
	
	}
	restore
					
			
	//age 
	******
	**
	*
	
	preserve
	{
	keep  job_dyn_YO  job_dyn  dummy_young  jahr  l_sectorSamYO
	keep if jahr >= 1996 | jahr <= 2017

	rename jahr year
	rename  dummy_young   mac_sector
	rename l_sectorSamYO  lab
	rename job_dyn_YO job_dyn_mac 
	keep if job_dyn_mac !=.
	keep if lab !=.
	keep if mac_sector !=.
	gen country = 1
	
	gen flag_fallzahlen = 1 if job_dyn_mac !=.
	bys country year: egen Fallhep = total(flag_fallzahlen)
	egen Fallzahlen = min(Fallhep)
		duplicates drop
		xtset mac_sector  year
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
	//gen weight
	bys country year : egen total_lab = total(lab)
	gen lab_weight = lab / total_lab
	
	
	//gen first and last year
	bys country : egen min_year = min(year) if lab_weight !=. 
	by country : egen max_year = max(year) if lab_weight !=.

	//gen job dyn in first and last year
	bys country mac_sector: g fy_jd_h = job_dyn_mac if year == min_year
	by country mac_sector: egen fy_jd = mean(fy_jd_h) 
	by country mac_sector: g ly_jd_h = job_dyn_mac if year == max_year
	by country mac_sector: egen ly_jd = mean(ly_jd_h)
	
	//gen weight in first year
	by country mac_sector: gen lab_weight_help = lab_weight if year == min_year
	by country mac_sector: egen lab_weight_base = mean(lab_weight_help)
	by country mac_sector: gen lab_weight_help2 = lab_weight if year == max_year
	by country mac_sector: egen lab_weight_end = mean(lab_weight_help2)
	
	
	
	//macro sector change
	g g_jd = (ly_jd - fy_jd)
	g g_weight_jd = (lab_weight_end - lab_weight_base)
	* generate AGGREGATE change - at the country level 
	bys country year: egen jr_country = total(job_dyn_mac*lab_weight)
	g jr_country_fy_h = jr_country if year == min_year
	bys country: egen jr_country_fy = mean(jr_country_fy_h) 
	g jr_country_ly_h = jr_country if year == max_year
	bys country: egen jr_country_ly = mean(jr_country_ly_h) 
	
	
	g job_dyn_fy_h = job_dyn if year == min_year
	bys country: egen job_dyn_fy = mean(job_dyn_fy_h) 
	g job_dyn_ly_h = job_dyn if year == max_year
	bys country: egen job_dyn_ly = mean(job_dyn_ly_h) 
	
	
	*generate the change of job reallocation. This will be decomposed
	g g_jr_country = jr_country_ly - jr_country_fy

	
	
	* Generate the within term. 2 steps: mutiply by initial weight, then sum over
	*step 1 
	g with_jd_help = g_jd * lab_weight_base
	g betw_jd_help = g_weight_jd * fy_jd
	g cross_jd_help = g_weight_jd * g_jd


keep if year == min_year  |  year == max_year 
	*step 2 - Sum to the country level
	bys country year: egen with_jd = total(with_jd_help)
	by country year: egen betw_jd = total(betw_jd_help)	
	by country year: egen cross_jd = total(cross_jd_help)

	
	sort country mac_sector

	
	
	
	

		keep country  Fallzahlen  jr_country_ly jr_country_fy job_dyn_fy job_dyn_ly  with_jd    cross_jd betw_jd  min_year   max_year year
		duplicates drop

		
		
		cap log close Log_FREI_DEstats
		log using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\Log_FREI_DEstats.log", append name(Log_FREI_DEstats)
	display "Age-decomposition"
		tabstat Fallzahlen jr_country_fy jr_country_ly  job_dyn_fy job_dyn_ly  with_jd    cross_jd betw_jd min_year   max_year , stat(mean sd) by(year)
	log close Log_FREI_DEstats
	
	}
	restore

	
}	
	
}	
		
//Generate variables for production function estimation
*********************************************************	
{
	
	
tostring(wz_unr_like_statistical2), gen(wz_ls_string)
	
gen ln`Revenue' = ln(`Revenue')
gen ln`Revenue'_cpi = ln(`Revenue'_nom/PriceWage)
gen lnY_allF_nom = ln(Y_allF_nom)
gen lnY_allE = ln(Y_allE)
gen lab	= 	ln(L_FTE)  
gen mat	= 	ln(Mb_E)
gen kap = 	ln(`Capital')



gen mat_nom = Mb_E_nom  // includes energy
gen wageB_nom = (kse_ef60 + kse_ef61 + kse_ef62)
replace  wageB_nom =. if wageB_nom ==0



gen realwage = (wageB_nom/PriceWage) / L_FTE
gen wage_nominal = (wageB_nom) / L_FTE
gen va_lprod = ln((Y_allE- Mb_E) / L_FTE)
gen VA_LPROD = ((Y_allE- Mb_E) / L_FTE)
gen va_ls = wageB_nom / (Y_allF_nom - Mb_E_nom)

gen K_L = (`Capital' /  L_FTE ) /1000 
gen M_L = (Mb_E /  L_FTE ) /1000 
gen VA_REV = (Y_allF_nom - Mb_E_nom) / Y_allF_nom 


			
xtset unr jahr
gen l_lab	= 	l.lab
gen l_mat	= 	l.mat
gen l_kap 	= 	l.kap

gen lab2	=	lab^2
gen mat2	=	mat^2
gen kap2	=	kap^2



gen l_lab2	= 	l.lab2
gen l_mat2	= 	l.mat2
gen l_kap2 	= 	l.kap2



gen lab_mat	=	lab*mat
gen	lab_kap	=	lab*kap
gen mat_kap	=	kap*mat

gen	lab_mat_kap	=	lab*mat*kap


gen l_lab_mat	=	l.lab_mat
gen	l_lab_kap	=	l.lab_kap
gen l_mat_kap	=	l.mat_kap
gen l_lab_mat_kap = l.lab_mat_kap




//productivity proxy for 3 input version
//----------------------------------------------

gen mP = ln(`Proxy')


replace ms_f = ms_f * 100
replace ms_Qf = ms_Qf * 100
gen pf	= ln(PriceFirm)
gen Rshare = ln(ms_f)

//CPI
replace pf = ln(PriceFirm) - ln(PriceWage) 
	



	
replace Firm_ms =  Firm_ms *100
replace Firm_msQ = Firm_msQ *100
replace Rshare = ln(Firm_ms)
	






gen l_pf = l.pf
gen l_Rshare = l.Rshare

gen wage = (wageB_nom/L_FTE)/PriceWage
gen lnwage = ln(wage)    	
gen l_lnwage = l.lnwage

	gen priceL		=		pf*lab
	gen priceM		=		pf*mat
	gen priceK		=		pf*kap

	gen priceL2		=		pf*lab2
	gen priceM2		=		pf*mat2
	gen priceK2		=		pf*kap2

	gen priceLM		=		pf*lab*mat
	gen priceLK		=		pf*lab*kap
	gen priceMK		=		pf*mat*kap

	gen priceLMK	=		pf*lab*mat*kap


		foreach v of varlist price*     {
		gen lag`v'	= l.`v'
	}



	gen lnNumP = ln(NumP)
	gen l_lnNumP = l.lnNumP
	gen l_D_Exp = l.D_Exp



}

// Globals Production function estimation 
*********************************************************	
{
// User written program, see master file
poly_varlist "lab kap mP " 3 termslags
global contr_prod_mP = " l_lnNumP  l_D_Exp  l_lnwage  " + " `r(lagged_trans_vars)' "
display "$contr_prod_mP"



//for pooled estimation 
global exoreg_base 		lab lab2 kap kap2   lab_kap  	$contr_prod_mP

global endoreg_base		mat  mat2  mat_kap lab_mat  lab_mat_kap 	 pf  Rshare /*
*/ priceL priceM   priceK   priceL2   priceM2   priceK2   priceLM  priceLK   priceMK   priceLMK

global enBETA_base 		mat mat2 mat_kap lab_mat lab_mat_kap kap kap2 lab_kap lab lab2	pf  Rshare /*
*/ priceL priceM   priceK   priceL2   priceM2   priceK2   priceLM  priceLK   priceMK   priceLMK


global enBETATFP_base 		mat mat2 mat_kap lab_mat lab_mat_kap kap kap2 lab_kap lab lab2	


global instr_base		l_mat  l_mat2  l_mat_kap l_lab_mat  l_lab_mat_kap  l_pf l_Rshare  /*
*/ lagpriceL lagpriceM   lagpriceK   lagpriceL2   lagpriceM2   lagpriceK2   lagpriceLM  lagpriceLK   lagpriceMK   lagpriceLMK


gen ln`Revenue'_base = ln`Revenue'
gen ln`Revenue'_baseMA = ln`Revenue'
gen ln`Revenue'_baseFL = ln`Revenue'



}


//Prod fun ESTIMATION******************
***********************************************************************
{


//TRANSLOG standard 
{


foreach model in  base    {




							gen BL_dummyfe`model' = .
							gen wz_dummyfe`model' = .
							gen time_dummyfe`model' = .							
	local betalocaltfp =  "${enBETATFP_`model'}"
	display "`betalocaltfp'"
	local betalocal =  "${enBETA_`model'}"
	display "`betalocal'"
	foreach var in  `betalocal'  {
		gen el_`var'_`model' =.
	}



						levelsof wz_unr_like_statistical2
						local prod_fun_est_levels = "`r(levels)'"
						foreach wz_count in `prod_fun_est_levels' {
						display "this is industry: `wz_count'"

//  Y_hat_`model'
						cap noisily  reghdfe  ln`Revenue'_`model'   ${exoreg_`model'} ( ${endoreg_`model'} = ${instr_`model'} )  if  wz_unr_like_statistical2 == `wz_count' , absorb( time_dummy=jahr wzdfe=wz_4_numeric_help  BLdfe=BL_n  ) vce(cluster unr) $iwh_run_reghdfe  //wzdfe=wz
						
									 if _rc == 0 {
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\\prodfun `model'.xls", append ctitle(industry `wz_count' ) adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )

							cap noisily replace BL_dummyfe`model' = BLdfe if wz_unr_like_statistical2 == `wz_count'
							cap noisily replace wz_dummyfe`model' = wzdfe if wz_unr_like_statistical2 == `wz_count'
							cap noisily replace time_dummyfe`model' = time_dummy if wz_unr_like_statistical2 == `wz_count'
							
							display "`betalocal'"
							foreach var of varlist `betalocal'   {
							cap noisily replace el_`var'_`model' = _b[`var'] if wz_unr_like_statistical2 == `wz_count'

							}

cap drop residual
cap drop ydfe
cap drop wzdfe
cap drop BLdfe
cap drop time_dummy




						}
						//close success condition

						}
						//close wz loop



			cap noisily bys BL_n : egen help2 = mean(BL_dummyfe`model')
			cap noisily bys wz   : egen help3 = mean(wz_dummyfe`model')
			cap noisily bys jahr   : egen help4 = mean(time_dummyfe`model')			

			cap noisily replace  BL_dummyfe`model' = help2
			cap noisily replace  wz_dummyfe`model' = help3
			cap noisily replace  time_dummyfe`model' = help4
						
			cap noisily drop  help2  help3 help4
			
		
xtset unr jahr

cap drop xbd*


global prod_elas_new =""
display "`betalocal'"
foreach var of varlist `betalocal' {
		global prod_elas_new = " $prod_elas_new " + " el_`var'_`model' * `var' +  "
}
display "$prod_elas_new"



cap noisily gen xbd`model'_withFE = $prod_elas_new  0
cap noisily gen xbd`model' = $prod_elas_new  BL_dummyfe`model' + wz_dummyfe`model'


xtset unr jahr





cap noisily gen TFP_`model' = ( ln`Revenue'_`model'  + pf )  - el_mat_`model' * mat -  el_mat2_`model' * mat2  -   el_mat_kap_`model' *  mat_kap  /*
*/ -   el_lab_mat_`model' *  lab_mat  -  el_lab_mat_kap_`model' *  lab_mat_kap -  el_kap_`model' * kap  -   /*
*/  el_kap2_`model' * kap2 - el_lab_kap_`model' * lab_kap - el_lab_`model' *  lab - el_lab2_`model' * lab2 - el_pf_`model' * pf - el_Rshare_`model' * Rshare  -   el_priceL_`model' * priceL -   el_priceM_`model' * priceM -  /*
*/ el_priceK_`model' * priceK  -    el_priceL2_`model' * priceL2 -  el_priceM2_`model' * priceM2 -   el_priceK2_`model' * priceK2 -  /*
*/   el_priceLM_`model' * priceLM -   el_priceLK_`model' * priceLK  -    el_priceMK_`model' * priceMK -   el_priceLMK_`model' * priceLMK  - BL_dummyfe`model'     - wz_dummyfe`model'

cap noisily gen TFPQ_`model' = ( ln`Revenue'_`model' )  - el_mat_`model' * mat -  el_mat2_`model' * mat2  -   el_mat_kap_`model' *  mat_kap  /*
*/ -   el_lab_mat_`model' *  lab_mat  -  el_lab_mat_kap_`model' *  lab_mat_kap -  el_kap_`model' * kap  -   /*
*/  el_kap2_`model' * kap2 - el_lab_kap_`model' * lab_kap - el_lab_`model' *  lab - el_lab2_`model' * lab2 - el_pf_`model' * pf - el_Rshare_`model' * Rshare  -   el_priceL_`model' * priceL -   el_priceM_`model' * priceM -  /*
*/ el_priceK_`model' * priceK  -    el_priceL2_`model' * priceL2 -  el_priceM2_`model' * priceM2 -   el_priceK2_`model' * priceK2 -  /*
*/   el_priceLM_`model' * priceLM -   el_priceLK_`model' * priceLK  -    el_priceMK_`model' * priceMK -   el_priceLMK_`model' * priceLMK  - BL_dummyfe`model'     - wz_dummyfe`model'




gen adjustment_`model' = ( ln`Revenue'_`model'  + pf)  - TFP_`model' 


}
//close model loop


cap drop xbd* 

}
// close translog models

cap drop xbd* 






}


//CALCULATE ELASTICITIES AND RTS********************* 
***********************************************************************
{



foreach x in base   {  




////price correction
gen w_con_`x' = el_pf_`x' * pf + el_Rshare_`x' * Rshare  +   el_priceL_`x' * priceL  +   el_priceM_`x' * priceM  +  /*
*/ el_priceK_`x' * priceK  +    el_priceL2_`x' * priceL2 +  el_priceM2_`x' * priceM2 +   el_priceK2_`x' * priceK2 +  /*
*/   el_priceLM_`x' * priceLM +   el_priceLK_`x' * priceLK +    el_priceMK_`x' * priceMK +   el_priceLMK_`x' * priceLMK  +BL_dummyfe`x'     + wz_dummyfe`x'



		display "su price correction"
cap noisily fdztabstat w_con_`x'  , statistics(mean var sd p10 p25 p50 p75 p90 count)




gen elas_mat_`x' = el_mat_`x' + 2 * el_mat2_`x'*(mat-w_con_`x') + el_mat_kap_`x'*  (kap-w_con_`x') + el_lab_mat_`x' * (lab) + el_lab_mat_kap_`x' * (lab) * (kap-w_con_`x')
gen elas_lab_`x' = el_lab_`x' + 2 * el_lab2_`x'*(lab) + el_lab_kap_`x' * (kap-w_con_`x') + el_lab_mat_kap_`x' * (mat-w_con_`x') * (kap-w_con_`x') + el_lab_mat_`x' * (mat-w_con_`x')
gen elas_kap_`x' = el_kap_`x' + 2 * el_kap2_`x'*(kap-w_con_`x') + el_lab_kap_`x' * (lab) + el_lab_mat_kap_`x' * (mat-w_con_`x') * (lab) + el_mat_kap_`x' * (mat-w_con_`x')


}






drop el_*
foreach x in $models  {
gen RTS_`x' = elas_mat_`x' + elas_lab_`x' + elas_kap_`x'
}



log using "$savepath\Freigabe\\Log_KDFV_FREIGABE.log", append name(Log_KDFV_FREIGABE)



		display "***********************************"
		display "***********************************"
		display "***********************************"
		display "***********************************"

foreach x in $models  {
		display "TABULATE OUTPUTELASTICITIES AND RTS FOR MODEL WITH NEGATIVE VALUES:"
		display "     *******     `x'      **********        "
		cap noisily fdztabstat elas_mat_`x' elas_lab_`x' elas_kap_`x' RTS_`x'  , statistics(mean var sd p10 p25 p50 p75 p90 count)
		display "*****************************************************"
		display "*************     NOW PER WZ!!!!      ***************"
		display "TABULATE OUTPUTELASTICITIES AND RTS PER WZ FOR MODEL WITH NEGATIVE VALUES:"
		display "     **********     `x'    **************        "
		cap noisily fdztabstat elas_mat_`x' elas_lab_`x' elas_kap_`x' RTS_`x'  ,by(wz_unr_like_statistical2) statistics(mean sd  p10 p25 p50 p75 p90  count)
		display "*****************************************************"
		

}
		display "***********************************"
		display "***********************************"
		display "***********************************"
		display "***********************************"



log close Log_KDFV_FREIGABE





//drop neg output elas
foreach x in $models  {
gen neg_`x' = 1 if ((elas_mat_`x' <=0 & elas_mat_`x' !=.)  | (elas_lab_`x' <=0 & elas_lab_`x' !=.)| (elas_kap_`x' <=0 & elas_kap_`x' !=.))
}




log using "$savepath\Freigabe\\Log_KDFV_FREIGABE.log", append name(Log_KDFV_FREIGABE)


foreach model in  $models    {
bys jahr: egen T_neg_`model' = total(neg_`model')


display "negative output elas `model'"
tab jahr, su(T_neg_`model')
}

log close Log_KDFV_FREIGABE

//RTS
*****
foreach model in   $models        {
replace RTS_`model' = .   if  neg_`model' ==1

}


xtset unr jahr
foreach x in  $models  {
replace elas_mat_`x' = .   if  neg_`x' ==1
replace elas_lab_`x' = . if  neg_`x' ==1
replace elas_kap_`x' = . if  neg_`x' ==1


cap noisily replace TFPQ_`x' = .   if  neg_`x' ==1
cap noisily replace TFP_`x' = .   if  neg_`x' ==1
cap noisily replace adjustment_`x' = .   if  neg_`x' ==1



}




		display "***********************************"
		display "***********************************"
		display "***********************************"
		display "***********************************"

foreach x in $models  {
		display "TABULATE OUTPUTELASTICITIES AND RTS FOR MODEL WITHOUT WITHOUT NEGATIVE VALUES:"
		display "     *******     `x'      **********        "
		cap noisily fdztabstat elas_mat_`x' elas_lab_`x' elas_kap_`x' RTS_`x' , statistics(mean var sd p10 p25 p50 p75 p90 count)
		display "*****************************************************"
		display "*************     NOW PER WZ!!!!      ***************"
		display "TABULATE OUTPUTELASTICITIES AND RTS PER WZ FOR MODEL WITHOUT WITHOUT NEGATIVE VALUES:"
		display "     **********     `x'    **************        "
		cap noisily fdztabstat elas_mat_`x' elas_lab_`x' elas_kap_`x' RTS_`x'  ,by(wz_unr_like_statistical2) statistics(mean sd  p10 p25 p50 p75 p90  count)
		display "*****************************************************"
		






}
drop neg_*
// close calculate elasticities and RTS



}


//cost share estimates
***********************************************************************
{
	
	
gen cap_cost = `Capital'_nom *0.08

//cost share 4
//base

bys wz_4_numeric_help jahr: egen elas_lab_baseCS = median(wageB_nom/(wageB_nom +mat_nom +cap_cost))
by wz_4_numeric_help jahr: egen elas_mat_baseCS = median(mat_nom/(wageB_nom +mat_nom +cap_cost))
by wz_4_numeric_help jahr: egen elas_kap_baseCS = median(cap_cost/(wageB_nom +mat_nom +cap_cost))


gen TFP_baseCS =  ln`Revenue'_cpi  -  elas_lab_baseCS *lab -  elas_mat_baseCS * mat -   elas_kap_baseCS *kap 
gen TFPQ_baseCS = ln`Revenue'  -  elas_lab_baseCS *lab -  elas_mat_baseCS * mat -   elas_kap_baseCS *kap

gen RTS_baseCS = elas_lab_baseCS +  elas_mat_baseCS +  elas_kap_baseCS


gen adjustment_baseCS = (ln`Revenue'_cpi)  - TFP_baseCS 


//cost share 2
//base

bys wz_unr_like_statistical2 jahr: egen elas_lab_baseCS2 = median(wageB_nom/(wageB_nom +mat_nom +cap_cost))
by wz_unr_like_statistical2 jahr: egen elas_mat_baseCS2 = median(mat_nom/(wageB_nom +mat_nom +cap_cost))
by wz_unr_like_statistical2 jahr: egen elas_kap_baseCS2 = median(cap_cost/(wageB_nom +mat_nom +cap_cost))


gen TFP_baseCS2 =  ln`Revenue'_cpi  -  elas_lab_baseCS2 *lab -  elas_mat_baseCS2 * mat -   elas_kap_baseCS2 *kap 
gen TFPQ_baseCS2 = ln`Revenue'  -  elas_lab_baseCS2 *lab -  elas_mat_baseCS2 * mat -   elas_kap_baseCS2 *kap

gen RTS_baseCS2 = elas_lab_baseCS2 +  elas_mat_baseCS2 +  elas_kap_baseCS2


gen adjustment_baseCS2 = (ln`Revenue'_cpi)  - TFP_baseCS2 


//possible extent for cost share models herel
global models   $models  // baseCS   baseCS2  
global estimation_models   $estimation_models  // baseCS   baseCS2   






log using "$savepath\Freigabe\\Log_KDFV_FREIGABE.log", append name(Log_KDFV_FREIGABE)



		display "***********************************"
		display "***********************************"
		display "***********************************"
		display "***********************************"

foreach x in baseCS   baseCS2      {
		display "TABULATE OUTPUTELASTICITIES AND RTS FOR MODEL WITH NEGATIVE VALUES:"
		display "     *******     `x'      **********        "
		cap noisily fdztabstat elas_mat_`x' elas_lab_`x' elas_kap_`x' RTS_`x'  , statistics(mean var sd p10 p25 p50 p75 p90 count)
		display "*****************************************************"
		display "*************     NOW PER WZ!!!!      ***************"
		display "TABULATE OUTPUTELASTICITIES AND RTS PER WZ FOR MODEL WITH NEGATIVE VALUES:"
		display "     **********     `x'    **************        "
		cap noisily fdztabstat elas_mat_`x' elas_lab_`x' elas_kap_`x' RTS_`x'  ,by(wz_unr_like_statistical2) statistics(mean sd  p10 p25 p50 p75 p90  count)
		display "*****************************************************"


}

log close Log_KDFV_FREIGABE

}



//define market power, clean extreme values, and demean TFP
***********************************************************************
{


foreach model in $models  {

 //demean TFP
  bys wz_unr_like_statistical2 :	egen Mwz_TFP_`model' = mean(TFP_`model')
  gen  TFPwz_`model'   = TFP_`model' -  Mwz_TFP_`model'
  

}

//
*********
// market power
foreach x in  $models    {  
gen DM_`x' =(elas_lab_`x' /elas_mat_`x') * (mat_nom/wageB_nom)
gen MU_mat_`x' = elas_mat_`x' * ((`Revenue'_nom) / mat_nom )
gen MU_lab_`x' = elas_lab_`x' * ((`Revenue'_nom) / wageB_nom )
}



//drop outlier  and define sample
foreach x in  $models     {


  
  
     egen MU_mat_`x'_p99 =   pctile(MU_mat_`x') , p(99)
  egen MU_mat_`x'_p01 =   pctile(MU_mat_`x') , p(1)
    egen DM_`x'_p99 =   pctile(DM_`x') , p(99)
     egen DM_`x'_p01 =   pctile(DM_`x') , p(1)
  bys wz_unr_like_statistical2: egen TFP_`x'_p99 =   pctile(TFP_`x') , p(99)
   bys wz_unr_like_statistical2: egen TFP_`x'_p01 =   pctile(TFP_`x') , p(1)
 egen TFPwz_`x'_p99 =   pctile(TFPwz_`x') , p(99)
 egen TFPwz_`x'_p01 =   pctile(TFPwz_`x') , p(1)

 


 if "$out_MP"  =="YES"   &   "$IWH_run" != "YES" {  

 replace TFP_`x' = . if  (TFP_`x'  > TFP_`x'_p99) | (TFP_`x' < TFP_`x'_p01)
  replace TFPwz_`x' = . if  (TFPwz_`x'  > TFPwz_`x'_p99) | (TFPwz_`x' < TFPwz_`x'_p01)
  
  
 }
 
 

}



 cap drop *p99  *p01 

}



//define lags and sample
***********************************************************************
{
xtset unr jahr
gen lnrealwage = ln(realwage)


gen l_va_lprod = l.va_lprod
gen l_lnrealwage = l.lnrealwage

 
cap gen lnL_FTE = ln(L_FTE)
cap gen lnK_L =  ln(K_L)
cap gen lnK = ln(KMRSoft)
cap gen lnNumP = ln(NumP)
cap gen lnM = ln(Mb_E)
cap gen lnR = ln(Y_allE)
 
 gen wage_nom_nom =  wage_nominal

	
	gen help  = wage_nominal/ PriceWage

	replace wage_nominal = help
	
	drop help 
	
	

 
foreach x of varlist DM*  MU_mat*  MU_lab*   elas_kap_*    elas_mat_*    elas_lab_*   RTS_*   adjustment*   lnL_FTE  lnK_L  lnM lnR  Y_allF_nom  L_FTE  TFP* TFPQ* pf  va_lprod Y_allE lnrealwage wage_nominal wage_nom_nom      {

cap noisily gen l_`x' = l.`x'
cap noisily gen l2_`x' = l.`x'
cap noisily gen d_`x' = s.`x'
cap noisily gen d_l_`x' = s.l_`x'
cap noisily gen ln_`x' = ln(`x')
cap noisily gen l_ln_`x' = l.ln_`x'
cap noisily gen l2_ln_`x' = l2.ln_`x'
cap noisily gen d_ln_`x' = s.ln_`x'
cap noisily gen l_d_ln_`x' = l.d_ln_`x'
cap noisily gen d2_ln_`x' = s2.ln_`x' 

}







gen lnwage_nominal = ln(wage_nominal)
cap noisily gen l_lnwage_nominal = l.ln_wage_nominal
cap noisily gen l2_lnwage_nominal = l2.ln_wage_nominal
cap noisily gen d_l_lnwage_nominal = s.l_lnwage_nominal
cap noisily gen d_lnwage_nominal = 	 s.lnwage_nominal  


gen lnwage_nom_nom = ln(wage_nom_nom)
cap noisily gen l_lnwage_nom_nom = l.ln_wage_nom_nom
cap noisily gen l2_lnwage_nom_nom = l2.ln_wage_nom_nom
cap noisily gen d_l_lnwage_nom_nom = s.l_lnwage_nom_nom
cap noisily gen d_lnwage_nom_nom = 	 s.lnwage_nom_nom 

 
gen weight_firmlevel =  0.5*(L_FTE + l.L_FTE)
gen weight_employ = weight_firmlevel
gen weight_cap =  0.5*(l.`Capital'+`Capital')
gen weight_mat =  0.5*(l.Mb_E+Mb_E)
gen weight_R =  0.5*(l.Y_allE+Y_allE)






 
 
 gen log_growth_L = lnL_FTE - l_lnL_FTE





cap gen f_gr_firm = gr_firm


cap egen group_wz_unr = group(wz_4_numeric_help unr)
cap egen group_wz_year = group(wz_4_numeric_help jahr)
cap replace  group_wz_unr =. if wz_4_numeric_help ==.
cap replace  group_wz_year =. if wz_4_numeric_help ==.

cap egen group_wz2_unr = group(wz_unr_like_statistical2 unr)
cap egen group_wz2_year = group(wz_unr_like_statistical2 jahr)
cap replace  group_wz2_unr =. if wz_unr_like_statistical2 ==.
cap replace  group_wz2_year =. if wz_unr_like_statistical2 ==.



local fixed_effects     group_wz2_year 




		 //DEFINE SAMPLE
 *****************
  if   "$IWH_run" != "YES"  {

// lnrealwage   MU_mat_base  DM_base   elas_lab_base   adjustment_base
// lnrealwage   MU_mat_base  DM_base   elas_lab_base   adjustment_base

cap drop flag_sample
 gen flag_sample  = 1 if   DM_base !=. &  MU_mat_base !=.  & TFPwz_base !=.  & elas_lab_base !=.  & lnrealwage  !=.    //& adjustment_base  !=.   &   va_lprod !=.  & lnL_FTE !=. 
 
cap drop helpsample_RD1
//  l_lnrealwage   l_MU_mat_base  l_DM_base   l_elas_lab_base   l_adjustment_base   
 reghdfe  gr_firm   l_TFPwz_base  l_lnL_FTE              [aweight=weight_employ]    , absorb(group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
gen helpsample_RD1 = e(sample)
 //l_lnrealwage   l_MU_mat_base  l_DM_base   l_elas_lab_base   l_adjustment_base  
  reghdfe  gr_firm   l_va_lprod  l_lnL_FTE                  [aweight=weight_employ]    , absorb(group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
  gen helpsample_RD2 = e(sample)
  
 keep if helpsample_RD1 ==1     & flag_sample ==1  //& helpsample_RD2 ==1 
   
drop  helpsample_RD1  helpsample_RD2

     }
 
  // END DEFINE SAMPLE
 *****************


 
 
 

//size flags
gen small100 = 1 if  L_FTE <100   & L_FTE !=.  & L_FTE !=0
replace  small100 = 0 if   L_FTE >=100   & L_FTE !=.  & L_FTE !=0


gen large100 = 1 if  L_FTE >=100   & L_FTE !=.  & L_FTE !=0
replace large100 = 0 if  L_FTE <100   & L_FTE !=.  & L_FTE !=0


gen l_small100 = 1 if  l_L_FTE <100   & l_L_FTE !=.  & l_L_FTE !=0
replace  l_small100 = 0 if   l_L_FTE >=100   & l_L_FTE !=.  & l_L_FTE !=0


gen l_large100 = 1 if  l_L_FTE >=100   & l_L_FTE !=.  & l_L_FTE !=0
replace l_large100 = 0 if  l_L_FTE <100   & l_L_FTE !=.  & l_L_FTE !=0






}






//define weighted averages weight is the same as for the JR
***********************************************************************
{
gen all = 1 

 foreach x in small100  large100   all   {



 
gen help_fl_wage_nominal  = 1 if  wage_nominal  !=.  & jahr != .  & l_firm !=.  & `x' ==1 
gen help_fl_wage_nominal2  = 1 if  wage_nominal  !=.  & jahr != .  & l_firm !=.  & `x' ==1
replace help_fl_wage_nominal  =0 if help_fl_wage_nominal !=1
bys   jahr   : egen tot_w_wage_nominal = total(l_firm * help_fl_wage_nominal) 

by   jahr   : egen W_AVR_wage_nominalG_`x' = total(   wage_nominal  * l_firm/tot_w_wage_nominal * help_fl_wage_nominal )



cap drop help_*
cap drop tot_w*


foreach model in base  {


cap drop help_*
cap drop tot_w*




//LMP
gen help_fl_DMG_`model'  = 1 if DM_`model' !=.  & jahr != . &  l_firm !=.   & `x' ==1
gen help_fl_DMG_`model'2  = 1 if DM_`model' !=.  & jahr != . &  l_firm !=.   & `x' ==1
replace help_fl_DMG_`model'  =0 if help_fl_DMG_`model' !=1
by   jahr   : egen tot_w_l_DMG_`model' = total(l_firm * help_fl_DMG_`model') 


by   jahr   : egen W_AVR_DMG_`model'_`x' = total(  DM_`model' * l_firm/tot_w_l_DMG_`model' * help_fl_DMG_`model' )




//PMP
gen help_fl_MUG_`model'  = 1 if MU_mat_`model' !=.  & jahr != .   & l_firm != .   & `x' ==1
gen help_fl_MUG_`model'2  = 1 if MU_mat_`model' !=.  & jahr != .   & l_firm != .   & `x' ==1
replace help_fl_MUG_`model'  =0 if help_fl_MUG_`model' !=1
by   jahr   : egen tot_w_l_MUG_`model' = total(l_firm * help_fl_MUG_`model') 


by   jahr   : egen W_AVR_MUG_`model'_`x' = total(  MU_mat_`model' * l_firm/tot_w_l_MUG_`model'  * help_fl_MUG_`model')


//overall MP
gen help_fl_MPG_`model'  = 1 if MU_lab_`model' !=.  & jahr != .   & l_firm != .   & `x' ==1
gen help_fl_MPG_`model'2  = 1 if MU_lab_`model' !=.  & jahr != .   & l_firm != .   & `x' ==1
replace help_fl_MPG_`model'  =0 if help_fl_MPG_`model' !=1
by   jahr   : egen tot_w_l_MPG_`model' = total(l_firm * help_fl_MPG_`model') 


by   jahr   : egen W_AVR_MPG_`model'_`x' = total(  MU_lab_`model' * l_firm/tot_w_l_MPG_`model'  * help_fl_MPG_`model')



 

//output elas lab
gen help_fl_ELLAG_`model'  = 1 if elas_lab_`model'  !=.  & jahr != .  & l_firm !=.   & `x' ==1   
gen help_fl_ELLAG_`model'2  = 1 if elas_lab_`model'  !=.  & jahr != .  & l_firm !=.   & `x' ==1      
replace help_fl_ELLAG_`model'  =0 if help_fl_ELLAG_`model' !=1
by   jahr   : egen tot_w_ELLAG_`model' =  total(l_firm * help_fl_ELLAG_`model')  


by   jahr   : egen W_AVR_ELLAG_`model'_`x' = total(  elas_lab_`model'  * l_firm/tot_w_ELLAG_`model'  * help_fl_ELLAG_`model')  

//output elas mat
gen help_fl_ELMAG_`model'  = 1 if elas_mat_`model'  !=.  & jahr != .   & l_firm !=.   & `x' ==1   
gen help_fl_ELMAG_`model'2  = 1 if elas_mat_`model'  !=.  & jahr != .   & l_firm !=.   & `x' ==1  
replace help_fl_ELMAG_`model'  =0 if help_fl_ELMAG_`model' !=1
by   jahr   : egen tot_w_ELMAG_`model' =   total(l_firm * help_fl_ELMAG_`model') 


by   jahr   : egen W_AVR_ELMAG_`model'_`x' = total(  elas_mat_`model'  * l_firm/tot_w_ELMAG_`model' * help_fl_ELMAG_`model' ) 




//output elas kap
gen help_fl_ELKAG_`model'  = 1 if elas_kap_`model'  !=.  & jahr != .    & l_firm !=.   & `x' ==1  
gen help_fl_ELKAG_`model'2  = 1 if elas_kap_`model'  !=.  & jahr != .    & l_firm !=.   & `x' ==1   
replace help_fl_ELKAG_`model'  =0 if help_fl_ELKAG_`model' !=1
by   jahr   : egen tot_w_ELKAG_`model' = total(l_firm * help_fl_ELKAG_`model') 


by   jahr   : egen W_AVR_ELKAG_`model'_`x' = total(  elas_kap_`model'  * l_firm/tot_w_ELKAG_`model'  * help_fl_ELKAG_`model') 



//RTSG
gen help_fl_RTSG_`model'  = 1 if RTS_`model'  !=.  & jahr != .  & l_firm !=.   & `x' ==1     
gen help_fl_RTSG_`model'2  = 1 if RTS_`model'  !=.  & jahr != .  & l_firm !=.   & `x' ==1     
replace help_fl_RTSG_`model'  =0 if help_fl_RTSG_`model' !=1
by   jahr   : egen tot_w_RTSG_`model' = total(l_firm * help_fl_RTSG_`model')  


by   jahr   : egen W_AVR_RTSG_`model'_`x' = total(  RTS_`model'  * l_firm/tot_w_RTSG_`model'  * help_fl_RTSG_`model') 



 
 cap drop help_*
cap drop tot_w*

}
}
 
 


}




//Table A5 Results  summary stats
***********************************************************************
{



display "Firm stats germany"
display "**********************"
cap noisily fdztabstat  L_FTE  gr_firm    TFPwz_base  realwage   MU_mat_base MU_lab_base DM_base    elas_lab_base  elas_kap_base    elas_mat_base    RTS_base       , stat(mean sd  p25 p50 p75 count) 

}



//Table D5 Results  aggregate changes//weighted averages
***********************************************************************
{



display "Aggregate stats germany"
display "**********************"
 foreach x in all small100  large100      {
 foreach model in base   {
cap noisily tabstat  W_AVR_wage_nominalG_`x'    W_AVR_DMG_`model'_`x'   W_AVR_MUG_`model'_`x'    W_AVR_MPG_`model'_`x'    W_AVR_ELLAG_`model'_`x'   W_AVR_ELMAG_`model'_`x'    W_AVR_ELKAG_`model'_`x'    W_AVR_RTSG_`model'_`x'      , by(jahr) stat(mean) 
 }
 }
 
 
}






//Figure 16 - shock dispersion and persistence German data
***********************************************************************
{
preserve

local fixed_effects     group_wz2_year 
	xtset unr jahr 
 keep jahr unr  `fixed_effects'   TFPwz_*    l_TFPwz_*    weight_employ
 
 duplicates drop
 
	
	
	
	xtset unr jahr
cap gen period1 = 1 if jahr >=1995 & jahr  <=1998
cap gen period2 = 1 if jahr >=1999 & jahr  <=2002
cap gen period3 = 1 if jahr >=2003 & jahr  <=2006
cap gen period4 = 1 if jahr >=2007 & jahr  <=2010
cap gen period5 = 1 if jahr >=2011 & jahr  <=2014
cap gen period6 = 1 if jahr >=2015 & jahr  <=2017

cap replace period1 = 0 if jahr <1995 | jahr  >1998
cap replace period2 = 0 if jahr <1999 | jahr  >2002
cap replace period3 = 0 if jahr <2003 | jahr  >2006
cap replace period4 = 0 if jahr <2007 | jahr  >2010
cap replace period5 = 0 if jahr <2011 | jahr  >2014
cap replace period6 = 0 if jahr <2015 | jahr  >2017





foreach model in   $models      { 
	
	
	
	foreach x in  period1   period2   period3   period4   period5   period6  {
	
	gen Il_TFPwz_`model'_`x' =  l_TFPwz_`model' * `x'	
}

	
	
	
gen  shock_`model' = .
	

	

	
cap drop residual 
cap noisily reghdfe   TFPwz_`model'   Il_TFPwz_`model'_period*  [aweight=weight_employ]   , absorb(`fixed_effects' ) vce(cluster unr) 	residuals(residual)  $iwh_run_reghdfe
capture noisily  outreg2 using "$savepath\Freigabe\techno_LMP\Regs\Persistence `model' weighted_6.xls", append ctitle(  `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )


			
		
		foreach z in   period1   period2   period3   period4   period5   period6     {
cap drop residual 
cap noisily reghdfe  TFPwz_`model'  l_TFPwz_`model' if `z' ==1 [aweight=weight_employ]   , absorb(`fixed_effects' ) vce(cluster unr) 	residuals(residual)  $iwh_run_reghdfe
capture noisily  outreg2 using "$savepath\Freigabe\techno_LMP\Regs\Persistence `model' weighted_6.xls", append ctitle(  `fixed_effects' `z') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )

	cap noisily predict res , res 
	cap noisily replace shock_`model' = res 	if `z' ==1
	cap noisily drop res
	clear results
			
	
		}
	
	
	
	
	bys jahr: egen sd_shock_`model'= sd(shock_`model')
	
	
	keep jahr sd_shock_`model'
	duplicates drop 
	
	
	
	tabstat sd_shock_`model' , by(jahr)
	

	

}	
restore
}



//Figure 15, Table D1, Table D4 - responsiveness regressions in various forms
*******************************************************************************************
{
	
foreach model in   $models      { 
preserve
xtset unr jahr
cap gen period1 = 1 if jahr >=1995 & jahr  <=1998
cap gen period2 = 1 if jahr >=1999 & jahr  <=2002
cap gen period3 = 1 if jahr >=2003 & jahr  <=2006
cap gen period4 = 1 if jahr >=2007 & jahr  <=2010
cap gen period5 = 1 if jahr >=2011 & jahr  <=2014
cap gen period6 = 1 if jahr >=2015 & jahr  <=2017

cap replace period1 = 0 if jahr <1995 | jahr  >1998
cap replace period2 = 0 if jahr <1999 | jahr  >2002
cap replace period3 = 0 if jahr <2003 | jahr  >2006
cap replace period4 = 0 if jahr <2007 | jahr  >2010
cap replace period5 = 0 if jahr <2011 | jahr  >2014
cap replace period6 = 0 if jahr <2015 | jahr  >2017


gen trend = jahr - $start_year_estimate - 1


foreach y in    l_TFPwz_`model'    l_lnL_FTE       {
	
	cap gen `y'_jahr = `y' * trend	
	
	
}



foreach z in   period1   period2   period3   period4   period5   period6    {
	

	
	

	 gen D`z'_l_TFPwz_`model' = `z' *  d_l_TFPwz_`model'
	 gen `z'_l_TFPwz_`model' = `z' *  l_TFPwz_`model'	
	 gen `z'_l_lnL_FTE = `z' *  l_lnL_FTE		
	
}




foreach y in     l_lnrealwage   l_ln_MU_mat_`model'  l_ln_DM_`model'   l_ln_elas_lab_`model'    l_adjustment_`model'     l_ln_RTS_`model'       {
	
	
	cap gen `y'_l_TFPwz_`model'    = `y' * l_TFPwz_`model' 

}




//Regressions	
	

//Figure 15 (a)		
cap noisily reghdfe  gr_firm      period1_l_TFPwz_`model'   period2_l_TFPwz_`model'   period3_l_TFPwz_`model'   period4_l_TFPwz_`model'   period5_l_TFPwz_`model'   period6_l_TFPwz_`model'  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE           if l_large100 ==1         [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted period `model' `rr'.xls", append ctitle(   gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )
	
cap noisily reghdfe  gr_firm      period1_l_TFPwz_`model'   period2_l_TFPwz_`model'   period3_l_TFPwz_`model'   period4_l_TFPwz_`model'   period5_l_TFPwz_`model'   period6_l_TFPwz_`model'  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE           if l_small100 ==1         [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted period `model' `rr'.xls", append ctitle(   gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )
		

//Figure 15 (b)	
cap noisily reghdfe  gr_firm      Dperiod1_l_TFPwz_`model'   Dperiod2_l_TFPwz_`model'   Dperiod3_l_TFPwz_`model'   Dperiod4_l_TFPwz_`model'   Dperiod5_l_TFPwz_`model'   Dperiod6_l_TFPwz_`model'        if  l_large100== 1              [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted period `model' FD `rr'.xls", append ctitle(   gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )

cap noisily reghdfe  gr_firm      Dperiod1_l_TFPwz_`model'   Dperiod2_l_TFPwz_`model'   Dperiod3_l_TFPwz_`model'   Dperiod4_l_TFPwz_`model'   Dperiod5_l_TFPwz_`model'   Dperiod6_l_TFPwz_`model'        if  l_small100 == 1              [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted period `model' FD `rr'.xls", append ctitle(   gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )
	
	
//column #1 , Table D1
cap noisily reghdfe  gr_firm   l_TFPwz_`model' l_TFPwz_`model'_jahr  l_lnL_FTE     l_lnL_FTE_jahr              [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted trend `model'.xls", append ctitle(gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )

//column #2 , Table D1
cap noisily reghdfe  gr_firm   l_TFPwz_`model'  l_lnL_FTE  c.l_large100#c.l_TFPwz_`model'     l_large100    c.l_large100#c.l_lnL_FTE        [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted `model' largesmall1100.xls", append ctitle(gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )




//column #1 , Table D4  
cap noisily reghdfe  gr_firm   l_TFPwz_`model'  l_lnL_FTE     l_ln_elas_lab_`model'_l_TFPwz_`model'     l_ln_elas_lab_`model'  l_adjustment_`model'_l_TFPwz_`model'     l_adjustment_`model'     l_lnrealwage_l_TFPwz_`model'     l_lnrealwage     l_ln_MU_mat_`model'_l_TFPwz_`model'     l_ln_DM_`model'_l_TFPwz_`model'    l_ln_MU_mat_`model'   l_ln_DM_`model'     l_ln_RTS_`model'_l_TFPwz_`model'     l_ln_RTS_`model'       [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted trend `model'_controls.xls", append ctitle(gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )




foreach y in     l_lnrealwage   l_ln_MU_mat_`model'  l_ln_DM_`model'   l_ln_elas_lab_`model'    l_adjustment_`model'     l_ln_RTS_`model'       {
	
	
	 drop `y'_l_TFPwz_`model'   
	
	 gen  `y'_l_TFPwz_`model'    = `y' *  d_l_TFPwz_`model' 
}






//column #2 , Table D4  		  
 cap noisily reghdfe  gr_firm   d_l_TFPwz_`model'         l_ln_elas_lab_`model'_l_TFPwz    l_adjustment_`model'_l_TFPwz   l_lnrealwage_l_TFPwz   l_ln_MU_mat_`model'_l_TFPwz     l_ln_DM_`model'_l_TFPwz          l_ln_elas_lab_`model'    l_adjustment_`model'   l_lnrealwage   l_ln_MU_mat_`model'     l_ln_DM_`model'     l_ln_RTS_`model'    l_ln_RTS_`model'_l_TFPwz      [aweight=weight_employ]    , absorb(`fixed_effects') vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\final_freigabe\responsiveness    weighted `model' FD_control.xls", append ctitle(gr_firm `fixed_effects') adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )

	
	

restore
}		
}




//Table D2, Figure 17 - responsiveness regressions by quintiles and counterfactual analysis
*******************************************************************************************
{
preserve
local model base
xtset unr jahr
cap gen period1 = 1 if jahr >=1995 & jahr  <=1998
cap gen period2 = 1 if jahr >=1999 & jahr  <=2002
cap gen period3 = 1 if jahr >=2003 & jahr  <=2006
cap gen period4 = 1 if jahr >=2007 & jahr  <=2010
cap gen period5 = 1 if jahr >=2011 & jahr  <=2014
cap gen period6 = 1 if jahr >=2015 & jahr  <=2017

cap replace period1 = 0 if jahr <1995 | jahr  >1998
cap replace period2 = 0 if jahr <1999 | jahr  >2002
cap replace period3 = 0 if jahr <2003 | jahr  >2006
cap replace period4 = 0 if jahr <2007 | jahr  >2010
cap replace period5 = 0 if jahr <2011 | jahr  >2014
cap replace period6 = 0 if jahr <2015 | jahr  >2017






foreach z in   period1   period2   period3   period4   period5   period6    {
	

	 gen `z'_l_TFPwz_`model' = `z' *  l_TFPwz_`model'	
	gen `z'_l_lnL_FTE = `z' *  l_lnL_FTE		
}



	bys   wz_unr_like_statistical2 : egen Q1_L_FTE = pctile(l_lnL_FTE) , p(20)
	by   wz_unr_like_statistical2 :  egen Q2_L_FTE = pctile(l_lnL_FTE) , p(40)
	by   wz_unr_like_statistical2 :  egen Q3_L_FTE = pctile(l_lnL_FTE) , p(60)
	by   wz_unr_like_statistical2 :  egen Q4_L_FTE = pctile(l_lnL_FTE) , p(80)
		
	
	gen flag_Q1_L_FTE = 1 if l_lnL_FTE <= Q1_L_FTE &  l_lnL_FTE !=.
	gen flag_Q2_L_FTE = 1 if l_lnL_FTE > Q1_L_FTE  &  l_lnL_FTE <= Q2_L_FTE &  l_lnL_FTE !=.
	gen flag_Q3_L_FTE = 1 if l_lnL_FTE > Q2_L_FTE  &  l_lnL_FTE <= Q3_L_FTE &  l_lnL_FTE !=.
	gen flag_Q4_L_FTE = 1 if l_lnL_FTE > Q3_L_FTE  &  l_lnL_FTE <= Q4_L_FTE &  l_lnL_FTE !=.
	gen flag_Q5_L_FTE = 1 if l_lnL_FTE > Q4_L_FTE  &  l_lnL_FTE !=. 
	
	
	


//Regressions - Table D2	
**************************
{
//Q1
clear results

cap noisily reghdfe   gr_firm  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE        period1_l_TFPwz_base   period2_l_TFPwz_base   period3_l_TFPwz_base   period4_l_TFPwz_base   period5_l_TFPwz_base   period6_l_TFPwz_base      if flag_Q1_L_FTE ==1                   [aweight=weight_employ]    , absorb(group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\counterfact AER.xls", append ctitle(    group_wz2_year) adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )


cap drop  b_l_lnL_FTE
cap drop  b_period1_l_TFPwz_base
cap drop  b_period2_l_TFPwz_base
cap drop  b_period3_l_TFPwz_base
cap drop  b_period4_l_TFPwz_base
cap drop  b_period5_l_TFPwz_base
cap drop  b_period6_l_TFPwz_base

cap drop  b_period1_l_lnL_FTE 
cap drop  b_period2_l_lnL_FTE 
cap drop  b_period3_l_lnL_FTE 
cap drop  b_period4_l_lnL_FTE 
cap drop  b_period5_l_lnL_FTE 
cap drop  b_period6_l_lnL_FTE 




gen b_period1_l_TFPwz_base = _b[period1_l_TFPwz_base] 
gen b_period2_l_TFPwz_base = _b[period2_l_TFPwz_base] 
gen b_period3_l_TFPwz_base = _b[period3_l_TFPwz_base] 
gen b_period4_l_TFPwz_base = _b[period4_l_TFPwz_base] 
gen b_period5_l_TFPwz_base = _b[period5_l_TFPwz_base] 
gen b_period6_l_TFPwz_base = _b[period6_l_TFPwz_base] 




gen b_period1_l_lnL_FTE  = _b[period1_l_lnL_FTE] 
gen b_period2_l_lnL_FTE  = _b[period2_l_lnL_FTE] 
gen b_period3_l_lnL_FTE  = _b[period3_l_lnL_FTE] 
gen b_period4_l_lnL_FTE  = _b[period4_l_lnL_FTE] 
gen b_period5_l_lnL_FTE  = _b[period5_l_lnL_FTE] 
gen b_period6_l_lnL_FTE  = _b[period6_l_lnL_FTE] 

gen gr_firmWZY_LI_DRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period2_l_TFPwz_base * period2_l_TFPwz_base + b_period3_l_TFPwz_base * period3_l_TFPwz_base + b_period4_l_TFPwz_base * period4_l_TFPwz_base + b_period5_l_TFPwz_base * period5_l_TFPwz_base + b_period6_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q1_L_FTE ==1 
gen gr_firmWZY_LI_CRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period1_l_TFPwz_base * period2_l_TFPwz_base + b_period1_l_TFPwz_base * period3_l_TFPwz_base + b_period1_l_TFPwz_base * period4_l_TFPwz_base + b_period1_l_TFPwz_base * period5_l_TFPwz_base + b_period1_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q1_L_FTE ==1 



//Q2
clear results


cap noisily reghdfe   gr_firm  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE        period1_l_TFPwz_base   period2_l_TFPwz_base   period3_l_TFPwz_base   period4_l_TFPwz_base   period5_l_TFPwz_base   period6_l_TFPwz_base      if flag_Q2_L_FTE ==1                   [aweight=weight_employ]    , absorb(group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\counterfact AER.xls", append ctitle(    group_wz2_year) adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )


cap drop  b_l_lnL_FTE
cap drop  b_period1_l_TFPwz_base
cap drop  b_period2_l_TFPwz_base
cap drop  b_period3_l_TFPwz_base
cap drop  b_period4_l_TFPwz_base
cap drop  b_period5_l_TFPwz_base
cap drop  b_period6_l_TFPwz_base

cap drop  b_period1_l_lnL_FTE 
cap drop  b_period2_l_lnL_FTE 
cap drop  b_period3_l_lnL_FTE 
cap drop  b_period4_l_lnL_FTE 
cap drop  b_period5_l_lnL_FTE 
cap drop  b_period6_l_lnL_FTE 







gen b_period1_l_TFPwz_base = _b[period1_l_TFPwz_base] 
gen b_period2_l_TFPwz_base = _b[period2_l_TFPwz_base] 
gen b_period3_l_TFPwz_base = _b[period3_l_TFPwz_base] 
gen b_period4_l_TFPwz_base = _b[period4_l_TFPwz_base] 
gen b_period5_l_TFPwz_base = _b[period5_l_TFPwz_base] 
gen b_period6_l_TFPwz_base = _b[period6_l_TFPwz_base] 




gen b_period1_l_lnL_FTE  = _b[period1_l_lnL_FTE] 
gen b_period2_l_lnL_FTE  = _b[period2_l_lnL_FTE] 
gen b_period3_l_lnL_FTE  = _b[period3_l_lnL_FTE] 
gen b_period4_l_lnL_FTE  = _b[period4_l_lnL_FTE] 
gen b_period5_l_lnL_FTE  = _b[period5_l_lnL_FTE] 
gen b_period6_l_lnL_FTE  = _b[period6_l_lnL_FTE] 

replace gr_firmWZY_LI_DRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period2_l_TFPwz_base * period2_l_TFPwz_base + b_period3_l_TFPwz_base * period3_l_TFPwz_base + b_period4_l_TFPwz_base * period4_l_TFPwz_base + b_period5_l_TFPwz_base * period5_l_TFPwz_base + b_period6_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q2_L_FTE ==1 
replace gr_firmWZY_LI_CRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period1_l_TFPwz_base * period2_l_TFPwz_base + b_period1_l_TFPwz_base * period3_l_TFPwz_base + b_period1_l_TFPwz_base * period4_l_TFPwz_base + b_period1_l_TFPwz_base * period5_l_TFPwz_base + b_period1_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q2_L_FTE ==1  





//Q3
clear results


cap noisily reghdfe   gr_firm  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE        period1_l_TFPwz_base   period2_l_TFPwz_base   period3_l_TFPwz_base   period4_l_TFPwz_base   period5_l_TFPwz_base   period6_l_TFPwz_base      if flag_Q3_L_FTE ==1                   [aweight=weight_employ]    , absorb(group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\counterfact AER.xls", append ctitle(    group_wz2_year) adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )


cap drop  b_l_lnL_FTE
cap drop  b_period1_l_TFPwz_base
cap drop  b_period2_l_TFPwz_base
cap drop  b_period3_l_TFPwz_base
cap drop  b_period4_l_TFPwz_base
cap drop  b_period5_l_TFPwz_base
cap drop  b_period6_l_TFPwz_base

cap drop  b_period1_l_lnL_FTE 
cap drop  b_period2_l_lnL_FTE 
cap drop  b_period3_l_lnL_FTE 
cap drop  b_period4_l_lnL_FTE 
cap drop  b_period5_l_lnL_FTE 
cap drop  b_period6_l_lnL_FTE 







gen b_period1_l_TFPwz_base = _b[period1_l_TFPwz_base] 
gen b_period2_l_TFPwz_base = _b[period2_l_TFPwz_base] 
gen b_period3_l_TFPwz_base = _b[period3_l_TFPwz_base] 
gen b_period4_l_TFPwz_base = _b[period4_l_TFPwz_base] 
gen b_period5_l_TFPwz_base = _b[period5_l_TFPwz_base] 
gen b_period6_l_TFPwz_base = _b[period6_l_TFPwz_base] 




gen b_period1_l_lnL_FTE  = _b[period1_l_lnL_FTE] 
gen b_period2_l_lnL_FTE  = _b[period2_l_lnL_FTE] 
gen b_period3_l_lnL_FTE  = _b[period3_l_lnL_FTE] 
gen b_period4_l_lnL_FTE  = _b[period4_l_lnL_FTE] 
gen b_period5_l_lnL_FTE  = _b[period5_l_lnL_FTE] 
gen b_period6_l_lnL_FTE  = _b[period6_l_lnL_FTE]  

replace gr_firmWZY_LI_DRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period2_l_TFPwz_base * period2_l_TFPwz_base + b_period3_l_TFPwz_base * period3_l_TFPwz_base + b_period4_l_TFPwz_base * period4_l_TFPwz_base + b_period5_l_TFPwz_base * period5_l_TFPwz_base + b_period6_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q3_L_FTE ==1 
replace gr_firmWZY_LI_CRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period1_l_TFPwz_base * period2_l_TFPwz_base + b_period1_l_TFPwz_base * period3_l_TFPwz_base + b_period1_l_TFPwz_base * period4_l_TFPwz_base + b_period1_l_TFPwz_base * period5_l_TFPwz_base + b_period1_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q3_L_FTE ==1  





//Q4
clear results


cap noisily reghdfe   gr_firm  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE        period1_l_TFPwz_base   period2_l_TFPwz_base   period3_l_TFPwz_base   period4_l_TFPwz_base   period5_l_TFPwz_base   period6_l_TFPwz_base      if flag_Q4_L_FTE ==1                   [aweight=weight_employ]    , absorb(group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\counterfact AER.xls", append ctitle(    group_wz2_year) adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )


cap drop  b_l_lnL_FTE
cap drop  b_period1_l_TFPwz_base
cap drop  b_period2_l_TFPwz_base
cap drop  b_period3_l_TFPwz_base
cap drop  b_period4_l_TFPwz_base
cap drop  b_period5_l_TFPwz_base
cap drop  b_period6_l_TFPwz_base

cap drop  b_period1_l_lnL_FTE 
cap drop  b_period2_l_lnL_FTE 
cap drop  b_period3_l_lnL_FTE 
cap drop  b_period4_l_lnL_FTE 
cap drop  b_period5_l_lnL_FTE 
cap drop  b_period6_l_lnL_FTE 







gen b_period1_l_TFPwz_base = _b[period1_l_TFPwz_base] 
gen b_period2_l_TFPwz_base = _b[period2_l_TFPwz_base] 
gen b_period3_l_TFPwz_base = _b[period3_l_TFPwz_base] 
gen b_period4_l_TFPwz_base = _b[period4_l_TFPwz_base] 
gen b_period5_l_TFPwz_base = _b[period5_l_TFPwz_base] 
gen b_period6_l_TFPwz_base = _b[period6_l_TFPwz_base] 




gen b_period1_l_lnL_FTE  = _b[period1_l_lnL_FTE] 
gen b_period2_l_lnL_FTE  = _b[period2_l_lnL_FTE] 
gen b_period3_l_lnL_FTE  = _b[period3_l_lnL_FTE] 
gen b_period4_l_lnL_FTE  = _b[period4_l_lnL_FTE] 
gen b_period5_l_lnL_FTE  = _b[period5_l_lnL_FTE] 
gen b_period6_l_lnL_FTE  = _b[period6_l_lnL_FTE] 

replace gr_firmWZY_LI_DRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period2_l_TFPwz_base * period2_l_TFPwz_base + b_period3_l_TFPwz_base * period3_l_TFPwz_base + b_period4_l_TFPwz_base * period4_l_TFPwz_base + b_period5_l_TFPwz_base * period5_l_TFPwz_base + b_period6_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q4_L_FTE ==1 
replace gr_firmWZY_LI_CRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period1_l_TFPwz_base * period2_l_TFPwz_base + b_period1_l_TFPwz_base * period3_l_TFPwz_base + b_period1_l_TFPwz_base * period4_l_TFPwz_base + b_period1_l_TFPwz_base * period5_l_TFPwz_base + b_period1_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q4_L_FTE ==1  






//Q5
clear results


cap noisily reghdfe   gr_firm  period1_l_lnL_FTE    period2_l_lnL_FTE       period3_l_lnL_FTE       period4_l_lnL_FTE       period5_l_lnL_FTE       period6_l_lnL_FTE        period1_l_TFPwz_base   period2_l_TFPwz_base   period3_l_TFPwz_base   period4_l_TFPwz_base   period5_l_TFPwz_base   period6_l_TFPwz_base      if flag_Q5_L_FTE ==1                   [aweight=weight_employ]    , absorb(fe_h=group_wz2_year) vce(cluster unr)  $iwh_run_reghdfe
capture outreg2 using "$savepath\Freigabe\techno_LMP\Regs\counterfact AER.xls", append ctitle(    group_wz2_year) adds( # of Clusters , `e(N_clust)' ) addtext( Estimator, `e(model)', Y , `e(depvar)', FE , `absorb_vars' )


cap drop  b_l_lnL_FTE
cap drop  b_period1_l_TFPwz_base
cap drop  b_period2_l_TFPwz_base
cap drop  b_period3_l_TFPwz_base
cap drop  b_period4_l_TFPwz_base
cap drop  b_period5_l_TFPwz_base
cap drop  b_period6_l_TFPwz_base

cap drop  b_period1_l_lnL_FTE 
cap drop  b_period2_l_lnL_FTE 
cap drop  b_period3_l_lnL_FTE 
cap drop  b_period4_l_lnL_FTE 
cap drop  b_period5_l_lnL_FTE 
cap drop  b_period6_l_lnL_FTE 



gen b_period1_l_TFPwz_base = _b[period1_l_TFPwz_base] 
gen b_period2_l_TFPwz_base = _b[period2_l_TFPwz_base] 
gen b_period3_l_TFPwz_base = _b[period3_l_TFPwz_base] 
gen b_period4_l_TFPwz_base = _b[period4_l_TFPwz_base] 
gen b_period5_l_TFPwz_base = _b[period5_l_TFPwz_base] 
gen b_period6_l_TFPwz_base = _b[period6_l_TFPwz_base] 



gen b_period1_l_lnL_FTE  = _b[period1_l_lnL_FTE] 
gen b_period2_l_lnL_FTE  = _b[period2_l_lnL_FTE] 
gen b_period3_l_lnL_FTE  = _b[period3_l_lnL_FTE] 
gen b_period4_l_lnL_FTE  = _b[period4_l_lnL_FTE] 
gen b_period5_l_lnL_FTE  = _b[period5_l_lnL_FTE] 
gen b_period6_l_lnL_FTE  = _b[period6_l_lnL_FTE]  

replace gr_firmWZY_LI_DRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period2_l_TFPwz_base * period2_l_TFPwz_base + b_period3_l_TFPwz_base * period3_l_TFPwz_base + b_period4_l_TFPwz_base * period4_l_TFPwz_base + b_period5_l_TFPwz_base * period5_l_TFPwz_base + b_period6_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q5_L_FTE ==1 
replace gr_firmWZY_LI_CRQL = b_period1_l_lnL_FTE  * period1_l_lnL_FTE  + b_period2_l_lnL_FTE  * period2_l_lnL_FTE  + b_period3_l_lnL_FTE  * period3_l_lnL_FTE  + b_period4_l_lnL_FTE  * period4_l_lnL_FTE  + b_period5_l_lnL_FTE  * period5_l_lnL_FTE  + b_period6_l_lnL_FTE  * period6_l_lnL_FTE   + b_period1_l_TFPwz_base * period1_l_TFPwz_base + b_period1_l_TFPwz_base * period2_l_TFPwz_base + b_period1_l_TFPwz_base * period3_l_TFPwz_base + b_period1_l_TFPwz_base * period4_l_TFPwz_base + b_period1_l_TFPwz_base * period5_l_TFPwz_base + b_period1_l_TFPwz_base * period6_l_TFPwz_base  if flag_Q5_L_FTE ==1  






}




//Predicted JR - Figure 17	 bars #2 #3
******************************************

foreach x in gr_firmWZY_LI_DRQL   gr_firmWZY_LI_CRQL      {
	
	
	
	

gen l_L_`x' = 	((1-0.5*`x'))*L_FTE /((1+ 0.5*`x'))
bys jahr: egen l_TS_`x'	= total(0.5*(L_FTE  + l_L_`x'))
gen l_S_`x'	= (0.5*(L_FTE  + l_L_`x')) / l_TS_`x'
gen l_A_`x' = abs(`x')
gen l_S2_`x'	= l_S_`x'^2
	
	
gen JR_`x'= .	



}

foreach x in gr_firmWZY_LI_DRQL   gr_firmWZY_LI_CRQL          {
	gen AB_`x' = abs(`x')
forval y=1995/2017  {
	
	
	cap noisily su AB_`x' [aweight=l_S_`x' ] if jahr == `y'
	cap noisily replace JR_`x' = r(mean) if jahr == `y'


	
	
	
} 
}


keep jahr JR_*  
duplicates drop 

// Figure 17, bars #2 #3 (we study difference between initial and last year)
************************************************************************************
tabstat JR_*, by(jahr)
	
	
	
restore	
}



//Figure 18 JR framework vs data - data series is also used in Figure 17 bar #1, Figure D1 (German part)
***************************************************************************************************************
{
//one year baseline with sales
foreach model in   base  { // $models  

 preserve	
	
	//for the fake data only
	if  "$IWH_run" == "YES" {
		
		replace jahr = jahr - 7
		keep if jahr <=2002
		
	}
	
	
	
	
	
	xtset unr jahr
	
	
	keep jahr unr   wz_unr_like_statistical2     wage_nominal   Y_allF_nom  l_L_FTE  L_FTE  elas_lab_`model'  MU_mat_`model'  DM_`model'  VA_LPROD      PriceWage /*
	*/  d_ln_wage_nominal  d_ln_Y_allF_nom  d_ln_L_FTE  d_ln_elas_lab_`model'  d_ln_MU_mat_`model'   d_ln_DM_`model'       wz_unr_like_statistical2     wage_nominal   Y_allF_nom  l_L_FTE  L_FTE  elas_lab_`model'  MU_mat_`model'  DM_`model'  VA_LPROD      PriceWage /*
	*/  d_ln_wage_nominal  d_ln_Y_allF_nom  d_ln_L_FTE  d_ln_elas_lab_`model'  d_ln_MU_mat_`model'   d_ln_DM_`model'       d_l_TFP_`model'  d_TFP_`model' /*
	*/  l_d_ln_wage_nominal  l_d_ln_L_FTE  l_d_ln_elas_lab_`model'  l_d_ln_MU_mat_`model'  l_d_ln_DM_`model'   d_l_TFP_`model'  l_d_ln_Y_allF_nom /*
	*/ d_l_adjustment_`model'   d_adjustment_`model'	
 
	
	
	gen help2 = Y_allF_nom /  PriceWage
	
	
	replace Y_allF_nom = help2
	 drop help2
	
	
	


	keep if d_ln_wage_nominal !=. &  d_ln_Y_allF_nom !=. &   d_ln_L_FTE !=. &   d_ln_elas_lab_`model' !=. &   d_ln_MU_mat_`model' !=. &  d_ln_DM_`model' !=. 

	
	//Growthrate/JR in the data
	//JD
xtset unr jahr
				gen l_firm = 0.5*(l_L_FTE+L_FTE)
            	gen gr_firm = (L_FTE- l_L_FTE)/l_firm

			bysort  jahr: egen l_Sam_sam = total(l_firm) if  gr_firm <.
			**total average employment per sector and year 

			gen firm_weight2_sam = l_firm/l_Sam_sam 
			gen gr_firm_weight2_sam =  firm_weight2_sam*gr_firm if gr_firm <.
            
	    **calculating the weighted growth rates with regard to the two-digit level 

            bysort  jahr: egen jcr_H_sam = total(gr_firm_weight2) if gr_firm >0 & gr_firm <.
            **JCR at two digit-level: sum of weighted (positive) firm-level growth rates for each sector
			
            by  jahr: egen jdr_H_sam = total(-gr_firm_weight2) if gr_firm <=0 & gr_firm <.
					
					
					
				by  jahr: egen   jcr_sam   = mean(jcr_H_sam)
				by  jahr: egen   jdr_sam    = mean(jdr_H_sam)
			
	
drop   jdr_H_sam   jcr_H_sam
gen job_dyn_sam =  jcr_sam +  jdr_sam

drop	 l_firm    l_Sam_sam    gr_firm_weight2_sam


//gen vars for growth decomoposition
*****************************************
gen weight = firm_weight2_sam 
gen g = gr_firm
gen g_deltaL = d_ln_L_FTE

//derived growth rate
gen g_derived = d_ln_Y_allF_nom - d_ln_wage_nominal + d_ln_elas_lab_`model' - d_ln_MU_mat_`model' - d_ln_DM_`model'
gen g_pos_derived =  g_derived if g_derived >0 & g_derived !=.
gen g_neg_derived_h =  g_derived if g_derived <=0 & g_derived !=.
gen g_neg_derived =  g_neg_derived_h

//derived growth rate
gen g_derived_weight = (firm_weight2_sam * d_ln_Y_allF_nom) - (firm_weight2_sam * d_ln_wage_nominal) +  (firm_weight2_sam * d_ln_elas_lab_`model') - (firm_weight2_sam * d_ln_MU_mat_`model') - (firm_weight2_sam * d_ln_DM_`model')

gen g_pos_derived_weight =  g_derived_weight if g_derived_weight >0 & g_derived_weight !=.
gen g_neg_derived_weight_h =  g_derived_weight if g_derived_weight <=0 & g_derived_weight !=.
gen g_neg_derived_weight =  g_neg_derived_weight_h


gen flag_g_derived_weight_pos = 1 if g_derived_weight > 0 & g_derived_weight !=.
gen flag_g_derived_weight_neg = 1 if g_derived_weight <= 0 & g_derived_weight !=.	

replace flag_g_derived_weight_pos = 0 if flag_g_derived_weight_pos !=1
replace flag_g_derived_weight_neg = 0 if flag_g_derived_weight_neg !=1


gen flag_gr_firm_pos = 1 if gr_firm > 0 & g_derived_weight !=. & gr_firm !=.
gen flag_gr_firm_neg = 1 if gr_firm <= 0 & g_derived_weight !=. & gr_firm !=.	

replace flag_gr_firm_pos = 0 if flag_gr_firm_pos !=1
replace flag_gr_firm_neg = 0 if flag_gr_firm_neg !=1

gen flag_g_deltaL_pos = 1 if  g_deltaL > 0 & g_derived_weight !=. & g_deltaL !=.
gen flag_g_deltaL_neg = 1 if  g_deltaL <= 0 & g_derived_weight !=. & g_deltaL !=.

replace flag_g_deltaL_pos = 0 if flag_g_deltaL_pos !=1
replace flag_g_deltaL_neg = 0 if flag_g_deltaL_neg !=1

	//aggregate

	by jahr: egen JCR_derived = total(g_derived_weight * flag_g_derived_weight_pos) 
	by jahr: egen JCR = total(gr_firm * weight * flag_gr_firm_pos)
	by jahr: egen JCR_deltaL = total(g_deltaL * weight * flag_g_deltaL_pos)
	


	by jahr: egen JDR_derived = total(g_derived_weight * flag_g_derived_weight_neg)
	by jahr: egen JDR = total(gr_firm * weight  * flag_gr_firm_neg)
	by jahr: egen JDR_deltaL = total(g_deltaL * weight  * flag_g_deltaL_neg)
	
	 
	
	
	keep jahr JCR_derived  JDR_derived  JDR  JCR  JDR_deltaL  JCR_deltaL
	 
	duplicates drop
	
	
	gen JD_derived = JCR_derived + (JDR_derived*(-1))
	gen JD = (JDR *(-1)) + JCR
	gen JD_deltaL = (JDR_deltaL *(-1)) + JCR_deltaL
	

		
	
	keep  jahr JD_derived   JD   JD_deltaL   
	duplicates drop 
	
	//Information for Figue 18, Figure 17 bar #1, Figure D1 (German part) - derived is the version derived from our framework.
	tabstat    JD   JD_deltaL  JD_derived  , by(jahr)
	

	restore
	}
	//close size loop
	
}




//Figure  19 - decomposition of responsiveness
***************************************************************************************************************
{
foreach model in   base  { // $models  
preserve
local fixed_effects     group_wz2_year 


	if  "$IWH_run" == "YES" {
		
		replace jahr = jahr - 7
		keep if jahr <=2002	
	}
	
	
	

	xtset unr jahr

	
	keep jahr unr group_wz2_year  l_d_ln_wage_nom_nom   d_ln_wage_nom_nom    ln_wage_nominal  l_ln_wage_nominal  l_lnrealwage   l_ln_MU_mat_`model'  l_ln_DM_`model'   l_ln_elas_lab_`model'  l_adjustment_`model'  lnrealwage   ln_MU_mat_`model'  ln_DM_`model'   ln_elas_lab_`model'  adjustment_`model'    wz_unr_like_statistical2    Y_allF_nom  wage_nominal   adjustment_base  TFP_base  l_L_FTE L_FTE  elas_lab_`model'  MU_mat_`model'  DM_`model'  VA_LPROD       small100  large100 PriceWage  l_d_ln_wage_nominal d_l_TFP_`model' d_l_adjustment_`model'  l_d_ln_L_FTE   l_d_ln_elas_lab_`model'  l_d_ln_MU_mat_`model'  l_d_ln_DM_`model'  d_l_pf  d_ln_Y_allF_nom  l_d_ln_Y_allF_nom       l_ln_RTS_`model'      /*
	*/ PriceWage d_ln_wage_nominal   d_TFP_`model'  d_adjustment_`model'   d_ln_L_FTE  d_ln_elas_lab_`model'   d_ln_MU_mat_`model'   d_ln_DM_`model' d_pf 


		

	
	gen help2 = Y_allF_nom /  PriceWage
	
	
	replace Y_allF_nom = help2
	 drop help2
	
	
	
	
	
	keep if d_ln_wage_nominal !=. &  d_TFP_`model' !=. &   d_TFP_`model' !=0 &  d_adjustment_`model' !=. &   d_ln_L_FTE !=. &   d_ln_elas_lab_`model' !=. &   d_ln_MU_mat_`model' !=. &  d_ln_DM_`model' !=. 

			
	keep if 	((d_TFP_`model' >= 0.01 &  d_TFP_`model' <= 1) | (d_TFP_`model' <= -0.01  &  d_TFP_`model' >= -1 ))  
		
	
		
		
		
	
	//JD
xtset unr jahr
				gen l_firm = 0.5*(l_L_FTE+L_FTE)
				gen gr_firm = (L_FTE- l_L_FTE)/l_firm
				bysort  jahr: egen l_Sam_sam = total(l_firm) if  gr_firm <.


//gen vars for responsiveness decomoposition
*****************************************
gen weight = l_firm/l_Sam_sam 
 


gen responsiveness_delta = d_ln_L_FTE /  d_TFP_`model'
gen CW_responsiveness_delta=  responsiveness_delta * weight 


sort jahr
foreach y in d_adjustment_`model' d_ln_elas_lab_`model'  d_ln_MU_mat_`model'   d_ln_DM_`model'  d_ln_wage_nominal    {

	
	gen C_`y' = `y'  /   d_TFP_`model'     
	gen CW_`y' = C_`y' 	* weight	 

	by jahr:  egen M_`y' = total(CW_`y')
	
}




foreach y in    responsiveness_delta {
by jahr:  egen M_`y' = total(CW_`y')




}	



keep jahr M_* 

gen M_sales =  M_d_adjustment_`model'  + 1
  drop  M_d_adjustment_`model' 
order jahr   M_*     
duplicates drop
	
	save  "$savepath\Freigabe\techno_LMP\resp_deco.dta" , replace

	

	
	
	

	restore
	}
	//close size loop
	
}	


}
//close chapter




 
 
 



