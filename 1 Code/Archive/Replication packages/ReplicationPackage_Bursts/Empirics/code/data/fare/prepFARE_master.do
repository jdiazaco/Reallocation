* ========================================================== *
* * This do file clean and append FARE dta from 2008 to 2019
* ========================================================== *

/* ======================================
* 1) YEAR-BY-YEAR CLEANING
====================================== */

foreach year of numlist $yr_start/$yr_end {

	use "$path/data/raw/fare/fare`year'.dta", clear

	* Treatment specific to FARE 2008
	*********************************
	if(`year' == 2008) {
			
			// Removing _08 from var names
				
			// list of vars to clean
			local SASvarList ///
						siren_08 IFRP_08 NOM_08 reg_08 DEP_08 NACE_08 EFF_ETP_08 EFF_3112_08  ///
						appgr_08 TYPIMPO_08 DATCR_08 DATCS_08 APE_08 mdst_08 CATOTAL_08  ///
						AUTPREX_08 PRODIMM_08 PRODSTO_08  VARSTMA_08 VARSTMP_08 ///
						ACHAMAR_08 ACHAMPR_08 AUTACHA_08 AUTCHEX_08 ///
						SALTRAI_08 CHARSOC_08 ///
						IMMOCOR_08 IMMOINC_08 ///
						VABCF_08 VAHT_08  EFFSALM_08 

			// loop over the exported vars
			foreach SASvar in `SASvarList' {
				local varRoot = lower(subinstr("`SASvar'", "_08","",.)) // create new name
				local lowerVar = lower("`SASvar'")	// this is because export from SAS yielded variable names in lowercase in Stata
				rename `lowerVar' `varRoot'			// do the renaming
			}
			// end of the loop to remove 08 from variable name
	
		}
	
	* Treatment specific to FARE 2009 AND UP
	****************************************
	else if (`year' >= 2009) { 

		
		* Convert variable names to "standard" ones
			
			//
			rename *, lower
			
			// Capital
			rename immo_corp 	immocor
			rename immo_inc 	immoinc
			
			// Investment
			// egen invavap = rowtotal(immo_corp immo_inc) // only if one 
			
			// Birth date
			rename ent_cre_daaaammjj datcr // needs to be converted to numeric later on in varTransformation.do
			
			// Exit date
			*gen datcs = stat_etat_daaaammjj if(stat_etat == "6") // stat_etat and stat_etat_daaaammjj are used in conjunction to determine birth and death

			// Sectoral classification
			rename ape_diff ape
			
			// Bunch of renames
			rename redi_r310 	catotal
			rename redi_r312 	prodimm /* PRODIMM */
			rename redi_r311 	prodsto /* PRODSTO */
			rename redi_r211 	varstma  /* VARSTMA_08 */
			rename redi_r213 	varstmp /* VARSTMP_08*/
			rename redi_r210 	achamar /* ACHAMAR */
			rename redi_r212 	achampr /* ACHAMPR */
			rename redi_r214 	autacha /* AUTACHA */
			rename redi_r222 	autchex /* AUTCHEX */
			rename redi_r216 	saltrai /* SALTRAI */ 
			rename redi_r217 	charsoc /* CHARSOC */
			rename redi_r003	vaht	/* VAHT */ 
			rename r004 	 	vabcf	/* VABCF */
			if `year' == 2017{
				gen redi_e001 = .
			}
			rename redi_e001	effsalm_3112 // MISSING FOR YEAR 2017 - CHANGE IN DADS COLLECTION
			rename redi_e200	effsalm
			rename liasse_source typimpo
			
		
		* Drop micro-BIN/micro-BIC
		count if typimpo=="21" | typimpo=="23" | typimpo=="99"
		di "`r(n)' obs dropped bc micro-BIN/BIC"
		sum effsalm_3112 if typimpo=="21" | typimpo=="23" | typimpo=="99"
		// large majority micro-enterprises but there are some outliers
		drop if typimpo=="21"
		drop if typimpo=="23"
		drop if typimpo=="99"
	
		
		}
		
			
	* Variable transformation
	*************************
		
	do "$path/code/data/fare/genvars.do" `year'

	
	* Transform NAF into NACE
	*************************
	
	// For years after 2008 the new NAF Rev 2 is used and it is a 5 digit 
	// classification with the first 4 coinciding with NACE.
	
	rename ape nace4_rev2
	replace nace4_rev2 = substr(nace4_rev2, 1, 4)
	destring nace4_rev2, replace force
	count if nace4_rev2 == .
	di "`r(n)' no obs with missing industry classification"
	drop if nace4_rev2 == .
	

	
	
	* Estimate no. of employees for year 2017
	*****************************************
	// Break in the time series bc INSEE started using info from DADS from 2017. As a result, only empl var on fte is available for this year.
	if `year' == 2017 {
		replace employees_hc = employees_fte if employees_hc == .
	}	
	
	* Save dataset
	tempfile fare`year'temp
	save `fare`year'temp'
	
	erase "$path/data/raw/fare/fare`year'.dta"
	
}
	
erase "$path/data/raw/fare/fare2008.dta"
erase "$path/data/raw/fare/fare2009.dta" 	
	
/* ======================================
* 2) CREATE AND CLEAN PANEL DTA
====================================== */	

* Append all years
******************
clear
foreach year of numlist $yr_start/$yr_end {
	append using `fare`year'temp'
}
order year siren nace4_rev2	
sort year siren nace4_rev2


* Consolidation
***************
// consistent with EAP

do "$path/code/data/fare/consolidation_fare.do"


* Treatment common to all years
*******************************

do "$path/code/data/fare/cleanallyears.do"


// Drop year 2008
// we do not need it in the analysis and employment var has many missings
drop if year == 2008



* Label vars
************
label var employees_fte "number of employees (FTE)"    
label var catotal "turnover"
label var nace4_rev2 "NACE rev.2 4-digit"
label var birthyear "legal birth year"
label var birthyearecon "economic birth year"
label var exityearecon "economic exit year"


* Save panel FARE dta
*********************
compress
save "$path/data/processed/FAREpanel_$yr_start$yr_end.dta", replace
	
