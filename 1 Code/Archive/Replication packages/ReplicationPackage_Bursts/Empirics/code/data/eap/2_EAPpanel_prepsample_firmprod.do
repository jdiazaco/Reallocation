/*	============================================================
	This do file prepare the panel EAP dta at firm-product level
	
	This do file performs the following tasks:
		1. Keep only FARE-EAP matched firms
		2. Keep only manufacturing firms (as reported in FARE)
		3. Keep only firms in EAP1 based on list provided by INSEE
		
	============================================================	*/
	


*==============================================================================*

foreach pc in correspc10 prodfra10 pc8 pc6 pc4 {

	use "$path/data/processed/EAP`pc'_${yr_start}${yr_end}_firmprod.dta", clear
	

	* Merge with FARE
	*****************
	merge m:1 year siren using "$path/data/processed/FAREpanel_${yr_start}${yr_end}.dta", ///
		keep(1 3) keepusing(nace4_rev2 birthyear birthyearecon exityear employees_fte employees_hc catotal immoinc)
	count if _m == 1	
	display "`r(N)' obs not matched with FARE"
	tab year if _m == 1
	drop if _m == 1
	drop _m	


			
	
	* Keep manufacturing firms
	**************************
	// Use FARE NACE 4-digit rev 2 industry classification
	tostring nace4_rev2, replace
	gen nace2_rev2 = substr(nace4_rev2,1,2)
	destring nace4_rev2 nace2_rev2, replace
	count if nace2_rev2<13 | nace2_rev2>32
	display "`r(N)' obs not in manufacturing"
	drop if nace2_rev2<13 & nace2_rev2!=. // 10,11 & 12 should not be in the EAP
	*drop if nace2_rev2==16 // 16 - Wood manufacturing should not be in the EAP
	drop if nace2_rev2>32 & nace2_rev2!=.
	drop nace2_rev2
	
			

	
	
	
	* Keep only firms in sample EAP1
	********************************
	
	* import list sirens in EAP1
	preserve
		clear 
		import delimited using "$path/data/external/INSEE/EAP_listsirens_bysample.csv"
		rename *, lower
		keep if c_enquete == "EAP1"
		drop c_enquete
		rename millesime year
		keep siren
		duplicates drop
		forvalue i = 1(1)9 {
			replace siren = "0" + siren if length(siren)<9
		}
		tempfile eap1
		save `eap1'
	restore
	
	* keep all firms in the EAP1 list
	merge m:1 siren using `eap1', keep(1 3)
	count if _merge == 1
	display "`r(N)' obs dropped bc not in EAP1"
	keep if _m == 3
	drop _m
			

	
	* Save final dta
	compress
	save "$path/data/processed/EAP`pc'_${yr_start}${yr_end}_OnlyEAP1_firmprod.dta", replace


}


