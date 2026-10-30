/*	============================================================
	This do file creates a panel based on contour data files
	============================================================	*/


	
* Create list of year-sirens in EAP
***********************************

forvalues year = $yr_start/$yr_end {
	
	* import data
	use "$path/data/raw/eap/eap`year'.dta", clear
	
	* rename varsr
	rename *, lower

	* clean siren
	tostring siren, replace
	forvalue i = 1(1)9 {
		replace siren = "0" + siren if length(siren)<9
	}
	
	* drop previous year information
	keep if millesime_ref != ""
	drop c_milmoins1 val_ref_nmoins1
	drop c_unite_var_nmoins1 prodfra_n_moins1
	
	* rename vars
	rename millesime_ref year
	rename val_ref value
	rename prodfra_n product
	rename c_variable model
	rename c_unite_var_n unit
	rename c_etat_val quality_check
	rename c_ape_lanc ape_eap
	cap rename c_orig_val quality_info // non-reponse totale (12); reponse de l'entreprise (20)
	rename c_naf_rev2 naf_rev2
	
	* drop useless vars
	drop raison_sociale c_dep c_cpf45 nat_prodfra_n c_evol_prodfra c_ape_cour ape_eap naf_rev2
	if `year' == 2019 {
		drop groupe ca_f achats_f
	}
	
	* destring year
	destring year, replace
	
	* save dta
	keep year siren
	duplicates drop year siren, force
	
	tempfile temp_eap_`year'
	save `temp_eap_`year''
	
}

clear
forvalues year = $yr_start/$yr_end {
		
		append using `temp_eap_`year''
		erase `temp_eap_`year''
}

keep year siren
duplicates drop year siren, force
save "$path/data/processed/EAP_list_sirenyear.dta", replace

	
	
	
	
	
********************************************************************************
*** Create raw panel dta

* 2009 - 2015
*************

forvalues year = $yr_start / 2015 {
	
	*** Import data
	***************
	
	use "$path/data/raw/contours/contour_`year'.dta", clear
	
	rename *, lower
	
	* keep siren of profiled companies and legal units
	cap keep ident_ep ident_ul taux ape_ep
	
	* clean sirus_id
	assert length(ident_ep) == 9
	
	* clean siren
	foreach var in ident_ul {
		tostring `var', replace
		forvalue i = 1(1)9 {
			replace `var' = "0" + `var' if length(`var')<9
		}
	}
	
	* gen var year
	gen year = `year'
	
	
	*** Save data
	******************
	
	rename ident_ep sirus_id
	rename ident_ul siren
	rename ape_ep ape_sirus
	
	tempfile contour_`year'
	save `contour_`year''

}


* 2016 - 2019
*************

// For these years the profiling is systematic and all firms with two or more legal units are profiled

forvalues year = 2016 / $yr_end {
	
	*** Import data
	***************
	
	use "$path/data/raw/contours/contour_`year'.dta", clear
	
	* keep siren of profiled companies and legal units
	cap keep sirus_id siren taux ape_ep
	
	* clean sirus_id
	assert length(sirus_id) == 9
	
	* clean siren
	foreach var in siren {
		tostring `var', replace
		forvalue i = 1(1)9 {
			replace `var' = "0" + `var' if length(`var')<9
		}
	}
	
	* gen var year
	gen year = `year'
	
	
	*** Save data
	******************
	
	rename ape_ep ape_sirus

	tempfile contour_`year'
	save `contour_`year''

}


* Create panel data
*******************

clear
forvalues year = $yr_start/$yr_end {
	append using `contour_`year''
}

order year


* Clean multiple profiled companies
***********************************
// There are few legal units (less than 0.05%) belonging to multiple profiled companies
// due to joint ventures, fragmented ownership, typos
// Solution: keep the profiled company with the largest ownership share 

gen taux_inv = - taux
bys siren year (taux_inv): gen n = _n
drop if n>1
drop taux n taux_inv


save "$path/data/processed/contour_panel_2010_2019.dta", replace



* Create contour for sumbsample of profiled companies in EAP
************************************************************

* use list of year-sirens
use "$path/data/processed/EAP_list_sirenyear.dta", clear

* keep profiled companies (i.e. all sirens starting with a P) 
// also include historical groups
keep if substr(siren,1,1)=="P"

* merge legal units from countour files
rename siren sirus_id
joinby year sirus_id using "$path/data/processed/contour_panel_${yr_start}_${yr_end}.dta"

* create profile companies network of affiliates consistent over time
keep siren sirus_id
duplicates drop 

* identify 5 historical groups
gen histgroup = 0
replace histgroup = 1 if 	sirus_id == "P12000019" | sirus_id == "P12000020" | sirus_id == "P12000021" | 	///
							sirus_id == "P12000022" | sirus_id == "P12000030"
							
* save historical groups' countour
preserve
	keep if histgroup == 1
	drop histgroup
	save "$path/data/processed/histgroups_contour_${yr_start}_${yr_end}.dta", replace
restore

* save profiled companies' contour
preserve
	keep if histgroup == 0
	drop histgroup
	save "$path/data/processed/profiledsirens_contour_${yr_start}_${yr_end}.dta", replace
restore



