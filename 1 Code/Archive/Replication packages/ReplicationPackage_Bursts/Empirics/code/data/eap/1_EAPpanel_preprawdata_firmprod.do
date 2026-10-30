/*	============================================================
	This do file clean the raw EAP files and creates a panel dataset
	
	This do file performs the following tasks:
		1. Import & clean EAP file for each year
		2. Append all years
		3. Drop duplicates
		5. Gen var model of production (2 to 5)
		6. Gen var revenue & quantity
		7. Consolidate historical groups
		8. Concord product over time
		
	============================================================	*/
	



* ============================================================================= *


foreach pc in correspc10 prodfra10 pc8 pc6 pc4 {

		* Import & clean EAP files
		**************************

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
		order year siren product
		sort year siren product
		tempfile temp_eap_`year'
		save `temp_eap_`year''
		
	}


		* Append years
		*****************
		
	clear
	forvalues year = $yr_start/$yr_end {
			
			append using `temp_eap_`year''
			erase `temp_eap_`year''
	}
	
	
	
		* Drop duplicates
		*******************
	
	duplicates tag year siren product model value, gen(dupl)
	count if dupl > 1
	display "`r(N)' duplicates"
	duplicates drop year siren product model value, force // 3 obs deleted
	bys year siren product model: gen n = _N
	assert n== 1
	drop n dupl

	
	
		* Gen var quantity vs value
		***************************
		
	* gen var quantity in invoice: model 2 + 3 + 4 + 5
	gen type = ""
	foreach var in VQ1B VQ1C VQ1D VQ1E {
			replace type = "Q" if model == "`var'"
	}
	* gen var revenue in invoice: model 2 + 3 + 4 + 5
	foreach var in VS2 VF1 VF2 VF3 {
			replace type = "F" if model == "`var'"
	}
	* drop remaining models
	drop if type == ""

		
		
		* Gen var model of production
		*****************************

	gen model_c = ""
	replace model_c = "2" if model == "VS2" | model == "VQ1B"     
	replace model_c = "3" if model == "VF1" | model == "VQ1C"
	replace model_c = "4" if model == "VF2" | model == "VQ1D"
	replace model_c = "5" if model == "VF3" | model == "VQ1E"
	drop if model_c == ""
	drop model


	
	
		* Gen revenue and quantity
		************************

	* Reshape panel
	drop quality_check quality_info unit
	reshape wide value, i(year siren product type) j(model_c) string
	reshape wide value*, i(year siren product) j(type) string

	egen revenue = rowtotal(value2F value3F value4F value5F), missing
	egen quantity = rowtotal(value2Q value3Q value4Q value5Q), missing

	
	
	

		* Consolidation profiles companies
		**********************************
		

			
			* Consolidation historical groups
			**********************************

			//  ACCOR is not available for the whole period since it is not a manufacturing firm
			
			* Drop profiled historical groups post 2012
			drop if siren == "P12000020"
			drop if siren == "P12000019" 
			drop if siren == "P12000030"
			drop if siren == "P12000022" 
			drop if siren == "P12000021"
			
			* Drop profiled historical groups before 2012
			drop if siren == "044001113"
			drop if siren == "044001121"
			drop if siren == "044001014"
			drop if siren == "044000917"
			drop if siren == "044000925"

			* Drop legal units of historical groups
			merge m:1 siren using "$path/data/processed/histgroups_contour_${yr_start}_${yr_end}.dta", keep(1 3)
			count if _m == 3
			di "drop `r(N)' legal units of historical groups"
			drop if _m == 3
			drop _m sirus_id
			
			
			
			* Consolidation profiled companies
			**********************************

			* There are 11 profiled companies in the EAP, starting showing from the year 2012:
			* P12000001; P12000002; P12000003; P12000004; P12000032; P12000033; 
			* P12000034; P12000035; P13000013; P13000017; P13000071
			
			* Drop profiled companies bc is not possible to fully reconstruct their groups
			count if substr(siren,1,1)=="P"
			di "drop `r(N)' profiled companies"
			drop if substr(siren,1,1)=="P"
			assert substr(siren,1,1)!="P"
			
			* Drop legal units belonging to these 11 profiled companies
			merge m:1 siren using "$path/data/processed/profiledsirens_contour_${yr_start}_${yr_end}.dta", keep(1 3) // all profiled companies are merged!
			count if _m == 3
			di "drop `r(N)' legal units of profiled companies"
			drop if _m == 3
			drop _m sirus_id
				
		


			
	
		* Concord products over time
		****************************

	*** Prodfra 10 digit - Only product categories not changing over time
	if "`pc'" == "prodfra10" {
		
		* Drop those products without direct correspondance
		gen d9 = substr(product,-2,1)
		tab d9
		drop if d9 == "H" | d9 == "N" | d9 == "S" | d9 == "Y"
		drop d9
				
		* Keep only products not changing over time
		// Using concordance table for PRODFRA from INSEE
		preserve
			clear 
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2009_2017", case(lower) firstrow 
			keep annee_n c_prodeap_n
			save temp1
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2018_2020", case(lower) firstrow 
			keep annee_n c_prodeap_n
			append using temp1
			erase temp1.dta
			destring annee_n, replace
			drop if annee_n > $yr_end
			drop if annee_n < $yr_start
			duplicates drop annee_n c_prodeap_n, force
			bys c_prodeap_n: gen n = _n
			tab n
			keep if n == ($yr_end - $yr_start + 1)
			drop annee_n n
			rename c_prodeap_n product
			tempfile listprod10
			save `listprod10'			
		restore 
		merge m:1 product using `listprod10'
		tab _m
		count if _m == 1
		di "`r(N)' obs dropped due to changes in code over time accounting for 10% of total revenue"
		tab year _m
		keep if _m == 3
		drop _m
		label var product "PRODFRA10 not concorded"
	
	}
	
	
	*** Collapse to PC8
	if "`pc'" == "pc8" {
		
		* Drop those products without direct correspondance
		gen d9 = substr(product,-2,1)
		drop if d9 == "H" | d9 == "N" | d9 == "S" | d9 == "Y"
		drop d9
				
		* Keep only products not changing over time
		// Using concordance table for PRODFRA from INSEE
		preserve
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2009_2017", case(lower) firstrow 
			keep annee_n c_prodeap_n
			cap erase temp1.dta
			save temp1
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2018_2020", case(lower) firstrow 
			keep annee_n c_prodeap_n
			append using temp1
			erase temp1.dta
			destring annee_n, replace
			drop if annee_n > $yr_end
			drop if annee_n < $yr_start
			gen pc8 = substr(c_prodeap_n,1,8)
			drop c_prodeap_n
			duplicates drop annee_n pc8, force
			bys pc8: gen n = _n
			tab n
			keep if n == ($yr_end - $yr_start + 1)
			drop annee_n n
			tempfile listprod8d
			save `listprod8d'			
		restore 
				
		* Gen PRODCOM 8 digit
		gen pc8 = substr(product,1,8)
		collapse (sum) value* revenue quantity, by(year siren pc8)
				
		* Merge list 8-digit product codes not changing over time
		merge m:1 pc8 using `listprod8d'
		tab _m
		count if _m == 1
		di "`r(N)' obs dropped due to changes in product code over time" //  accounting for 7% of total revenue
		tab year _m
		keep if _m == 3
		drop _m
	
		rename pc8 product
		label var product "PC8 not concorded"
		
	}
	
	*** Collapse to PC6
	if "`pc'" == "pc6" {
		
		* Drop those products without direct correspondance
		gen d9 = substr(product,-2,1)
		drop if d9 == "H" | d9 == "N" | d9 == "S" | d9 == "Y"
		drop d9
				
		* Keep only products not changing over time
		// Using concordance table for PRODFRA from INSEE
		preserve
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2009_2017", case(lower) firstrow 
			keep annee_n c_prodeap_n
			save temp1
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2018_2020", case(lower) firstrow 
			keep annee_n c_prodeap_n
			append using temp1
			erase temp1.dta
			destring annee_n, replace
			drop if annee_n > $yr_end
			drop if annee_n < $yr_start
			gen pc6 = substr(c_prodeap_n,1,6)
			drop c_prodeap_n
			duplicates drop annee_n pc6, force
			bys pc6: gen n = _n
			tab n
			keep if n == ($yr_end - $yr_start + 1)
			drop annee_n n
			tempfile listprod6d
			save `listprod6d'			
		restore 
				
		* Gen PRODCOM 6 digit
		gen pc6 = substr(product,1,6)
		
		collapse (sum) value* revenue quantity, by(year siren pc6)
				
		* Merge list 6-digit product codes not changing over time
		merge m:1 pc6 using `listprod6d'
		tab _m
		count if _m == 1
		di "`r(N)' obs dropped due to changes in product code over time"
		tab year _m
		keep if _m == 3
		drop _m
	
		rename pc6 product
		label var product "PC6 not concorded"
		
	}
	
	*** Collapse to PC4
	if "`pc'" == "pc4" {
		
		* Drop those products without direct correspondance
		gen d9 = substr(product,-2,1)
		drop if d9 == "H" | d9 == "N" | d9 == "S" | d9 == "Y"
		drop d9
				
		* Keep only products not changing over time
		// Using concordance table for PRODFRA from INSEE
		preserve
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2009_2017", case(lower) firstrow 
			keep annee_n c_prodeap_n
			save temp1
			clear
			import excel "$path/data/external/INSEE/correspondance tables INSEE/ProdEap_correspondance_table_2018_2020", case(lower) firstrow 
			keep annee_n c_prodeap_n
			append using temp1
			erase temp1.dta
			destring annee_n, replace
			drop if annee_n > $yr_end
			drop if annee_n < $yr_start
			gen pc4 = substr(c_prodeap_n,1,4)
			drop c_prodeap_n
			duplicates drop annee_n pc4, force
			bys pc4: gen n = _n
			tab n
			keep if n == ($yr_end - $yr_start + 1)
			drop annee_n n
			tempfile listprod4d
			save `listprod4d'			
		restore 
				
		* Gen PRODCOM 4 digit
		gen pc4 = substr(product,1,4)		
		collapse (sum) value* revenue quantity, by(year siren pc4)
				
		* Merge list 4-digit product codes not changing over time
		merge m:1 pc4 using `listprod4d'
		tab _m
		count if _m == 1
		di "`r(N)' obs dropped due to changes in product code over time"
		tab year _m
		keep if _m == 3
		drop _m
	
		rename pc4 product
		label var product "PC4 not concorded"
		
	}	
		
	
	*** Concord prod over time by grouping 1 to many, many to 1 and many to many cases
	if "`pc'" == "correspc10" {
	
		* Drop those products indicating livraison (Y), weird products (H) and old products (N)
		gen d9 = substr(product,-2,1)
		drop if d9 == "H" | d9 == "Y" | d9 == "N" | d9 == "S" 
		drop d9
		
		* Gen PRODFRA 10 digit concorded over time
		rename product pc10
		merge m:1 year pc10 using "$path/data/processed/corres_pc10_${yr_start}${yr_end}.dta", keep(1 3)
		*assert _m == 3
		count if _m == 1
		drop if _m == 1
		drop _m		
		collapse (sum) value* revenue quantity, by(year siren id_conc) fast
		
		rename id_conc product
		label var product "PRODFRA10 concorded"
		quietly tab product
		di "`r(r)' no of products concorded"

	}
	
	
	
		* Drop duplicates
		*****************
	// due to missing unit of measure
	* identify duplicates
	duplicates tag year siren product, gen(dupl)
	count if dupl == 1
	display "`r(N)' duplicates"

	
	
	
	
		* Save panel dta
		******************

	* label vars
	label var year "year"
	label var siren "firm id"
	label var revenue "revenue (th Euro)"
	label var quantity "quantity"
	label var value2F "revenue model 2"
	label var value3F "revenue model 3"
	label var value4F "revenue model 4"
	label var value5F "revenue model 5"
	label var value2Q "quantity model 2"
	label var value3Q "quantity model 3"
	label var value4Q "quantity model 4"
	label var value5Q "quantity model 5"

	order year siren product
	sort siren product (year)

	compress
	save "$path/data/processed/EAP`pc'_${yr_start}${yr_end}_firmprod.dta", replace
	

}



