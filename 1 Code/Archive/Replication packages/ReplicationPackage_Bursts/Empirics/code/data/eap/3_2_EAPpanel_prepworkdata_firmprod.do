/*	============================================================
	This do file cleans the prepped EAP panel data at firm-product level	
	============================================================	*/


foreach pc in correspc10 prodfra10 pc8 pc6 pc4 {

	* use raw data
	use "$path/data/processed/EAP`pc'_20102019_OnlyEAP1_firmprod.dta", clear

	* deflate
	gen deflator = .
	replace deflator = 95.52 if year == 2010
	replace deflator = 96.43 if year == 2011
	replace deflator = 97.54 if year == 2012
	replace deflator = 98.31 if year == 2013
	replace deflator = 98.87 if year == 2014
	replace deflator = 100   if year == 2015
	replace deflator = 100.52 if year == 2016
	replace deflator = 101.05 if year == 2017
	replace deflator = 102.04 if year == 2018
	replace deflator = 103.36 if year == 2019

	replace catotal = (catotal/deflator)*100
	replace revenue = (revenue/deflator)*100

	* drop if time-firm-product value is zero
	drop if revenue == 0


	*** clean firms' re-entry in EAP
	preserve
		gcollapse (nansum) revenue (first) birthyear birthyearecon exityearecon, by(year siren) fast
		egen id = group(siren)
		xtset id year
		bys id (year): gen gap = 1 if l1.revenue == . & revenue != .
		bys id (year): gen period = sum(gap)
		bys id (year): egen reentry = max(period)
		gen reentry_tag = (reentry > 1)
		keep if reentry_tag == 1
		keep year siren
		tempfile clean_firmreentry
		save `clean_firmreentry'
	restore

	merge m:1 year siren using `clean_firmreentry'
	drop if _m == 3
	drop _m
	

	* gen firm's entry in EAP
	bys siren: egen entrytemp = min(year)

	* gen firms' exit in EAP
	bys siren: egen exittemp = max(year)
	
	*** clean firms' entry
	gen fakeentry = (year < birthyear & birthyear !=.)
	assert fakeentry == 0
	drop fakeentry
	
	*** manually drop outlier
	drop if siren == "682031224"
	
	
	* gen id
	egen id = group(siren product)

	* create a full matrix for identifying firm's entry/exit
	xtset id year
	tsfill, full

	* fill observations
	bysort id: carryforward siren, gen(firm2)
	gsort id - year
	bysort id: carryforward firm2, gen(firmfinal)
	drop siren firm2
	rename firmfinal siren
	sort id (year)
	bysort id: carryforward product, gen(prod2)
	gsort id - year
	bysort id: carryforward prod2, gen(prodfinal)
	drop product prod2
	rename prodfinal product
	sort id (year)
	order id year siren product

	* gen firm's number of products
	gen index = 0
	replace index = 1 if revenue != .
	bys siren year: egen N = total(index)

	* gen firm's number of new products
	bys id (year): gen index_new = 1 if index == 1 & l1.index == 0
	replace index_new = . if index_new == 1 & entrytemp == year
	bys siren year: egen n_c = total(index_new)

	* gen firm's number of lost products
	bys id (year): gen index_des = 1 if l1.index == 1 & index == 0
	bys siren year: egen n_d = total(index_des)
	
	* gen firm's number of continuing products
	bys id (year): gen index_cont = 1 if index_new == . & index_des == . & revenue != .
	replace index_cont = . if index_cont == 1 & entrytemp == year	
	bys siren year: egen n_cont = total(index_cont)

	* gen revenue by new products
	gen temp_revenue_nc = revenue if index_new == 1
	bys year siren: egen revenue_nc = total(temp_revenue_nc)

	* gen revenue in year t-1 by lost products
	xtset id year
	gen temp_revenue_nd = l1.revenue if index_des == 1
	bys year siren: egen revenue_nd = total(temp_revenue_nd)

	* gen revenue continuing products
	gen temp_revenue_cont = revenue if index_cont == 1
	bys year siren: egen revenue_cont = total(temp_revenue_cont)

	* drop useless vars
	drop index temp_revenue_nc temp_revenue_nd temp_revenue_cont
	

	// create firm-level vars
	
	preserve
	
		* collapse at year-firm dimension
		gcollapse (nansum) revenue (first) N n_c n_d n_cont revenue_nc revenue_nd revenue_cont (max) entrytemp exittemp ///
				 (max) employees_fte catotal nace4_rev2 birthyear birthyearecon exityearecon immoinc, by(year siren) fast
		
		* gen id
		egen id = group(siren)
		order id
		xtset id year		 

		* set vars to missing when firm not active 
		replace revenue  = . if revenue == 0
		drop if revenue == . & n_d == 0
		foreach var in N n_c n_cont revenue_nc revenue_cont {
			replace `var' = . if revenue == .
		}
		
		* set new and lost products variables to missing for initial year or entry year
		foreach var in n_c n_d n_cont revenue_nc revenue_nd revenue_cont {
			replace `var' = . if year == $yr_start
			replace `var' = . if year == entrytemp & `var' == 0
		}
		drop entrytemp exittemp
		
		* gen lags and logs
		foreach var in n_c n_d N {
			gen lag`var'  = l1.`var'   if l1.`var' != .
			gen l`var'    = log(`var') if `var'!=.
			gen lagl`var' = l1.l`var'  if l1.l`var' !=.
		}

		* gen age				
		gen age = year - birthyear
		bys id: egen age_min = min(age)
		
		* clean vars to have same number of obs
		gen exit = 0
		replace exit = 1 if revenue == .
		drop if (catotal <=1 		| catotal == .) 		&  exit != 1
		drop if (employees_fte <=1 	| employees_fte == .) 	&  exit != 1
		drop if (age <0 			| age == .) 		    &  exit != 1
		drop if nace4_rev2        == . &  exit != 1
		drop exit

		* label vars
		label var N   "number of products"
		label var n_c "number of new products"
		label var n_d "number of lost products"
		label var revenue_nc "revenue of new products"
		label var revenue_nd "revenue of lost products"
		label var revenue_cont "revenue of continuing products"
		label var employees_fte "number of employees (FTE)"    
		label var catotal "turnover"
		label var nace4_rev2 "NACE rev.2 4-digit"
		label var birthyear "legal birth year"
		label var birthyearecon "economic birth year"
		label var exityearecon "economic exit year"
		
		drop id
		
		tempfile temp
		save `temp'
		
	restore
	
	* drop firm level vars
	drop N n_c n_d n_cont revenue_nc revenue_nd revenue_cont entrytemp exittemp /// 
		employees_fte catotal nace4_rev2 birthyear birthyearecon exityearecon immoinc
	
	* drop useless vars
	drop dupl employees_hc deflator
	
	merge m:1 year siren using `temp', keep(3) nogen

	* save data for summary stats
	save "$path/data/processed/EAP`pc'_20102019_OnlyEAP1_firmprod_clean_summstats.dta", replace
	
	* drop when revenue is missing
	drop if revenue == .
	
	* save data at firm-product level
	compress
	save "$path/data/processed/EAP`pc'_20102019_OnlyEAP1_firmprod_clean.dta", replace 


}
