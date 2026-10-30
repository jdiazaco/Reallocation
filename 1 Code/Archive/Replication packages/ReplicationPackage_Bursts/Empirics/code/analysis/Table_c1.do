

*** produce dataset 
foreach laghorizon in 1 {
	foreach pc in  correspc10  { 

		* use raw data
		use "$path/data/processed/EAP`pc'_20102019_OnlyEAP1_firmprod.dta", clear

		* deflate (redundant)
		gen deflator =100
		replace catotal = (catotal/deflator)*100
		replace revenue = (revenue/deflator)*100

		* drop if time-firm-product value is zero
		drop if revenue == 0

		*** clean firms' re-entry in EAP
		preserve
			collapse (sum) revenue (first) birthyear birthyearecon exityear, by(year siren) fast
			egen id = group(siren)
			xtset id year
			bys id (year): gen gap = 1 if l.revenue == . & revenue != .
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
		bys id (year): gen index_new = 1 if index == 1 & l`laghorizon'.index == 0
		replace index_new = . if index_new == 1 & entrytemp == year
		bys siren year: egen n_c = total(index_new)

		* gen firm's number of lost products
		bys id (year): gen index_des = 1 if l`laghorizon'.index == 1 & index == 0
		bys siren year: egen n_d = total(index_des)

		* gen revenue by new products
		gen temp_revenue_nc = revenue if index_new == 1
		bys year siren: egen revenue_nc = total(temp_revenue_nc)

		* gen revenue in year t-1 by lost products
		xtset id year
		gen temp_revenue_nd = l`laghorizon'.revenue if index_des == 1
		bys year siren: egen revenue_nd = total(temp_revenue_nd)

		* gen revenue continuing products
		bys id (year): gen index_cont = 1 if index_new == . & index_des == . & revenue != .
		replace index_cont = . if index_cont == 1 & entrytemp == year
		gen temp_revenue_cont = revenue if index_cont == 1
		bys year siren: egen revenue_cont = total(temp_revenue_cont)

		* drop useless vars
		drop index index_new index_des temp_revenue_nc temp_revenue_nd index_cont temp_revenue_cont	
		
		* collapse at year-firm dimension
		collapse (sum) revenue (first)  N n_c n_d revenue_nc revenue_nd revenue_cont (max) entrytemp exittemp ///
				 (max) employees_fte catotal nace4_rev2 birthyear birthyearecon exityear immoinc , by(year siren) fast

		* gen id
		egen id = group(siren)
		order id
		xtset id year		 

		* set vars to missing when firm not active 
		replace revenue  = . if revenue == 0
		foreach var in N n_c revenue_nc revenue_cont {
			replace `var' = . if revenue == .
		}

		* set new and lost products variables to missing for initial year or entry year
		foreach var in n_c n_d revenue_nc revenue_nd revenue_cont {
			replace `var' = . if year == $yr_start
			replace `var' = . if year == entrytemp & `var' == 0
		}
		drop entrytemp exittemp

		* gen lags and logs
		foreach var in n_c n_d N {
			gen lag`var'  = l`laghorizon'.`var'   if l`laghorizon'.`var' != .
			gen l`var'    = log(`var') if `var'!=.
			gen lagl`var' = l`laghorizon'.l`var'  if l`laghorizon'.l`var' !=.
		}

		* gen age				
		gen age = year - birthyear
		bys id: egen age_min = min(age)

		* clean vars to have same number of obs
		gen exit = 0
		replace exit = 1 if revenue == .

		* replace with previous obs when missing
		bys id (year): replace nace4_rev2 = nace4_rev2[_n-1] if nace4_rev2 == . & n_d != .

		* save
		compress
		save "$path/data/processed/temp_churn", replace


	}
} 




*** produce regression table 

** open data 
use "$path/data/processed/temp_churn", replace
rename lagN  lagN_1
rename n_c   n_c_1 
rename n_d   n_d_1 

* calculate innovation rates
forval i = 1/1 /*5*/ {
	gen n_c_`i'_rate  = n_c_`i'/lagN_`i'   /`i'
	gen n_d_`i'_rate  = n_d_`i'/lagN_`i'  // `i'
}

* run ppml regression 
ppmlhdfe n_c_1 laglN , cluster(siren year) 
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", replace tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe n_c_1 laglN if lagN> 1, cluster(siren year) 
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')	
ppmlhdfe n_c_1 laglN, absorb(nace4_rev2#year) cluster(siren year) 
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe n_c_1 laglN if lagN> 1, absorb(nace4_rev2#year) cluster(siren)  
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')		

* run ppml regression - product count weighted 
ppmlhdfe n_c_1 laglN [pweight=lagN], cluster(siren year) 
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe n_c_1 laglN if lagN> 1, cluster(siren year) 
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')	
ppmlhdfe n_c_1 laglN [pweight=lagN], absorb(nace4_rev2#year) cluster(siren year) 
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')
ppmlhdfe n_c_1 laglN [pweight=lagN] if lagN> 1, absorb(nace4_rev2#year) cluster(siren)  
test _b[laglN]=1 
local ttest1 = `r(p)'
outreg2 using "$path/output/table_c1", append tex nocon noobs noaster addstat(p-val = 1,`ttest1')		

erase "$path/output/table_c1.txt"
