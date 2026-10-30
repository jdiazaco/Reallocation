
*** Produce working data

use"$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear


*** calculate innovation rates by size bins

* gen innovation rates
gen n_c_rate  = n_c/lagN
gen n_d_rate  = n_d/lagN


* condition on lag N 
qui: gen tempbin =. 
qui: replace tempbin = 1 if lagN == 1
qui: replace tempbin = 2 if lagN == 2
qui: replace tempbin = 3 if lagN == 3
qui: replace tempbin = 4 if lagN == 4 | lagN == 5 
qui: replace tempbin = 5 if lagN == 6 | lagN == 7 | lagN == 8
qui: replace tempbin = 6 if lagN > 8

matrix drop _all

foreach var of varlist n_c_rate n_d_rate {
	
	summarize `var' 
	matrix `var'_uw = nullmat(`var'_uw), r(mean)
	
	summarize `var' [aweight=lagN] 
	matrix `var'_uw = `var'_uw, r(mean)

	forvalues i=1(1)6 {
		summarize `var' if tempbin == `i'
		matrix `var'_uw = `var'_uw, r(mean)

	}
}

foreach mat_type in uw {
	matrix table2_size_`mat_type' = n_c_rate_`mat_type' \ n_d_rate_`mat_type'
	matrix colnames table2_size_`mat_type' = "All" "Weighted" "1" "2" "3" "4-5" "6-8" "8+"
	matrix rownames table2_size_`mat_type' = "Product creation rate" "Product destruction rate"
	
	esttab matrix(table2_size_`mat_type', fmt(3)) using "$path/output/table_2_top", replace booktabs label nomtitles unstack plain ///
		prehead("\begin{tabular}{l*{7}{r}} \toprule \toprule") posthead("\midrule") prefoot("\bottomrule") /// 
		substitute("Product destruction" "\addlinespace Product destruction")

}


*** calculate innovation rates by age bins

capture drop agebin
gen byte agebin = (age <= 5)
replace agebin = 2  if (age >  5 & age <=10)
replace agebin = 3  if (age > 10 & age <=15)
replace agebin = 4  if (age > 15 & age <=20)
replace agebin = 5  if (age > 20 & age <=25)
replace agebin = 6  if (age > 25 & age <=50)
replace agebin = 7 if (age > 50 )
drop if agebin == 0 

matrix drop _all

foreach var of varlist n_c_rate n_d_rate {
	summarize `var' 
	matrix `var'_uw = nullmat(`var'_uw), r(mean)
	
	forvalues i=1(1)7 {
		summarize `var' if l.agebin == `i'
		matrix `var'_uw = `var'_uw, r(mean)
		
	}
}

foreach mat_type in uw {
	
	matrix table2_age_`mat_type' = n_c_rate_`mat_type' \ n_d_rate_`mat_type'
	matrix colnames table2_age_`mat_type' = "All" "0-5" "5-10" "10-15" "15-20" "20-25" "25-50" "50+"
	matrix rownames table2_age_`mat_type' = "Product creation rate" "Product destruction rate"
	
	esttab matrix(table2_age_`mat_type', fmt(3)) using "$path/output/table_2_bottom", replace booktabs label nomtitles unstack plain ///
		prehead("\begin{tabular}{l*{8}{r}} \toprule \toprule") posthead("\midrule") prefoot("\bottomrule") /// 
		substitute("Product destruction" "\addlinespace Product destruction")
		
}



