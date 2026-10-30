
** open data  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean", replace

** create standard variable names 
rename lagN  lagN_1
rename n_c   n_c_1 
rename n_d   n_d_1 
forval i = 1/1 /*5*/ {
	gen n_c_`i'_rate  = n_c_`i'/lagN_`i'   /`i'
	gen n_d_`i'_rate  = n_d_`i'/lagN_`i'  // `i'
}

** merge in the R&D data 
merge 1:1 year siren using "$path/data/processed/gecir_rd_20102019.dta"
drop if _merge == 2 //drop firms outside of manufacturing 
replace rd_exp_value = 0 if _merge  == 1 //admin data 
drop _merge 
merge 1:1 year siren using "$path/data/processed/dads_rd_20102019.dta", keep(3) keepusing(s_brut_rd)

* calculate R&D rates 
replace rd_exp_value = rd_exp_value/1000000
replace s_brut_rd = s_brut_rd/1000000

* calculate R&D rates
forval i = 1/1 /*5*/ {
	gen rd_`i'_rate  = rd_exp_value / lagN_`i'
	gen dads_`i'_rate  = s_brut_rd / lagN_`i'
}

* demeaning by industry 
foreach var in  rd_1_rate dads_1_rate laglN {
	qui: reghdfe `var' , absorb(nace4_rev2#year) resid 
	predict residual, resid 
	gen  `var'_fit =  residual 
	qui: su `var'
	drop   residual 
}



** plot with local polynomial regression of new products against r&d intensity 

* create r&d spending variables 
xtset id year 
gen lag_s_brut_rd_perproduct = l.s_brut_rd/lagN
gen lag_s_brut_rd = l.s_brut_rd


* demeaning by industry 
foreach var in lag_s_brut_rd lag_s_brut_rd_perproduct {
	qui: reghdfe `var' , absorb(siren year) resid 
	predict residual, resid 
	gen  `var'_fit1 =  residual 
	qui: su `var'
	drop   residual 
}


* demeaning by industry and firm 
foreach var in  lag_s_brut_rd lag_s_brut_rd_perproduct  {
	qui: reghdfe `var' , absorb(nace4_rev2#year siren) resid 
	predict residual, resid 
	gen  `var'_fit2 =  residual 
	qui: su `var'
	drop   residual 
}

* any number pf products 
lpoly lag_s_brut_rd_perproduct_fit1 ln_c if n_c_1 <= 256 , noscatter ci level(95)  degree(5)  bwidth(1)   /// 
					 xtitle("Number of new products", size(medlarge))                    					///
					 ytitle("R&D Spending (millions, demeaned) per product", size(medlarge))            				/// from new products 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.158883083 "64" 4.852030264 "128", labsize(medium)) ///
					 ylabel( -3(3)3, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  /// ylabel(,labsize(small)) 
					 legend(off) title("") note("")
graph export "$path/output/figure_d2a.pdf", replace

lpoly lag_s_brut_rd_perproduct_fit2 ln_c if n_c_1 <= 256  , noscatter ci level(95)  degree(5)  bwidth(1)   /// 
					 xtitle("Number of new products", size(medlarge))                    					///
					 ytitle("R&D Spending (millions, demeaned) per product", size(medlarge))            				/// from new products 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.158883083 "64" 4.852030264 "128", labsize(medium)) ///
					 ylabel( -3(3)3, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  /// ylabel(,labsize(small)) 
					 legend(off) title("") note("")
graph export "$path/output/figure_d2b.pdf", replace









