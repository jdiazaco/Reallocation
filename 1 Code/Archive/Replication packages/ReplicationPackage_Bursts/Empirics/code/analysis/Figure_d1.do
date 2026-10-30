
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



*** Produce output 

** produce with more bins 
forval i = 1/1 {
	capture drop plotbin_`i'  
	gen plotbin_`i' = . 
	qui: replace plotbin_`i' = 1 if lagN_`i' == 1
	qui: replace plotbin_`i' = 2 if lagN_`i' == 2
	qui: replace plotbin_`i' = 3 if lagN_`i' == 3
	qui: replace plotbin_`i' = 4 if lagN_`i' == 4 | lagN_`i' == 5 
	qui: replace plotbin_`i' = 5 if lagN_`i' == 6 | lagN_`i' == 7 | lagN_`i' == 8
	qui: replace plotbin_`i' = 6 if lagN_`i' > 8 & lagN_`i' <= 12 
	qui: replace plotbin_`i' = 7 if lagN_`i' > 12 & lagN_`i' <= 20 
	qui: replace plotbin_`i' = 8 if lagN_`i' > 20 & lagN_`i' <= 40 
	qui: replace plotbin_`i' = 9 if lagN_`i' > 40 
} 
			
* repeat for DADS 
bysort plotbin_1: egen dads_1_rate_fit_mean_plotbin = mean(dads_1_rate_fit)
bysort plotbin_1: egen dads_1_rate_fit_med_plotbin = median(dads_1_rate_fit)
bysort plotbin_1: egen dads_1_rate_fit_p10_plotbin = pctile(dads_1_rate_fit) , p(90) autotype 
bysort plotbin_1: egen dads_1_rate_fit_p90_plotbin = pctile(dads_1_rate_fit) , p(10) autotype 
bysort plotbin_1: egen dads_1_rate_mean_plotbin = mean(dads_1_rate)
bysort plotbin_1: egen dads_1_rate_med_plotbin = median(dads_1_rate)
bysort plotbin_1: egen dads_1_rate_p10_plotbin = pctile(dads_1_rate) , p(90) autotype 
bysort plotbin_1: egen dads_1_rate_p90_plotbin = pctile(dads_1_rate) , p(10) autotype 
		
		
preserve 
	duplicates drop plotbin_1, force 
	sort plotbin_1
	keep rd* plotbin_1 n_c* dads*	 
	
	* demeaned 
	graph twoway scatter  dads_1_rate_fit_mean_plotbin plotbin_1 if plotbin_1 <= 256 ,  /// 
						 xtitle("Number of products", size(medlarge))                    					///
						 ytitle("R&D per product (millions, demeaned)", size(medlarge))            				/// 
						 xlabel( 1 "1" 2 "2" 3 "3"	4 "4-5" 5 "6-8" 6 "9-11" 7 "12-19" 8 "20-40" 9 "40+"   ,  labsize(medium)) ///
						 ylabel( -0.5(0.5)0.5, labsize(medium)) lcolor(black) mcolor(blue) msize(vlarge)   ///  
						 legend(off) title("") note("")  || ///
			 rcap  dads_1_rate_fit_p10_plotbin dads_1_rate_fit_p90_plotbin plotbin_1  ,  lcolor(blue%40)   
						 
				graph export "$path/output/figure_d1b.pdf", replace		
			
	* raw data  
	graph twoway scatter  dads_1_rate_mean_plotbin plotbin_1 if plotbin_1 <= 256 ,  /// 
						 xtitle("Number of products", size(medlarge))                    					///
						 ytitle("R&D  per product (millions)", size(medlarge))            				///  
						 xlabel( 1 "1" 2 "2" 3 "3"	4 "4-5" 5 "6-8" 6 "9-11" 7 "12-19" 8 "20-40" 9 "40+"   ,  labsize(medium)) ///
						 ylabel( 0(0.5)1, labsize(medium)) lcolor(black) mcolor(blue) msize(vlarge)   /// 
						 legend(off) title("") note("")  || ///
			 rcap  dads_1_rate_p10_plotbin dads_1_rate_p90_plotbin plotbin_1  ,  lcolor(blue%40)   
						 
				graph export "$path/output/figure_d1a.pdf", replace		
				
													
restore 
	
