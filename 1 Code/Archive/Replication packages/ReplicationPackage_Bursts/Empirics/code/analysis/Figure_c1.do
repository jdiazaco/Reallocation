
*** produce plots 

* use raw data
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear
rename lagN  lagN_1
rename n_c   n_c_1 
rename n_d   n_d_1 

* calculate innovation rates
forval i = 1/1 {
	gen n_c_`i'_rate  = n_c_`i'/lagN_`i'   /`i'
	gen n_d_`i'_rate  = n_d_`i'/lagN_`i'  // `i'
}

* demeaning by industry 
foreach var in  n_c_1_rate n_d_1_rate laglN {
	qui: reghdfe `var' , absorb(nace4_rev2#year) resid 
	predict `var'_fit, resid 
}


				

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

//simple 
bysort plotbin_1: egen n_c_1_rate_mean_plotbin = mean(n_c_1_rate)
bysort plotbin_1: egen n_c_1_rate_p10_plotbin = pctile(n_c_1_rate) , p(90) autotype 
bysort plotbin_1: egen n_c_1_rate_p90_plotbin = pctile(n_c_1_rate) , p(10) autotype 
bysort plotbin_1: egen n_d_1_rate_mean_plotbin = mean(n_d_1_rate)
bysort plotbin_1: egen n_d_1_rate_p10_plotbin = pctile(n_d_1_rate) , p(90) autotype 
bysort plotbin_1: egen n_d_1_rate_p90_plotbin = pctile(n_d_1_rate) , p(10) autotype 
//full 
bysort plotbin_1: egen n_c_1_rate_fit_mean_plotbin = mean(n_c_1_rate_fit)
bysort plotbin_1: egen n_c_1_rate_fit_sd_plotbin = sd(n_c_1_rate_fit)
bysort plotbin_1: egen n_c_1_rate_fit_med_plotbin = median(n_c_1_rate_fit)
bysort plotbin_1: egen n_c_1_rate_fit_p10_plotbin = pctile(n_c_1_rate_fit) , p(90) autotype 
bysort plotbin_1: egen n_c_1_rate_fit_p90_plotbin = pctile(n_c_1_rate_fit) , p(10) autotype 
bysort plotbin_1: egen n_d_1_rate_fit_mean_plotbin = mean(n_d_1_rate_fit)
bysort plotbin_1: egen n_d_1_rate_fit_sd_plotbin = sd(n_d_1_rate_fit)
bysort plotbin_1: egen n_d_1_rate_fit_med_plotbin = median(n_d_1_rate_fit)
bysort plotbin_1: egen n_d_1_rate_fit_p10_plotbin = pctile(n_d_1_rate_fit) , p(90) autotype 
bysort plotbin_1: egen n_d_1_rate_fit_p90_plotbin = pctile(n_d_1_rate_fit) , p(10) autotype 

	
* produce plot 
preserve 
	duplicates drop plotbin_1, force 
	sort plotbin_1
	keep n_c*  n_d* plotbin_1
	graph twoway scatter  n_c_1_rate_mean_plotbin plotbin_1 if plotbin_1 <= 256 ,  /// 
						 xtitle("Number of products", size(medlarge))                    					///
						 ytitle("Product creation rate", size(medlarge))            				/// from new products 
						 xlabel( 1 "1" 2 "2" 3 "3"	4 "4-5" 5 "6-8" 6 "9-11" 7 "12-19" 8 "20-40" 9 "40+"   ,  labsize(medium)) ///
						 ylabel( 0.0(0.1).6, labsize(medium)) lcolor(black) mcolor(blue) msize(vlarge)   /// ylabel(,labsize(small)) 
						 legend(off) title("") note("")  || ///
				 rcap  n_c_1_rate_p10_plotbin n_c_1_rate_p90_plotbin plotbin_1  , lcolor(blue%40)   
						 
				graph export "$path/output/figure_c1a.pdf", replace

	graph twoway scatter  n_c_1_rate_fit_mean_plotbin plotbin_1 if plotbin_1 <= 256 ,  /// 
						 xtitle("Number of products", size(medlarge))                    					///
						 ytitle("Product creation rate (demeaned)" , size(medlarge))            				/// from new products 
						 xlabel( 1 "1" 2 "2" 3 "3"	4 "4-5" 5 "6-8" 6 "9-11" 7 "12-19" 8 "20-40" 9 "40+"   ,  labsize(medium)) ///
						 ylabel( -0.3(0.1).3, labsize(medium)) lcolor(black) mcolor(blue) msize(vlarge)   /// ylabel(,labsize(small)) 
						 legend(off) title("") note("")  || ///
				 rcap  n_c_1_rate_fit_p10_plotbin n_c_1_rate_fit_p90_plotbin plotbin_1 ,   lcolor(blue%40)
				 
	graph export "$path/output/figure_c1b.pdf", replace
	
restore 
	
			
			




