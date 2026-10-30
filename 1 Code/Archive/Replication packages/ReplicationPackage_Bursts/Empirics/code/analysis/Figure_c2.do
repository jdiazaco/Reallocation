

use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear

* gen x
gen revenue_per_nc = revenue_nc/n_c
gen lrevenue_per_nc = log(revenue_per_nc)

* conditioned on size

foreach N in 5 {
	
	preserve 
	
		keep if lagN == `N'
		
		lpoly lrevenue_per_nc ln_c if n_c <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products", size(medlarge))                    					///
					 ytitle("Avg. revenue per new product  (log)", size(medlarge))            				///
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0 "10{superscript:0}"  2.302585093 "10{superscript:1}"  4.605170186  "10{superscript:2}" 6.907755279  "10{superscript:3}" 9.210340372 "10{superscript:4}" , labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  ///
					 legend(off) title("") note("")
	
		graph export "$path/output/figure_c2a.pdf", replace
		
	restore 
	
}



* conditioned on size 

foreach N in 10 {
	
	preserve 
	
		keep if lagN == `N'
		
		lpoly lrevenue_per_nc ln_c if n_c <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products", size(medlarge))                    					///
					 ytitle("Avg. revenue per new product  (log)", size(medlarge))            				/// 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel(-9.2103404 "10{superscript:-4}" -4.6051702 "10{superscript:-2}" 0 "10{superscript:0}" 4.605170186 "10{superscript:2}" 9.210340372 "10{superscript:4}" 13.815511 "10{superscript:6}" 18.420681 "10{superscript:8}", labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  /// 
					 legend(off) title("") note("")
	
		graph export "$path/output/figure_c2b.pdf", replace
		
	
	restore 
	
}
