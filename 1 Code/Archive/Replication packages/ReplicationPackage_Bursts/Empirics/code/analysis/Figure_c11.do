

** 1: obtain product-level data and identify new products

* open data  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear

* calculate number of products in a good's burst 
keep if index_new ==1
rename n_c n_c_uponentry 
rename lagN lagN_uponentry 
keep id year n_c_uponentry index_new lagN_uponentry 

* save data 
save "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_ncinfirstyear" , replace 


** 2: find probability of exit for all of the products 

* open data  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear

* merge with data on bursts 
merge 1:1 year id using "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_ncinfirstyear"  
 
* apply n_c_uponentry across years
sort id year 
bysort id: carryforward n_c_uponentry, replace 
bysort id: carryforward lagN_uponentry, replace 
 
* calculate product tenure 
xtset id year 
gen tenure = 0 if index_new == 1
replace tenure = 1 if l1.index_new == 1 & index_new == .  
replace tenure = 2 if l2.index_new == 1 & index_new == .  
replace tenure = 3 if l3.index_new == 1 & index_new == .  
replace tenure = 4 if l4.index_new == 1 & index_new == .  
replace tenure = 5 if l5.index_new == 1 & index_new == .  
replace tenure = 6 if l6.index_new == 1 & index_new == .  
replace tenure = 7 if l7.index_new == 1 & index_new == .  

* calculate probability of exit
bysort id: egen exityear = max(year) 
bysort siren: egen exityear_firm = max(year)  
gen exit = year == exityear 
replace exit = . if exityear == 2019 
replace exit = 0 if (exityear == exityear_firm & exityear_firm != exityearecon)  



** plot the local polynomial regression 

* calculate probability of exit 
gen log_n_c_uponentry = log(n_c_uponentry) 

lpoly exit  log_n_c_uponentry if tenure == 0 & n_c_uponentry <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products in initial burst", size(medlarge))                    					///
					 ytitle("Annual Probability that Product Exits Within 1 Year", size(medlarge))            				/// 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0(0.1).4, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  /// 
					 legend(off) title("") note("")
graph export "$path/output/figure_c11a.pdf", replace

lpoly exit  log_n_c_uponentry if tenure < 5  & n_c_uponentry <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products in initial burst", size(medlarge))                    					///
					 ytitle("Annual Probability that Product Exits Within 5 Years", size(medlarge))            				/// 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0(0.1).4, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  ///
					 legend(off) title("") note("")
graph export "$path/output/figure_c11c.pdf", replace

lpoly exit  log_n_c_uponentry if tenure < 8  & n_c_uponentry <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products in initial burst", size(medlarge))                    					///
					 ytitle("Annual Probability that Product Exits Within 7 Years", size(medlarge))            				/// 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0(0.1).4, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  ///  
					 legend(off) title("") note("")
graph export "$path/output/figure_c11e.pdf", replace

 

** repeat for firms with 1 initial prodcut 	

lpoly exit  log_n_c_uponentry if lagN_uponentry == 1 & tenure == 0 & n_c_uponentry <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products in initial burst", size(medlarge))                    					///
					 ytitle("Annual Probability that Product Exits Within 1 Year", size(medlarge))            				/// 
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0(0.1).4, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  ///
					 legend(off) title("") note("")
graph export "$path/output/figure_c11b.pdf", replace

lpoly exit  log_n_c_uponentry if lagN_uponentry == 1 & tenure < 5 & n_c_uponentry <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products in initial burst", size(medlarge))                    					///
					 ytitle("Annual Probability that Product Exits Within 5 Years", size(medlarge))            				///  
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0(0.1).4, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  ///  
					 legend(off) title("") note("")
graph export "$path/output/figure_c11d.pdf", replace

lpoly exit  log_n_c_uponentry if lagN_uponentry == 1 & tenure < 8 & n_c_uponentry <= 32 , noscatter ci level(95) degree(5)  bwidth(1)  /// 
					 xtitle("Number of new products in initial burst", size(medlarge))                    					///
					 ytitle("Annual Probability that Product Exits Within 7 Years", size(medlarge))            				///  
					 xlabel( 0 "1" .6931472 "2" 1.386294 "4"	2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medium)) ///
					 ylabel( 0(0.1).4, labsize(medium)) lineopts(lcolor(black)) ciopts(lcolor(blue) fcolor(blue))  /// 
					 legend(off) title("") note("")
graph export "$path/output/figure_c11f.pdf", replace







