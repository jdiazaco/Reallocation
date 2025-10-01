*** table 5
* mdr 
* 1-1-25 

/*
	This table produces output for table 5 where we look at regressions
	of markup estimates on other variables 
*/ 



*** prepare data 

* open the panel,  merge with estimates  
use "$pathdata/$dataset", clear 
merge m:1 reps using analysisdata/coefficients_byind_dgm, nogen 
foreach var in betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
	rename `var' `var'_dgm 
}
merge m:1 reps using analysisdata/coefficients_byind_acf
foreach var in betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
	rename `var' `var'_acf 
}

* calculate the elasticities 
gen elast_dgm = betav_tl1_dgm + betavk_tl_dgm*k + 2*betav_tl2_dgm*v 
gen elast_acf = betav_tl1_acf + betavk_tl_acf*k + 2*betav_tl2_acf*v 

* calculate the markup 
gen markup_dgm_log = log(elast_dgm*saleslevelfirm/(var_input*var_price))
gen markup_acf_log = log(elast_acf*saleslevelfirm/(var_input*var_price))
gen markup_true_log= log(mu_true)

* prepare the variables 
gen wagebill 	     = var_price *var_input
gen profitshare  	 = (saleslevelfirm - wagebill)/saleslevelfirm 
gen materialshare    = wagebill / saleslevelfirm
gen marketshare 	 = salessharefirm 

* normalize markups
foreach markup in markup_true_log markup_dgm_log markup_acf_log { 
    qui: su `markup'
	gen `markup'_std = `markup'/r(sd)
}



*** run regressions 

foreach spec in dgm acf true {
 	qui: su reps 
    matrix table_`spec'_p = J(`r(max)'+1, 3, 0)
    matrix table_`spec'_m = J(`r(max)'+1, 3, 0)
    matrix table_`spec'_s = J(`r(max)'+1, 3, 0)
	forval reps = 0/`r(max)' {
	    preserve 
			local repsindex= `reps'+1
			qui: keep if reps == `reps' 
			qui: xtset firmid date 
			//profit 
			qui: xtreg profitshare markup_`spec'_log_std i.date, fe robust 
			matrix temp = e(b)
			matrix table_`spec'_p[`repsindex',1] = temp[1,1]
			matrix temp = e(V)
			matrix table_`spec'_p[`repsindex',2] = sqrt(temp[1,1])
			matrix table_`spec'_p[`repsindex',3] = e(r2)
			//profit 
			qui: xtreg materialshare markup_`spec'_log_std i.date, fe robust  
			matrix temp = e(b)
			matrix table_`spec'_m[`repsindex',1] = temp[1,1]
			matrix temp = e(V)
			matrix table_`spec'_m[`repsindex',2] = sqrt(temp[1,1])
			matrix table_`spec'_m[`repsindex',3] = e(r2)
			//profit 
			qui: xtreg marketshare markup_`spec'_log_std  i.date, fe robust 
			matrix temp = e(b)
			matrix table_`spec'_s[`repsindex',1] = temp[1,1]
			matrix temp = e(V)
			matrix table_`spec'_s[`repsindex',2] = sqrt(temp[1,1])
			matrix table_`spec'_s[`repsindex',3] = e(r2)
		restore 
	}
}




*** export 

* turn matrices into variables 
clear 
foreach spec in dgm acf true {
	foreach depvar in p m s {
		svmat table_`spec'_`depvar' 
		}
}

* save results
//row 1:  true 
qui: su table_true_p1
matrix results1 = round(r(mean) , 0.0001) 
qui: su table_true_p2  
matrix results1 = results1 \ r(mean)   
qui: su table_true_p3  
matrix results1 = results1 \ round(r(mean),0.001)
qui: su table_true_m1
matrix results2 = round(r(mean) , 0.0001)  
qui: su table_true_m2  
matrix results2 = results2 \ r(mean)   
qui: su table_true_m3  
matrix results2 = results2 \ round(r(mean),0.001)  
qui: su table_true_s1
matrix results3 = round(r(mean) , 0.0001)  
qui: su table_true_s2  
matrix results3 = results3 \ r(mean)   
qui: su table_true_s3  
matrix results3 = results3 \ r(mean)   
matrix results_true = results1, results2, results3 
//row 2:  dgm 
qui: su table_dgm_p1
matrix results1 = round(r(mean) , 0.0001)  
qui: su table_dgm_p2  
matrix results1 = results1 \ r(mean)   
qui: su table_dgm_p3  
matrix results1 = results1 \ round(r(mean),0.001)   
qui: su table_dgm_m1
matrix results2 = round(r(mean) , 0.0001)   
qui: su table_dgm_m2  
matrix results2 = results2 \ r(mean)   
qui: su table_dgm_m3  
matrix results2 = results2 \ round(r(mean),0.001) 
qui: su table_dgm_s1
matrix results3 = round(r(mean) , 0.0001)  
qui: su table_dgm_s2  
matrix results3 = results3 \ r(mean)   
qui: su table_dgm_s3  
matrix results3 = results3 \ round(r(mean),0.001)   
matrix results_dgm = results1, results2, results3 
//row 2:  acf 
qui: su table_acf_p1
matrix results1 = round(r(mean) , 0.0001)   
qui: su table_acf_p2  
matrix results1 = results1 \ r(mean)   
qui: su table_acf_p3  
matrix results1 = results1 \ round(r(mean),0.001)  
qui: su table_acf_m1
matrix results2 = round(r(mean) , 0.0001)    
qui: su table_acf_m2  
matrix results2 = results2 \ r(mean)   
qui: su table_acf_m3  
matrix results2 = results2 \ round(r(mean),0.001)   
qui: su table_acf_s1
matrix results3 = round(r(mean) , 0.0001)   
qui: su table_acf_s2  
matrix results3 = results3 \ r(mean)   
qui: su table_acf_s3  
matrix results3 = results3 \ round(r(mean),0.001)  
matrix results_acf = results1, results2, results3 


** export tables 
matrix results_full = results_true \results_dgm \results_acf 
putexcel set output/table3, replace 
putexcel A1 = matrix(results_full)





