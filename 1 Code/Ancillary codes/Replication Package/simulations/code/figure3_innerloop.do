//produce markups  
	use "$pathdata/$dataset", clear 
	merge m:1 reps using "analysisdata/coefficients_byind_acf_$par", nogen 
	foreach var in betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
		rename `var' `var'_acf
	}
	merge m:1 reps using "analysisdata/coefficients_byind_dgm_$par", nogen 
	foreach var in betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
		rename `var' `var'_dgm 
	}

	//calculate elasticities 
	gen elast_dgm = betav_tl1_dgm + betavk_tl_dgm*k + 2*betav_tl2_dgm*v 
	gen elast_acf = betav_tl1_acf + betavk_tl_acf*k + 2*betav_tl2_acf*v 

	// calculate the markup 
	gen markup_dgm_log = log(elast_dgm*saleslevelfirm/(var_input*var_price))
	gen markup_acf_log = log(elast_acf*saleslevelfirm/(var_input*var_price))
	gen markup_true_log= log(mu_true)
	
	// create empty variables
	foreach spec in dgm acf true {
		qui: gen `spec'_true_corr = . 
		qui: gen `spec'_avg = . 
		qui: gen `spec'_sd = . 
	}

	// calculate moments by repetition 
	foreach spec in dgm acf true {
		qui: su reps 
		forval reps = 0/`r(max)' {
			qui: corr markup_`spec'_log markup_true_log 	if reps == `reps' 
			qui: replace `spec'_true_corr = `r(rho)' 		if reps == `reps' 
			qui: su markup_`spec'_log     					if reps == `reps' ,d 
			qui: replace `spec'_avg = `r(mean)' 			if reps == `reps' 
			qui: replace `spec'_sd = `r(sd)' 				if reps == `reps' 
		}
	}
	duplicates drop reps, force 
	keep acf* dgm* true*
	
		
	// store quantity moments  
	qui: su dgm_true_corr,d
	matrix temp1= r(mean)
	matrix temp4= r(p25)
	matrix temp5= r(p75)
	qui: su dgm_avg
	matrix temp2= r(mean)
	qui: su true_avg
	matrix temp2[1,1]= temp2[1,1]-r(mean)  
	qui: su dgm_sd
	matrix temp3= r(mean)
	matrix dgm_$par = [temp1,temp2,temp3, temp4, temp5]

	// store revenue moments  
	qui: su acf_true_corr,d 
	matrix temp1= r(mean)
	matrix temp4= r(p25)
	matrix temp5= r(p75)
	qui: su acf_avg
	matrix temp2= r(mean)
	qui: su true_avg
	matrix temp2[1,1]= temp2[1,1]-r(mean)  
	qui: su acf_sd
	matrix temp3= r(mean)
	matrix acf_$par = [temp1,temp2,temp3, temp4, temp5]	