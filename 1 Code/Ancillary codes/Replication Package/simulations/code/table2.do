*** table 2 


** prepare dataset

* open the panel,  merge with estimates  
use "$pathdata/$dataset", clear 
merge m:1 reps using  analysisdata/coefficients_byind_dgm, nogen 
foreach var in betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
	rename `var' `var'_dgm 
}
merge m:1 reps using  analysisdata/coefficients_byind_acf
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


** prepare output 

* create empty variables
foreach spec in dgm acf true {
	qui: gen `spec'_true_corr = . 
	qui: gen `spec'_avg = . 
	qui: gen `spec'_sd = . 
	qui: gen `spec'_med = . 
	qui: gen `spec'_iqr = . 	
}

* calculate moments by repetition 
foreach spec in dgm acf true {
	qui: su reps 
	forval reps = 0/`r(max)' {
		qui: corr markup_`spec'_log markup_true_log 	if reps == `reps' 
		qui: replace `spec'_true_corr = `r(rho)' 		if reps == `reps' 
		qui: su markup_`spec'_log     					if reps == `reps' ,d 
		qui: replace `spec'_avg = `r(mean)' 			if reps == `reps' 
		qui: replace `spec'_sd = `r(sd)' 				if reps == `reps' 
		qui: replace `spec'_med = `r(p50)' 				if reps == `reps' 
		qui: replace `spec'_iqr = `r(p75)' -  `r(p25)' 	if reps == `reps' 
	}
}
duplicates drop reps, force 
keep acf* dgm* true*



** produce table 

* true line 
qui: su true_true_corr
matrix temp1= [round(r(mean),0.01)\ .]
qui: su true_avg
matrix temp2= [round(r(mean),0.01)\ round(r(sd),0.0001)]
qui: su true_sd
matrix temp3= [round(r(mean),0.01)\ round(r(sd),0.0001)]
qui: su true_med
matrix temp4= [round(r(mean),0.01)\ round(r(sd),0.0001)]
qui: su true_iqr
matrix temp5= [round(r(mean),0.01)\ round(r(sd),0.0001)]
matrix true = [temp1,temp2,temp3,temp4,temp5] 

* quantity line 
qui: su dgm_true_corr
matrix temp1= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su dgm_avg
matrix temp2= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su dgm_sd
matrix temp3= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su dgm_med
matrix temp4= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su dgm_iqr
matrix temp5= [round(r(mean),0.01)\ round(r(sd),0.001)]
matrix dgm = [temp1,temp2,temp3,temp4,temp5]

* revenue line 
qui: su acf_true_corr
matrix temp1= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su acf_avg
matrix temp2= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su acf_sd
matrix temp3= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su acf_med
matrix temp4= [round(r(mean),0.01)\ round(r(sd),0.001)]
qui: su acf_iqr
matrix temp5= [round(r(mean),0.01)\ round(r(sd),0.001)]
matrix acf = [temp1,temp2,temp3,temp4,temp5]

* combine
matrix table = [true\dgm\acf] 

** export results
putexcel set output/table2, replace 
putexcel A1 = matrix(table)
clear 