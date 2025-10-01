*** figure 2

*** open and prepare the data

* open data 
use "$pathdata/$dataset", clear 
ssc install xtabond2, replace 

* create variables 
gen v2 = v*v 
gen k2 = k*k 
gen vk = v*k

* set panel
xtset firmid date 



*** create various levels of noise in measurement error 

qui: su eps_q
local sigma_orig = r(sd)
* Define new desired variances to test
foreach sigma_new in 2 4 8 16 {
    
    * Calculate scaling factor k
    local k = sqrt(`sigma_new'/`sigma_orig')
    
    * Create new scaled error
    gen eps_q_`sigma_new' = `k' * eps_q
    
    * Create new noisy x 
    gen q_obs_`sigma_new' = q + eps_q_`sigma_new'
	gen r_defl_obs_`sigma_new' = r + eps_q_`sigma_new'
}
rename q_obs q_obs_1 
rename r_defl_obs r_defl_obs_1 




*** obtain bb coefficients 

* loop 
qui: su reps 
global rmax = r(max)
forval reps = 0/`r(max)' { 

	preserve 
		* keep relevant sample 
		keep if reps == `reps' // replace 

		* obtain relevant coefficients 
		qui: xtabond2 q l(0/1).v l(0/1).k l(0/1).v2 l(0/1).k2 l(0/1).vk l.q , gmm(q v k v2 k2 vk , lag(3 3))  
		qui: md_ar1, nx( 5 ) beta(e(b)) cov(e(V))

		* save the coefficients 
		gen betav_tl1_bbq = all[1,1]
		gen betav_tl2_bbq = all[3,1]
		gen betavk_tl_bbq  = all[5,1]

		* obtain relevant coefficients 
		qui: xtabond2 r l(0/1).v l(0/1).k l(0/1).v2 l(0/1).k2 l(0/1).vk l.r , gmm(r v k v2 k2 vk , lag(3 3))  
		qui: md_ar1, nx( 5 ) beta(e(b)) cov(e(V))

		* save the coefficients 
		gen betav_tl1_bbr = all[1,1]
		gen betav_tl2_bbr = all[3,1]
		gen betavk_tl_bbr = all[5,1]
		
		* repeat with errors 
		foreach sigma_new in 1 2 4 8 16 {
			
			// obtain quantity coefficients 
			qui: xtabond2 q_obs_`sigma_new' l(0/1).v l(0/1).k l(0/1).v2 l(0/1).k2 l(0/1).vk l.q_obs_`sigma_new' , gmm(q_obs_`sigma_new' v k v2 k2 vk , lag(3 3))  
			qui: md_ar1, nx( 5 ) beta(e(b)) cov(e(V))
			gen betav_tl1_bbq_`sigma_new' = all[1,1]
			gen betav_tl2_bbq_`sigma_new' = all[3,1]
			gen betavk_tl_bbq_`sigma_new' = all[5,1]

			// obtain revenue coefficients 
			qui: xtabond2 r_defl_obs_`sigma_new' l(0/1).v l(0/1).k l(0/1).v2 l(0/1).k2 l(0/1).vk l.r_defl_obs_`sigma_new' , gmm(r_defl_obs_`sigma_new' v k v2 k2 vk , lag(3 3))  
			qui: md_ar1, nx( 5 ) beta(e(b)) cov(e(V))
			gen betav_tl1_bbr_`sigma_new' = all[1,1]
			gen betav_tl2_bbr_`sigma_new' = all[3,1]
			gen betavk_tl_bbr_`sigma_new' = all[5,1]
			
		} 
		
		* Keep one observation per industry with coefficients
		qui: duplicates drop reps  , force
		keep reps beta* 

		*save and restore
		save  temp/coefficients_ind`reps'  , replace 
	restore 	
} 

*coefficients 
clear 
use  temp/coefficients_ind0 , clear
qui: su reps  
forval i = 1/$rmax{						 
	append using  temp/coefficients_ind`i'
}

*save $temp/coefficients_byind, replace 
save  analysisdata/coefficients_byind_bbq, replace 



** obtain the DGM, ACF coefficients
foreach sigma in 1 2 4 8 16 {

	* revenue 
	global depvar  revenuefirm_obs_`sigma' 
	do code/acf_estimator 
} 
foreach sigma in 1 2 4 8 16 {

	* quantity 
	global depvar  outputfirm_obs_`sigma'
	do code/dgm_estimator 

} 







** analyse the BB markups 

* calculate the elasticities 
use "$pathdata/$dataset", clear 
merge m:1 reps using  analysisdata/coefficients_byind_bbq

* repeat with errors 
gen elast_q = betav_tl1_bbq + betavk_tl_bbq*k + 2*betav_tl2_bbq*v
gen elast_r = betav_tl1_bbr + betavk_tl_bbr*k + 2*betav_tl2_bbr*v
foreach spec in q r { 
	foreach sigma_new in 1 2 4 8 16 {
		gen elast_`spec'_error`sigma_new' = betav_tl1_bb`spec'_`sigma_new' + betavk_tl_bb`spec'_`sigma_new'*k + 2*betav_tl2_bb`spec'_`sigma_new'*v
	} 
}

* calculate the markups 
gen markup_q  =elast_q * saleslevelfirm/(var_input* var_price)
gen markup_r  =elast_r * saleslevelfirm/(var_input* var_price)
foreach spec in q r { 
	foreach sigma_new in 1 2 4 8 16 {
		gen markup_`spec'_error`sigma_new'_log  = log(elast_`spec'_error`sigma_new' * saleslevelfirm/(var_input* var_price))
	} 
}
gen markup_true_log = log(mu_true)

* calculate average correlation 
foreach spec in r q {
   	foreach sigma_new in 1 2 4 8 16 { 
	    qui: gen `spec'_error`sigma_new'_true_corr = . 
		qui: su reps 
		forval reps = 0/`r(max)' {
			qui: corr markup_`spec'_error`sigma_new'_log markup_true_log 	if reps == `reps' 
			qui: replace `spec'_error`sigma_new'_true_corr = `r(rho)' 		if reps == `reps' 
		}
	}
}

* keep single observation per rep 
duplicates drop reps, force 
keep reps *corr


* create plots 
//save results   
qui: su r_error1_true_corr, d 
matrix plot_r = r(mean)
matrix plot_r_lb = r(p25)
matrix plot_r_ub = r(p75)
qui: su r_error2_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su r_error4_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su r_error8_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su r_error16_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su q_error1_true_corr, d 
matrix plot_q =  r(mean)
matrix plot_q_lb =   r(p25)
matrix plot_q_ub =   r(p75)
qui: su q_error2_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
qui: su q_error4_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
su q_error8_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
su q_error16_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
//create plot
clear
svmat plot_r 
svmat plot_q
svmat plot_r_lb
svmat plot_r_ub
svmat plot_q_lb
svmat plot_q_ub
gen xaxis = 1
replace xaxis = 2  if _n == 2 
replace xaxis = 4  if _n == 3 
replace xaxis = 8  if _n == 4 
replace xaxis = 16 if _n == 5
//plot 
graph twoway ///
    (rarea plot_q_lb plot_q_ub xaxis, color(blue%20) lcolor(%0)) || ///
    (rarea plot_r_lb plot_r_ub xaxis, color(red%20) lcolor(%0)) || ///
    (connected plot_q1 xaxis, lcolor(blue) mcolor(blue) msymbol(Oh) msize(large) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected plot_r1 xaxis, lcolor(red) mcolor(red) msymbol(Oh) msize(large) lpattern(dash)    legend(label(4 "Revenue"))) ///
    , xsize(5) ysize(3.2) aspectratio(0.6) ylabel(0(0.5)1, grid labsize(large)) yscale(range(-0.0 1)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Correlation with True Markups", size(large)) ///
    xtitle("Std. Dev. of Measurement Error (1 = baseline)", size(large)) ///
    xlabel(1 2 4 8 16, grid labsize(large)) ylabel(,grid) plotregion(lcolor(black)) ///
	legend(position(1) ring(0) col(1) label(3 "Quantity") label(4 "Revenue") order(3 4)) ///
    xscale(log)
graph export output/figure2a.pdf, replace






** analyse the ACF and DGM estimates 


** analyse the BB markups 

* calculate the elasticities for quantity 
use "$pathdata/$dataset", clear 
merge m:1 reps using analysisdata/coefficients_byind_dgm_outputfirm_obs_1, nogen 
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_dgm_1 
}
merge m:1 reps using analysisdata/coefficients_byind_dgm_outputfirm_obs_2 , nogen 
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_dgm_2
}
merge m:1 reps using  analysisdata/coefficients_byind_dgm_outputfirm_obs_4, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_dgm_4 
}
merge m:1 reps using  analysisdata/coefficients_byind_dgm_outputfirm_obs_8, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_dgm_8
}
merge m:1 reps using  analysisdata/coefficients_byind_dgm_outputfirm_obs_16, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_dgm_16
}
merge m:1 reps using  analysisdata/coefficients_byind_acf_revenuefirm_obs_1, nogen 
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_acf_1 
}
merge m:1 reps using  analysisdata/coefficients_byind_acf_revenuefirm_obs_2, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_acf_2
}
merge m:1 reps using  analysisdata/coefficients_byind_acf_revenuefirm_obs_4, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_acf_4 
}
merge m:1 reps using  analysisdata/coefficients_byind_acf_revenuefirm_obs_8, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_acf_8
}
merge m:1 reps using  analysisdata/coefficients_byind_acf_revenuefirm_obs_16, nogen  
foreach var in betav_tl1 betav_tl2  betavk_tl {
	rename `var' `var'_acf_16
}
foreach spec in acf dgm { 
	foreach sigma_new in 1 2 4 8 16 {
		gen elast_`spec'_error`sigma_new' = betav_tl1_`spec'_`sigma_new' + betavk_tl_`spec'_`sigma_new'*k + 2*betav_tl2_`spec'_`sigma_new'*v
	} 
}

* calculate the markups 
foreach sigma_new in 1 2 4 8 16 {
		gen markup_r_error`sigma_new'_log  = log(elast_acf_error`sigma_new' * saleslevelfirm/(var_input* var_price))
} 
foreach sigma_new in 1 2 4 8 16 {
	gen markup_q_error`sigma_new'_log  = log(elast_dgm_error`sigma_new' * saleslevelfirm/(var_input* var_price))
} 
gen markup_true_log = log(mu_true)

* calculate average correlation 
foreach spec in r q {
   	foreach sigma_new in 1 2 4 8 16 { 
	    qui: gen `spec'_error`sigma_new'_true_corr = . 
		qui: su reps 
		forval reps = 0/`r(max)' {
			qui: corr markup_`spec'_error`sigma_new'_log markup_true_log 	if reps == `reps' 
			qui: replace `spec'_error`sigma_new'_true_corr = `r(rho)' 		if reps == `reps' 
		}
	}
}

* keep single observation per rep 
duplicates drop reps, force 
keep reps *corr


* create plots 
//save results   
qui: su r_error1_true_corr, d 
matrix plot_r = r(mean)
matrix plot_r_lb = r(p25)
matrix plot_r_ub = r(p75)
qui: su r_error2_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su r_error4_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su r_error8_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su r_error16_true_corr, d 
matrix plot_r = plot_r \ r(mean)
matrix plot_r_lb = plot_r_lb \  r(p25)
matrix plot_r_ub = plot_r_ub \  r(p75)
qui: su q_error1_true_corr, d 
matrix plot_q =  r(mean)
matrix plot_q_lb =   r(p25)
matrix plot_q_ub =   r(p75)
qui: su q_error2_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
qui: su q_error4_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
su q_error8_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
su q_error16_true_corr, d 
matrix plot_q = plot_q \ r(mean)
matrix plot_q_lb = plot_q_lb \  r(p25)
matrix plot_q_ub = plot_q_ub \  r(p75)
//create plot
clear
svmat plot_r 
svmat plot_q
svmat plot_r_lb
svmat plot_r_ub
svmat plot_q_lb
svmat plot_q_ub
gen xaxis = 1
replace xaxis = 2  if _n == 2 
replace xaxis = 4  if _n == 3 
replace xaxis = 8  if _n == 4 
replace xaxis = 16 if _n == 5
//plot 
graph twoway ///
    (rarea plot_q_lb plot_q_ub xaxis, color(blue%20) lcolor(%0)) || ///
    (rarea plot_r_lb plot_r_ub xaxis, color(red%20) lcolor(%0)) || ///
    (connected plot_q1 xaxis, lcolor(blue) mcolor(blue) msymbol(Oh) msize(large) lpattern(solid) ) || ///
    (connected plot_r1 xaxis, lcolor(red) mcolor(red) msymbol(Oh) msize(large) lpattern(dash)    ) ///
    , xsize(5) ysize(3.2) aspectratio(0.6) ylabel(0(0.5)1, grid labsize(large)) yscale(range(-0.0 1)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Correlation with True Markups", size(large)) ///
    xtitle("Std. Dev. of Measurement Error (1 = baseline)", size(large)) ///
    xlabel(1 2 4 8 16, grid labsize(large)) ylabel(,grid) plotregion(lcolor(black)) legend(off) ///
    xscale(log)
graph export output/figure2b.pdf, replace






* clean up the coefficient estimates
/*
forval i = 0/$rmax{						 
	erase  coefficients_ind`i'.dta 
}
*/ 
