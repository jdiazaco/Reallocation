*** figure 1

** prepare dataset
ssc install binscatter 

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

* first-differenced
xtset firmid date 
gen d_markup_dgm_log  = d.markup_dgm_log 
gen d_markup_acf_log  = d.markup_acf_log 
gen d_markup_true_log = d.markup_true_log 


** take binscatter over all repetitions because we're not interested in variance  --> binscatter for all repetitions combined is same as running seperately + take avg 

*levels
binscatter markup_dgm_log markup_acf_log, graphregion(margin(0.5 0.5 0.5 4) lcolor(white))  ///
	xsize(5) ysize(4.5) aspectratio(1)  nquantiles(40) lcolor(red) mcolors(blue) msymbols(Oh) ///
	ytitle("Quantity-Based Markup (log)") xtitle("Revenue-Based Markup (log)") xlabel(,grid) ylabel(,grid) plotregion(lcolor(black))
graph export output/figure1a.pdf, replace

* first difference
binscatter d_markup_dgm_log d_markup_acf_log, graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
	xsize(5) ysize(4.5) aspectratio(1)  nquantiles(40) lcolor(red) mcolors(blue) msymbols(Oh) ///
	ytitle("Quantity-Based Markup ({&Delta} log)") xtitle("Revenue-Based Markup ({&Delta} log)") xlabel(,grid) ylabel(,grid) plotregion(lcolor(black))
graph export output/figure1b.pdf, replace

* obtain regression coefficients 
/*
reg    markup_dgm_log   markup_acf_log 
outreg2 using figure1, replace 
reg  d_markup_dgm_log d_markup_acf_log 
outreg2 using figure1, append 
*/ 

