*** figure 3 


** preamble 
clear 


** estimate coefficients

* alpha 
foreach alpha in 4 5 6 7 8 9   {

	//preamble
	global par alpha_`alpha'
	global dataset results_dataReps_ForStata_alpha_sigma1d1_epsi10_alpha0d`alpha'_gamma0d8_eta1d1_saved_reps
	//run estimators 
	global depvar outputfirm_obs
	do code/dgm_estimator 
	global depvar revenuefirm_obs
	do code/acf_estimator 
}

*alpha plot
foreach alpha in 4 5 6 7 8 9   {

	//preamble
	global par alpha_`alpha'
	global dataset results_dataReps_ForStata_alpha_sigma1d1_epsi10_alpha0d`alpha'_gamma0d8_eta1d1_saved_reps
	
	//produce figure 
	do code/figure3_innerloop 
}
clear
* alpha
//create variables
foreach alpha in 4 5 6 7 8 9   {
	matrix dgm_alpha_`alpha' = dgm_alpha_`alpha'' 
	matrix acf_alpha_`alpha' = acf_alpha_`alpha'' 
	svmat dgm_alpha_`alpha'
	svmat acf_alpha_`alpha'
}
gen id= _n 
//collapse 
reshape long dgm_alpha_ acf_alpha_, i(id) j(alpha) string
replace alpha = substr(alpha, 1, 1)
destring alpha, replace 
replace alpha = alpha*0.1
reshape wide dgm_alpha_ acf_alpha_, i(alpha) j(id)
gen dgm_lbound= dgm_alpha_2-dgm_alpha_3
gen dgm_ubound= dgm_alpha_2+dgm_alpha_3
gen acf_lbound= acf_alpha_2-acf_alpha_3
gen acf_ubound= acf_alpha_2+acf_alpha_3
graph twoway ///
    (rarea dgm_alpha_4 dgm_alpha_5 alpha, color(blue%20) lcolor(%0)) || ///
    (rarea acf_alpha_4 acf_alpha_5 alpha, color(red%20) lcolor(%0)) || ///
    (connected dgm_alpha_1 alpha, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_alpha_1 alpha, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci 0 0.4 1 0.4, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1.0) ylabel(0(0.5)1, grid labsize(vhuge)) yscale(range(-0.0 1)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Correlation - True Markups", size(vhuge)) ///
    title("Parameter {&alpha}", size(vhuge)) ///
    xlabel( 0.4 0.5 0.6 0.7 0.8 0.9, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(position(4) ring(0) col(1) label(3 "Quantity") label(4 "Revenue") order(3 4) size(vhuge)) ///
    xscale(range(0.4 0.9))
graph export output/figure3a.pdf, replace

graph twoway ///
    (rarea dgm_lbound dgm_ubound alpha, color(blue%20) lcolor(%0)) || ///
    (rarea acf_lbound acf_ubound alpha, color(red%20) lcolor(%0)) || ///
    (connected dgm_alpha_2 alpha, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_alpha_2 alpha, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci -0.3 0.4 0.3 0.4, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1.0) ylabel(-0.3(0.3)0.3, grid labsize(vhuge)) yscale(range(-0.3 0.3)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Difference - True Mean", size(vhuge)) ///
    xlabel(0.4 0.5 0.6 0.7 0.8 0.9, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(0.4 0.9))
graph export output/figure3e.pdf, replace







* gamma
foreach gamma in 0d6 0d7 0d8 0d9 1 {

	//preamble
	global par gamma_`gamma'
	global dataset results_dataReps_ForStata_gamma_sigma1d1_epsi10_alpha0d4_gamma`gamma'_eta1d1_saved_reps
	
	//run estimators 
	global depvar outputfirm_obs
	do code/dgm_estimator 
	global depvar revenuefirm_obs
	do code/acf_estimator 
}


*gamma plot
foreach gamma in 0d6 0d7 0d8 0d9 1 {

	//preamble
	global par gamma_`gamma'
	global dataset results_dataReps_ForStata_gamma_sigma1d1_epsi10_alpha0d4_gamma`gamma'_eta1d1_saved_reps
	
	//produce figure 
	do code/figure3_innerloop 
}

** gamma
clear
//create variables
foreach gamma in 0d6 0d7 0d8 0d9 1 {
	matrix dgm_gamma_`gamma' = dgm_gamma_`gamma'' 
	matrix acf_gamma_`gamma' = acf_gamma_`gamma'' 
	svmat dgm_gamma_`gamma'
	svmat acf_gamma_`gamma'
}
gen id= _n 
//collapse 
reshape long dgm_gamma_ acf_gamma_, i(id) j(gamma) string
replace gamma = substr(gamma, 1, 3)
replace gamma = "1d0" if gamma == "11"
replace gamma = substr(gamma, 1, 1) + "." +  substr(gamma, 3, 1) 
destring gamma, replace 
reshape wide dgm_gamma_ acf_gamma_, i(gamma) j(id)
gen dgm_lbound= dgm_gamma_2-dgm_gamma_3
gen dgm_ubound= dgm_gamma_2+dgm_gamma_3
gen acf_lbound= acf_gamma_2-acf_gamma_3
gen acf_ubound= acf_gamma_2+acf_gamma_3
graph twoway ///
    (rarea dgm_gamma_4 dgm_gamma_5 gamma, color(blue%20) lcolor(%0)) || ///
    (rarea acf_gamma_4 acf_gamma_5 gamma, color(red%20) lcolor(%0)) || ///
    (connected dgm_gamma_1 gamma, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_gamma_1 gamma, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci 0 0.8 1 0.8, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1.0) ylabel(0(0.5)1, grid labsize(vhuge)) yscale(range(-0.0 1)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Correlation - True Markups", size(vhuge)) ///
    title("Parameter {&gamma}", size(vhuge)) ///
    xlabel(0.6 0.7 0.8 0.9 1.0, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(0.6 1.0))
graph export output/figure3b.pdf, replace

graph twoway ///
    (rarea dgm_lbound dgm_ubound gamma, color(blue%20) lcolor(%0)) || ///
    (rarea acf_lbound acf_ubound gamma, color(red%20) lcolor(%0)) || ///
    (connected dgm_gamma_2 gamma, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_gamma_2 gamma, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci -0.3 0.8 0.3 0.8, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1.0) ylabel(-0.3(0.3)0.3, grid labsize(vhuge)) yscale(range(-0.3 0.3)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Difference - True Mean", size(vhuge)) ///
    xlabel(0.6 0.7 0.8 0.9 1.0, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(0.6 1.0))
graph export output/figure3f.pdf, replace





* eta (phi in the manuscript)
foreach eta in 0d95  1 1d05 1d1 1d15 1d2 { // 

	//preamble
	global par eta_`eta'
	global dataset results_dataReps_ForStata_eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta`eta'_saved_reps

	//run estimators 
	global depvar outputfirm_obs
	do code/dgm_estimator 
	global depvar revenuefirm_obs
	do code/acf_estimator 
}


* eta plot 
foreach eta in 0d95  1 1d05 1d1 1d15 1d2 { // 

	//preamble
	global par eta_`eta'
	global dataset results_dataReps_ForStata_eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta`eta'_saved_reps
	
	//produce figure 
	do code/figure3_innerloop 
}
clear
//create variables
foreach eta in 0d95 1 1d05 1d1 1d15 1d2    { //     
	matrix dgm_eta_`eta' = dgm_eta_`eta'' 
	matrix acf_eta_`eta' = acf_eta_`eta'' 
	svmat dgm_eta_`eta'
	svmat acf_eta_`eta'
}
gen id= _n 
//collapse 
reshape long dgm_eta_ acf_eta_, i(id) j(eta) string
replace eta = substr(eta, 1, 4)
replace eta = "1d0" if eta == "11"
replace eta = substr(eta, 1, 1) + "." +  substr(eta, 3, 2) 
replace eta = "1.1" if eta == "1.11"
destring eta, replace 
reshape wide dgm_eta_ acf_eta_, i(eta) j(id)
gen dgm_lbound= dgm_eta_2-dgm_eta_3
gen dgm_ubound= dgm_eta_2+dgm_eta_3
gen acf_lbound= acf_eta_2-acf_eta_3
gen acf_ubound= acf_eta_2+acf_eta_3
graph twoway ///
    (rarea dgm_eta_4 dgm_eta_5 eta, color(blue%20) lcolor(%0)) || ///
    (rarea acf_eta_4 acf_eta_5 eta, color(red%20) lcolor(%0)) || ///
    (connected dgm_eta_1 eta, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_eta_1 eta, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci 0 1.1 1 1.1, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1) ylabel(0(0.5)1, grid labsize(vhuge)) yscale(range(-0.0 1)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Correlation - True Markups", size(vhuge)) ///
    title("Parameter {&phi}", size(vhuge)) ///
    xlabel(1 1.1 1.2, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(0.95 1.2))
graph export output/figure3c.pdf, replace

graph twoway ///
    (rarea dgm_lbound dgm_ubound eta, color(blue%20) lcolor(%0)) || ///
    (rarea acf_lbound acf_ubound eta, color(red%20) lcolor(%0)) || ///
    (connected dgm_eta_2 eta, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_eta_2 eta, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci -0.3 1.1 0.3 1.1, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1.0) ylabel(-0.3(0.3)0.3, grid labsize(vhuge)) yscale(range(-0.3 0.3)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Difference - True Mean", size(vhuge)) ///
    xlabel(1 1.1 1.2, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(0.95 1.2))
graph export output/figure3g.pdf, replace







* sigma 
foreach sigma in 1d1 1d6 2d1 2d6 3d1 {

	//preamble
	global par sigma_`sigma'
	global dataset results_dataReps_ForStata_sigma_sigma`sigma'_epsi10_alpha0d4_gamma0d8_eta1d1_saved_reps
	//run estimators 
	global depvar outputfirm_obs
	do code/dgm_estimator 
	global depvar revenuefirm_obs
	do code/acf_estimator 
}


* sigma plot
foreach sigma in 1d1 1d6 2d1 2d6 3d1 {

	//preamble
	global par sigma_`sigma'
	global dataset results_dataReps_ForStata_sigma_sigma`sigma'_epsi10_alpha0d4_gamma0d8_eta1d1_saved_reps
	
	//produce figure 
	do code/figure3_innerloop 
}

clear
//create variables
foreach sigma in 1d1 1d6 2d1 2d6 3d1 {
	matrix dgm_sigma_`sigma' = dgm_sigma_`sigma'' 
	matrix acf_sigma_`sigma' = acf_sigma_`sigma'' 
	svmat dgm_sigma_`sigma'
	svmat acf_sigma_`sigma'
}
gen id= _n 
//collapse 
reshape long dgm_sigma_ acf_sigma_, i(id) j(sigma) string
replace sigma = substr(sigma, 1, 4)
replace sigma = substr(sigma, 1, 1) + "." +  substr(sigma, 3, 2) 
destring sigma, replace 
replace sigma= sigma-0.01
reshape wide dgm_sigma_ acf_sigma_, i(sigma) j(id)
gen dgm_lbound= dgm_sigma_2-dgm_sigma_3
gen dgm_ubound= dgm_sigma_2+dgm_sigma_3
gen acf_lbound= acf_sigma_2-acf_sigma_3
gen acf_ubound= acf_sigma_2+acf_sigma_3
graph twoway ///
    (rarea dgm_sigma_4 dgm_sigma_5 sigma, color(blue%20) lcolor(%0)) || ///
    (rarea acf_sigma_4 acf_sigma_5 sigma, color(red%20) lcolor(%0)) || ///
    (connected dgm_sigma_1 sigma, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_sigma_1 sigma, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci 0 1.1 1 1.1, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1) ylabel(0(0.5)1, grid labsize(vhuge)) yscale(range(-0.0 1)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Correlation - True Markups", size(vhuge)) ///
    title("Parameter {&sigma}", size(vhuge)) ///
    xlabel(1.1 1.6 2.1 2.6 3.1, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(1.05 1.2))
graph export output/figure3d.pdf, replace

graph twoway ///
    (rarea dgm_lbound dgm_ubound sigma, color(blue%20) lcolor(%0)) || ///
    (rarea acf_lbound acf_ubound sigma, color(red%20) lcolor(%0)) || ///
    (connected dgm_sigma_2 sigma, lcolor(blue) mcolor(blue) msymbol(Oh) msize(vhuge) lpattern(solid) legend(label(3 "Quantity"))) || ///
    (connected acf_sigma_2 sigma, lcolor(red) mcolor(red) msymbol(Oh) msize(vhuge) lpattern(dash) legend(label(4 "Revenue"))) || ///
    (pci -0.3 1.1 0.3 1.1, lcolor(black) lpattern(dash)) ///
    , xsize(2) ysize(2) aspectratio(1.0) ylabel(-0.3(0.3)0.3, grid labsize(vhuge)) yscale(range(-0.3 0.3)) ///
    graphregion(margin(0.5 0.5 0.5 4) lcolor(white)) ///
    ytitle("Difference - True Mean", size(vhuge)) ///
    xlabel(1.1 1.6 2.1 2.6 3.1, grid labsize(vhuge)) ylabel(,grid) plotregion(lcolor(black)) ///
    legend(off) ///
    xscale(range(1.05 1.2))
graph export output/figure3h.pdf, replace