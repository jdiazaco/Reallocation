*** table 1 
* mdr 
* 24-12-24 


/* 
	This file creates a table with the production function coefficients estimates 
*/ 


** prepare dataset

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



** produce the output 

* calculate the true values 
gen o = log(prodfirm)
gen v2= v^2
gen k2= k^2
gen vk= v*k 
reg q v k v2 k2 vk o 
matrix results = e(b)
matrix true	=  results[1,1], results[1,2], results[1,3], results[1,4], round(results[1,5],0.001)
gen elasticity_true = results[1,1] +  results[1,5]*k + 2*results[1,3]*v 

* obtain average elasticity per specification 
bysort reps: egen elast_dgm_avg = mean(elast_dgm) 
bysort reps: egen elast_dgm_sd  = sd(elast_dgm) 
bysort reps: egen elast_acf_avg = mean(elast_acf) 
bysort reps: egen elast_acf_sd  = sd(elast_acf) 

* keep 1 obs per reps 
duplicates drop reps, force 
keep *elast* beta* 

* obtain statistics per coefficient per specification 
matrix table = J(5, 5, 0)
matrix table[1,1] = true
local index = 0 
foreach coef in  betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
	local index = `index' +1 
	qui: su `coef'_dgm 
	matrix table[2,`index'] = round(r(mean),0.01)
	matrix table[3,`index'] = round(r(sd),0.001)
	if `index' > 2  { //regime: round to 3 digits if true value has 3 digit 
		matrix table[2,`index'] = round(r(mean),0.001)
		matrix table[3,`index'] = round(r(sd),0.0001)
	}
} 
local index = 0 
foreach coef in  betav_tl1 betak_tl1 betav_tl2 betak_tl2  betavk_tl {
	local index = `index' +1 
	qui: su `coef'_acf 
	matrix table[4,`index'] = round(r(mean),0.01)
	matrix table[5,`index'] = round(r(sd),0.001)
	if `index' > 2  { //regime: round to 3 digits if true value has 3 digit 
		matrix table[4,`index'] = round(r(mean),0.001)
		matrix table[5,`index'] = round(r(sd),0.0001)
	}
} 
                                        

** export results
local rownames "True value Quantity Baseline Revenue ACF"
local colnames "Beta_v Beta_k Beta_vv Beta_kk Beta_vk"
putexcel set output/table1, replace 

* Write column headers starting from B1 (A1 will be for row labels)
local colnames "Beta_v Beta_k Beta_vv Beta_kk Beta_vk"
local i = 2
foreach name of local colnames {
    local col = char(64 + `i')  // 66 = B, so this gives B, C, D, etc.
    putexcel `col'1 = ("`name'")
    local ++i
}
* Write row labels in column A starting from A2
putexcel A2 = ("True value")
putexcel A3 = ("Quantity")
putexcel A4 = ("Baseline")
putexcel A5 = ("Revenue")
putexcel A6 = ("ACF")
       
* Write the matrix starting from B2 (which corresponds to row 1 column 1 of the matrix)
putexcel B2 = matrix(table)                                                                                                                                                                 




