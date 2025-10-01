*** letter table
* mdr 
* 15-3-25 

/*
	This file produces the R3 table. 
*/ 



** Preamble

* open data 
use "$pathdata/$dataset", clear 

* prepare variables 
xtset firmid date 
gen ptilde= p - 0.6*l.p
gen vlag = l.v 
gen vlagsq = l.v^2 
gen vlagk = l.v*k 
gen ksq = k^2

* give basic correlation matrix 
corr ptilde vlag  k vlagsq ksq vlagk 


* Loop through each variable and calculate correlation with ptilde by reps
local varlist vlag k vlagsq ksq vlagk

matrix corrs = .
matrix corrs_sd = .
foreach var of local varlist {
    * Get unique values of reps
    quietly levelsof reps, local(rep_values)
    
    * Initialize local to store correlations
    local corrs ""
    
    * Calculate correlation for each rep
    foreach rep of local rep_values {
        quietly correlate ptilde `var' if reps == `rep'
        local corrs "`corrs' `r(rho)'"
    }
    
    * Calculate mean and sd manually
    local sum = 0
    local count = 0
    foreach c of local corrs {
        local sum = `sum' + `c'
        local count = `count' + 1
    }
    local mean = `sum' / `count'
    
    * Calculate standard deviation
    local sum_sq_dev = 0
    foreach c of local corrs {
        local dev = `c' - `mean'
        local sum_sq_dev = `sum_sq_dev' + (`dev' * `dev')
    }
    local var = `sum_sq_dev' / (`count' - 1)
    local sd = sqrt(`var')
    
    * Display result
    display "`var' {col 15} " %9.6f `mean' " {col 35} " %9.6f `sd'
	matrix corrs = corrs \  `mean'
	matrix corrs_sd = corrs_sd \ `sd' 
}




** Produce table
putexcel set output/tableF1.xlsx, replace 
putexcel A1 = matrix(corrs')                                                                                                                                                                 
putexcel A2 = matrix(corrs_sd')                                                                                                                                                                 


 