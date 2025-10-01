*** letter table
* mdr 
* 15-3-25 

/*
	This file produces the R3 table. 
*/ 



** Preamble

* open data 
use "$pathdata/results_dataReps_ForStata_V23_fixed_multi_2_sigma1d1_epsi10_rhoz0d6_sigmas0d2_0d66_0_alpha0d4_gamma0d8_eta1d1_Nk8_N320000_sigma_wage0d24495_sigma_pm0d24495_sigma_PsigmaY0d43589_saved_reps", clear 

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
* Loop through variables
local varlist vlag k vlagsq ksq vlagk

display _newline "Correlation summary for ptilde with each variable:"
display "{hline 60}"
display "Variable {col 15} Mean Correlation {col 35} Std. Dev."
display "{hline 60}"

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
}

display "{hline 60}"

display "{hline 60}"

** Produce table


 



** Export 