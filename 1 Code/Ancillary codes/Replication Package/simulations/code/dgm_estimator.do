*** estimate dgm coefficients  


***** 1. Preamble ******

* clean up 
clear  
clear mata 
set matsize 11000
set seed 1234
version 17 
mata: mata clear
mata: mata set matastrict off 

***** 2: Select Variables *****

*variable for sales (revenue) 
local sale outputfirm_obs

*variable for capital 
local capital fixed_input 
	
*variable for variable input
local varcost var_input

*variable for firm identifiers
local firm firmid

*variable for year
local year date 
  


***** 3: Select Method *****

*specification for AR(1) shock
	local prodar1_sq  0 //choose 1 or 0 

*include capital in criterion function 
	local gmm_k 1 
	
*include capital in production function estimation 
	local est_k 1  //choose 1 or 0 

*choose whether you want to add materials in the production function and in the estimation of phi 
	local mater 0 //choose 1,0 
	
*choose to winsorize yes/no
	local winsor 0 //choose 1,0 
	
*choose order of expansion for production factors
	local order 3

*choose years to be considered for analysis 
	local min_year 1
	local max_year 40

	
 
 
***** 4: Define Mata Progam (based on De Loecker & Warzynski)  *****



mata 

	rseed(1234)
	** Section 4.1: Cobb Douglas
	void GMM_DLW(todo,betas,crit,g,H)
	{
		//import fitted value of production function to be estimated in Section 6 	
		PHI 	= st_data(.,("phi")) 												
		PHI_LAG = st_data(.,("phi_lag"))
	
		//import the matrix of production factors to be uncorrelated with productivity shocks in the GMM moment condition 
		Z= st_data(.,("const","v_lag","k"))

		//specify the vector of variables in the production function that is estimated as part of the GMM estimation (does not have to equal variables used to estimate phi!) 
		X 		= st_data(.,("const","v","k"))
		X_lag	= st_data(.,("const","v_lag","k_lag"))
	
		//specificy the outcome variable	
		Y		= st_data(.,("s")) //s is the variable name in the new paper, dont use capital S as that name is used later on for different purpose) 
	
		//generate a constant for the OLS regression on productivity 	
		C		= st_data(.,("const"))

		//generate the productivity variable: fitted sales from OLS regression - fitted sales from production function GMM estimation 
		OMEGA			=PHI-X*betas'
		OMEGA_lag		=PHI_LAG-X_lag*betas'   
		OMEGA_lag_sq	=OMEGA_lag:*OMEGA_lag  //element-by-element square
		if ("`prodar1_sq'" == "1")  OMEGA_lag_pol	=(C,OMEGA_lag, OMEGA_lag_sq) 
		else 						OMEGA_lag_pol	=(C,OMEGA_lag)
	
		//OLS regression to find the productivity shocks which are to be orthogonal to lagged factor usage in the GMM estimation (note: actual regression is part of the GMM regression )
		g_b 			= invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA // g_b is the vector of coefficients for the productivity AR(1) estimation w = a+ rho*w(-1)+ xi 

		//definition of the productivity shocks 
		XI				=OMEGA-OMEGA_lag_pol*g_b
	
		//definition of GMM criterion 
		crit			=(Z'XI)'(Z'XI) 
	}
	// Start GMM estimation. 
	void DLW()		
	{
		S = optimize_init() 						
		optimize_init_evaluator(S, &GMM_DLW())		
		optimize_init_evaluatortype(S, "d0")		
		optimize_init_technique(S, "nm")			 
		optimize_init_nmsimplexdeltas(S, 0.1)		
		optimize_init_which(S, "min")				
		optimize_init_conv_nrtol(S, 1e-10)
		optimize_init_conv_maxiter(S, 10000)
		if ("`est_k'" == "1") {
			b = st_matrix("init")
			optimize_init_params(S, b)				
		} 
		rseed(1234)
		p = optimize(S) 							
		p
		st_matrix("beta_dlw",p)						
	}


	** Section 4.2: Translog
	void GMM_DLW_TL(todo,betas,crit,g,H) 
	{
		//import fitted value of production function to be estimated in Section 6 	
		PHI 	= st_data(.,("phi")) 												
		PHI_LAG = st_data(.,("phi_lag"))
		
		//import the matrix of production factors to be uncorrelated with productivity shocks in the GMM moment condition 
	    Z= st_data(.,("const","v_lag","k","v_lag2","k2","v_lagk"))

		//specify the vector of variables in the production function that is estimated as part of the GMM estimation (does not have to equal variables used to estimate phi!) 
	    X 		= st_data(.,("const","v","k","v2","k2","vk"))
	    X_lag	= st_data(.,("const","v_lag","k_lag","v_lag2","k_lag2","v_lagk_lag"))

		
		//specificy the outcome variable
		Y		= st_data(.,("s"))
	
		//generate a constant for the OLS regression on productivity 
		C		= st_data(.,("const"))
	
		//generate the productivity variable: fitted sales from OLS regression - fitted sales from production function GMM estimation 
		OMEGA 			= PHI - X*betas' 
		OMEGA_lag 		= PHI_LAG - X_lag*betas' 
		OMEGA_lag_sq	= OMEGA_lag:*OMEGA_lag  //element-by-element square
		if ("`prodar1_sq'" == "1")  OMEGA_lag_pol	=(C,OMEGA_lag, OMEGA_lag_sq) 
		else 						OMEGA_lag_pol	=(C,OMEGA_lag)
	
		//OLS regression to find the productivity shocks which are to be orthogonal to lagged factor usage in the GMM estimation (note: actual regression is part of the GMM regression )
		g_b				= invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA // g_b is the vector of coefficients for the productivity AR(1) estimation w = a+ rho*w(-1)+ xi 
	
		//definition of the productivity shocks 
		XI				= OMEGA - OMEGA_lag_pol*g_b
	
		//definition of GMM criterion 
		crit 			= (Z'XI)'(Z'XI) 
	}
	// start GMM estimation. See Section 4.1 for explanations 
	void DLW_TRANSLOG()		
	{
	    S = optimize_init() 
		optimize_init_evaluator(S, &GMM_DLW_TL())
		optimize_init_evaluatortype(S, "d0")
		optimize_init_technique(S, "nm")
		optimize_init_nmsimplexdeltas(S, 0.1)
		optimize_init_conv_maxiter(S, 10000)
		optimize_init_which(S, "min")
		rseed(1234)
		b = st_matrix("init_tl")
		optimize_init_params(S, b)
		optimize_init_conv_nrtol(S, 1e-10)
	
		qui: p = optimize(S)
		st_matrix("beta_dlwtranslog",p)
	}


end 
 

** Section 4.3 Define both programs 

*Cobb-douglas production function (DLW)
capture program drop dlw 
program dlw, rclass 
preserve 
sort `firm' year 
mata DLW()
end 

*Translog production function (DLW_TRANSLOG)
cap program drop dlw_translog 
program dlw_translog , rclass 
preserve 
sort `firm' year 
mata DLW_TRANSLOG()
end 

 

 

 

 
***** 5: Open Dataset, Generate Variables and Perform Cleaning  *****


** 5.1 Open dataset and perform cleaning 

* open dataset 
use "$pathdata/$dataset", clear 
capture drop k
capture drop v 
gen outputfirm_obs = exp(q_obs)
gen revenuefirm_obs = exp(r_defl_obs)


* adjust for alternative error
qui: su eps_q
local sigma_orig = r(sd)
if "$depvar" != "outputfirm_obs" { 
	if "$depvar" == "outputfirm_obs_1" { 
		local k = sqrt(1/`sigma_orig')	
	}
	if 	"$depvar" == "outputfirm_obs_2" { 
		local k = sqrt(2/`sigma_orig')
	}
	
	if 	"$depvar" == "outputfirm_obs_4" { 
		local k = sqrt(4/`sigma_orig')
	}
	
	if 	"$depvar" == "outputfirm_obs_8" { 
		local k = sqrt(8/`sigma_orig')
	}
	
	if 	"$depvar" == "outputfirm_obs_16" { 
		local k = sqrt(16/`sigma_orig')
	}
    gen eps_q_new = `k' * eps_q
	replace outputfirm_obs = exp(q + eps_q_new)
} 
	

	
* initial cleaning 
drop if `year' > `max_year' 
drop if `year' < `min_year' 
duplicates drop `year' `firm' , force  //firms that switch fiscal year 
rename `year' year
set more off 

* if firms have no full industry code, deflate by GDP 
xtset `firm' year 


** 5.2 Generate additional variables 
 
* Generate and rename variables for consistency with paper
if "`winsor'" == "1"{
	winsor `capital' , generate(`capital'_w) p(0.01)
	winsor `varcost' , generate(`varcost'_w) p(0.01)
	winsor `sale'    , generate(`sale'_w)    p(0.01)
	gen k = log(`capital'_w)
	gen v = log(`varcost'_w) 
	gen s = log(`sale'_w)
}  
else { 
	gen k = log(`capital')  
	gen v = log(`varcost') 
	gen s = log(`sale')
}


//generate higher order terms 
if "`order'" == "2" | "`order'" == "3"  {
	gen k2 = k^2
	gen v2 = v^2
	gen vk = v*k
}
if "`order'" == "3" {
	gen k3  = k^3
	gen v3  = v^3
	gen v2k = v2*k
	gen vk2 = v*k2	
}

*initialize 
gen o = log(prodfirm)
qui: reg s v k o p  
matrix results = e(b)
matrix init	= results[1,3], results[1,1], results[1,2]
reg  s v k v2 k2 vk o
matrix results = e(b)
matrix init_tl	= results[1,7], results[1,1], results[1,2], results[1,3], results[1,4], results[1,5]



*/ 


************************************************************************************************************************************************************************************
************************************************************************************************************************************************************************************
 

 
***** 6: Estimate production function  ***** 
 

*count number of repetitions 
qui: sum reps
//local indcount = min(r(max),10) 
local indcount = r(max)
*run the loop 
forval i = 0/`indcount' { //rmax is the number of industries in the specified number of industries (can view using the return list command after qui: sum ind 


	** 6.1 Obtain fitted value of sales  
	
	*save original dataset to be retrieved later, then keep only industry of this run of the loop
	preserve 
	keep if reps == `i' 
	
	*estimate regression with variable input and capital as well as all expansions/interactions and year f.e. 
	if "`order'" == "2" {
		qui: reg s v k v2 k2 vk i.year salessharefirm p
	}
	if "`order'" == "3" {
		qui: reg s v k v2 k2 vk v3 k3 v2k vk2 i.year salessharefirm p 
	}


	*obtain fitted values and residuals 
	//gen phi = s 
	predict phi 
	predict epsilon , r
	label var phi "phi_it"
	label var epsilon "measurement error first stage" 
	gen phi_lag = l.phi 
	

	
	** 6.2 Estimate parameters of production function in Mata 
	
	* Generate additional (lagged) variables 
	qui: gen v_lag  		= l.v
	qui: gen k_lag  		= l.k
	qui: gen v_lag2 		= v_lag^2
	qui: gen k_lag2 		= k_lag^2
	qui: gen v_lagk_lag 	= v_lag*k_lag  
	//gen vk			= k*v 		   
	qui: gen v_lagk		= v_lag*k 	  

	* Prep data for Mata; Drop missing observations (otherwise Mata will complain about matrix dimensions) 
	qui: sort `firm' year 
	qui: gen const 		=  1 // constant for regressions in mata later 
	qui: drop if s 		== .
	qui: drop if v_lag 	== .
	qui: drop if k 		== .
	qui: drop if phi 	== .
	qui: drop if phi_lag == .
	
	* Compute markups: DLW (Cobb Douglas)
	/*
	dlw 
	gen beta_c1=beta_dlw[1,1]
	gen beta_v1=beta_dlw[1,2]
	gen beta_k1=beta_dlw[1,3] 
	*/ 
	
	* Compute markups: DLW_TL (Translog) 
	qui: dlw_translog
	gen betac_tl1=beta_dlwtranslog[1,1] //constant
	gen betav_tl1=beta_dlwtranslog[1,2]
	gen betav_tl2=beta_dlwtranslog[1,4]

	gen betak_tl1=beta_dlwtranslog[1,3]
	gen betak_tl2=beta_dlwtranslog[1,5]
	gen betavk_tl=beta_dlwtranslog[1,6]
	

	* Keep one observation per industry with coefficients
	qui: duplicates drop reps  , force
	keep reps beta* 

	*save and restore
	*save $temp/coefficients_ind`i' , replace 
	save temp/coefficients_ind`i' , replace 
	restore 	
} 



*coefficients  
use temp/coefficients_ind0 , clear 
forval i = 1/`indcount' {						 
	append using temp/coefficients_ind`i'
}

*save $temp/coefficients_byind, replace 
if "$depvar" == "outputfirm_obs" { 
	if "$robustness" == "1" {
		save analysisdata/coefficients_byind_dgm_$par, replace
	}
	if "$robustness" == "0" {
		save analysisdata/coefficients_byind_dgm, replace
	}
}
else {
		save analysisdata/coefficients_byind_dgm_$depvar, replace
}

* clean up the coefficient estimates
forval i = 0/`indcount' {	
	erase  temp/coefficients_ind`i'.dta 
}



