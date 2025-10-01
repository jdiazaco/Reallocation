*** estimate production function with DGM and ACF method 


***** 1. Preamble ******

* clean up 
clear  
clear mata 
set matsize 11000
global dataset analysisdata\data_smallest_for_python_p37_XTABOND
mata: mata clear
mata: mata set matastrict off


***** 2: Select Variables *****

	*variable for firm identifiers
	local firm firmsId

	*variable for year
	local year year 
	  


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
		*local min_year 2009
		*local max_year 2019

		
	 
	 
	***** 4: Define Mata Progam (based on De Loecker & Warzynski)  *****



	mata 

		** Section 4.1: Cobb Douglas
		void GMM_DLW(todo,betas,crit,g,H)
		{
			//import fitted value of production function to be estimated in Section 6 	
			PHI 	= st_data(.,("phi")) 												
			PHI_LAG = st_data(.,("phi_lag"))
		
			//import the matrix of production factors to be uncorrelated with productivity shocks in the GMM moment condition 
			Z= st_data(.,("const","v","k","m_lag", "o_lag"))

			//specify the vector of variables in the production function that is estimated as part of the GMM estimation (does not have to equal variables used to estimate phi!) 
			X 		= st_data(.,("const","v","k","m", "o"))
			X_lag	= st_data(.,("const","v_lag","k_lag","m_lag", "o_lag"))
		
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
			optimize_init_conv_nrtol(S, 0.00001 )
			optimize_init_conv_maxiter(S,1000)
			
			if ("`est_k'" == "1") {
				b = st_matrix("beta_ols_cd")
				optimize_init_params(S, b)				
			} 
			
			qui: p = optimize(S) 							
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
			Z= st_data(.,("const","v","k","m_lag", "o_lag", "v2", "k2", "m_lag2", "o_lag2", "m_lagv", "m_lago_lag", "m_lagk", "vo_lag", "vk", "o_lagk"))
			

			//specify the vector of variables in the production function that is estimated as part of the GMM estimation (does not have to equal variables used to estimate phi!) 
			X 		= st_data(.,("const","v","k","m", "o", "v2", "k2", "m2", "o2", "mv", "mo", "mk", "vo", "vk", "ok"))
			
			X_lag	= st_data(.,("const","v_lag","k_lag","m_lag", "o_lag", "v_lag2", "k_lag2", "m_lag2", "o_lag2", "m_lagv_lag", "m_lago_lag", "m_lagk_lag", "v_lago_lag", "v_lagk_lag", "o_lagk_lag"))

			
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
			optimize_init_which(S, "min")
			
			optimize_init_conv_maxiter(S,1000)
			
			b = st_matrix("beta_ols_tl")
			optimize_init_params(S, b)
			
			qui: p = optimize(S)
			p
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

	 

	 
foreach depvar in outputfirm_obs revenuefirm_obs {

	*variable for sales (revenue) 
	local sale `depvar'
	 

	 
	***** 5: Open Dataset, Generate Variables and Perform Cleaning  *****


	** 5.1 Open dataset and perform cleaning 

	* open dataset 
	use "$dataset", clear 

	gen outputfirm_obs = exp(q)
	gen revenuefirm_obs = exp(s)

	*Choose quantity	
	if "`depvar'" == "outputfirm_obs" { 
		drop s
		gen s = q
	}
	
			
			
		
	* initial cleaning 
	set more off 

	* tsset 
	xtset `firm' year 


	** 5.2 Generate additional variables 
	 

	//generate higher order terms 
	if "`order'" == "2" | "`order'" == "3"  {
		* Generate variable
		qui: gen v2 		= v^2
		qui: gen k2 		= k^2
		qui: gen m2 		= m^2
		qui: gen o2 		= o^2
		
		qui: gen mv 	= m*v  
		qui: gen mo	    = m*o
		qui: gen mk 	= m*k
		qui: gen vo 	= v*o
		qui: gen vk 	= v*k
		qui: gen ok 	= o*k
	}
	if "`order'" == "3" {
		qui: gen v3 		= v^3
		qui: gen k3 		= k^3
		qui: gen m3 		= m^3
		qui: gen o3 		= o^3
		
		qui: gen mv2 	= m*v2 
		qui: gen m2v 	= m2*v 
		
		qui: gen mo2	= m*o2
		qui: gen m2o	= m2*o
		
		qui: gen mk2 	= m*k2
		qui: gen m2k 	= m2*k
		
		qui: gen vo2 	= v*o2
		qui: gen v2o 	= v2*o
		
		qui: gen vk2 	= v*k2
		qui: gen v2k 	= v2*k
		
		qui: gen ok2 	= o*k2
		qui: gen o2k 	= o2*k
		
		qui: gen mvk 	= m*v*k
		qui: gen mvo 	= m*v*o
		qui: gen mko 	= m*k*o
		
		qui: gen kvo 	= k*v*o
		
		
		
	}







	************************************************************************************************************************************************************************************
	************************************************************************************************************************************************************************************
	 

	 
	***** 6: Estimate production function  ***** 
	 

	*count number of industry 
	qui: sum naf2d_num
	//local indcount = min(r(max),10) 
	local indcount = r(max)
	*run the loop 
	foreach i in 08 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95 { //rmax is the number of industries in the specified number of industries (can view using the return list command after qui: sum ind 




		** 6.1 Obtain fitted value of sales  
		
		*save original dataset to be retrieved later, then keep only industry of this run of the loop
		preserve 
			keep if naf2d_num == `i' 
			
			*initial guess
				**CD
				qui: reg s v k m o   
				matrix results = e(b)
				matrix beta_ols_cd	= results[1,5], results[1,1], results[1,2], results[1,3], results[1,4]
				
				**TL
				qui: reg s v k m o v2 k2 m2 o2 mv mo mk vo vk ok  
				matrix results = e(b)
				matrix beta_ols_tl	= results[1,15],  results[1,1], results[1,2], results[1,3], results[1,4] , results[1,5] , results[1,6], results[1,7], results[1,8], results[1,9], results[1,10], results[1,11], results[1,12], results[1,13], results[1,14]

			
			*estimate regression with variable input and capital as well as all expansions/interactions and year f.e. 
			if "`depvar'" == "outputfirm_obs" { 
				if "`order'" == "2" {
					qui: reg s v k m o v2 k2 m2 o2 mv mo mk vo vk ok i.year ms5d p
				}
				if "`order'" == "3" {
					qui: reg s v k m o v2 k2 m2 o2 mv mo mk vo vk ok v3 k3 m3 o3 mv2 m2v mo2 m2o mk2 m2k vo2 v2o vk2 v2k ok2 o2k mvk mvo mko kvo i.year ms5d p 
				}
			}
			if "`depvar'" == "revenuefirm_obs" {
				if "`order'" == "2" {
					qui: reg s v k m o v2 k2 m2 o2 mv mo mk vo vk ok i.year ms5d 
				}
				if "`order'" == "3" {
					qui: reg s v k m o v2 k2 m2 o2 mv mo mk vo vk ok v3 k3 m3 o3 mv2 m2v mo2 m2o mk2 m2k vo2 v2o vk2 v2k ok2 o2k mvk mvo mko kvo i.year ms5d  
				}
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
			qui: gen m_lag  		= l.m
			qui: gen o_lag  		= l.o
			
			qui: gen v_lag2 		= v_lag^2
			qui: gen k_lag2 		= k_lag^2
			qui: gen m_lag2 		= m_lag^2
			qui: gen o_lag2 		= o_lag^2
			
			qui: gen m_lagv_lag 	= m_lag*v_lag  
			qui: gen m_lago_lag 	= m_lag*o_lag  
			qui: gen m_lagk_lag 	= m_lag*k_lag  
			qui: gen v_lago_lag 	= v_lag*o_lag  
			qui: gen v_lagk_lag 	= v_lag*k_lag  
			qui: gen o_lagk_lag 	= o_lag*k_lag  
			
			* Generate additional instrument variables 
			qui: gen m_lagv 	= m_lag*v  
			*qui: gen m_lago_lag 	= m_lag*o_lag  
			qui: gen m_lagk 	= m_lag*k  
			qui: gen vo_lag 	= v*o_lag  
			*qui: gen vk 	= v*k  
			qui: gen o_lagk 	= o_lag*k 
			

			* Prep data for Mata; Drop missing observations (otherwise Mata will complain about matrix dimensions) 
			qui: sort `firm' year 
			qui: gen const 		=  1 // constant for regressions in mata later 
			qui: drop if s 		== .
			qui: drop if v_lag 	== .
			qui: drop if k 		== .
			qui: drop if phi 	== .
			qui: drop if phi_lag == .
			
			* Compute markups: DLW (Cobb Douglas)
			*X 		= st_data(.,("const","v","k","m", "o"))
			dlw 
			gen beta_const_cd=beta_dlw[1,1]
			gen beta_v_cd=beta_dlw[1,2]
			gen beta_k_cd=beta_dlw[1,3] 
			gen beta_m_cd=beta_dlw[1,4] 
			gen beta_o_cd=beta_dlw[1,5] 
			
			
			* Compute markups: DLW_TL (Translog) 
			*qui: dlw_translog
			*X 		= st_data(.,("const","v","k","m", "o", "v2", "k2", "m2", "o2", "mv", "mo", "mk", "vo", "vk", "ok"))
			dlw_translog
			gen beta_const_tl=beta_dlwtranslog[1,1] //constant
			gen beta_v_tl=beta_dlwtranslog[1,2]
			gen beta_k_tl=beta_dlwtranslog[1,3]
			gen beta_m_tl=beta_dlwtranslog[1,4]
			gen beta_o_tl=beta_dlwtranslog[1,5]
			
			gen beta_v2_tl=beta_dlwtranslog[1,6]
			gen beta_k2_tl=beta_dlwtranslog[1,7]
			gen beta_m2_tl=beta_dlwtranslog[1,8]
			gen beta_o2_tl=beta_dlwtranslog[1,9]
			
			gen beta_mv_tl=beta_dlwtranslog[1,10]
			gen beta_mo_tl=beta_dlwtranslog[1,11]
			gen beta_mk_tl=beta_dlwtranslog[1,12]
			gen beta_vo_tl=beta_dlwtranslog[1,13]
			gen beta_vk_tl=beta_dlwtranslog[1,14]
			gen beta_ok_tl=beta_dlwtranslog[1,15]
					
			

			* Keep one observation per industry with coefficients
			qui: duplicates drop naf2d_num  , force
			keep naf2d_num beta* 

			*save and restore
			*save $temp/coefficients_ind`i' , replace 
			save temp/coefficients_ind`i' , replace 
		restore 	
	} 


	*combine and save coefficients   
	use temp/coefficients_ind08 , clear 
	foreach i in 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95 {							 
		append using temp/coefficients_ind`i'
	}
	save analysisdata/coefficients_byind_dgm_`depvar' , replace 
	 
	 
	* clean up the coefficient estimates
	foreach i in 08 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95 {	
		erase  temp/coefficients_ind`i'.dta 
	}
}

