*** Create EAP panel 



*** 1. Produce Product Level Dataset 


forval year = 2009/2019 {

	** load data 
	insheet using "rawdata\eap`year'.csv", names clear

	
	** basic cleaning 
	
	* remove observations with empty values
	replace val_ref 		 = .  if  val_ref == 0 
	replace  val_ref_nmoins1 = .  if   val_ref_nmoins1 == 0 
	drop if val_ref == . &  val_ref_nmoins1 == .  
	
	* imputation 
	//drop if c_etat_val == 6 & c_orig_val == 12
	
	* variables removed
	drop  c_milmoins1 raison_sociale c_dep c_ape_cour  c_etat_val   c_cpf45 millesime_ref nat_prodfra_n c_evol_prodfra 
	
	
	* drop non-relevant lines 
	drop if c_variable == "IR1" | c_variable == "IR2" | c_variable == "IR3" | c_variable == "RR1" | c_variable == "RR2" |  c_variable == "RR3" |  c_variable == "VQ2"  |  c_variable == "VQ2"  |  c_variable == "VQ3"  |  c_variable == "VG1" 
	
	* generate year 
	gen year = `year'
	
	** Next assign measurement unit of quantity measured to both the value and the quantity of a production model
	
	* assign production model 
	gen model = .
	replace model = 2 if c_variable == "VS2" |  c_variable == "VQ1B" 
	replace model = 3 if c_variable == "VF1" |  c_variable == "VQ1C"  
	replace model = 4 if c_variable == "VF2" |  c_variable == "VQ1D"  
	replace model = 5 if c_variable == "VF3" |  c_variable == "VQ1E" 
	
	* get numerical value for measurement units in which variables are expressed 
		//this step is needed because values must have same unit as quantity to be considered the same product 
	egen unit = group(c_unite_var_n) 
	gen  unit_q = unit    if c_variable == "VQ1B" |  c_variable == "VQ1C" |  c_variable == "VQ1D"  |  c_variable == "VQ1E" 
	//lag 
	egen unit_lag = group(c_unite_var_nmoins1) 
	gen  unit_lag_q = unit_lag    if c_variable == "VQ1B" |  c_variable == "VQ1C" |  c_variable == "VQ1D"  |  c_variable == "VQ1E" 
	
	
	* assign unit_q to value variables 
	bysort model siren prodfra_n: egen temp = mean(unit_q) 
	replace unit = temp 
	drop unit_q temp 
	bysort model siren prodfra_n_moins1: egen temp = mean(unit_lag_q) 
	replace unit_lag = temp 
	drop unit_lag_q temp 
	
	* define a firm-product as the interaction of a measurement unit and a product code
	gen product8 	 	= substr(prodfra_n,1,8 )
	tostring prodfra_n_moins1 , replace 
	gen product8_lag 	= substr(prodfra_n_moins1,1,8 )
	destring product8*	  , replace 
	egen product 	 	= group(prodfra_n unit )
	egen product_lag 	= group(prodfra_n_moins1 unit_lag) 
	
	* transpose 
	//create new variables 
	gen value_m2  = val_ref if c_variable == "VS2" 
	gen value_m3  = val_ref if c_variable == "VF1" 
	gen value_m4  = val_ref if c_variable == "VF2" 
	gen value_m5  = val_ref if c_variable == "VF3" 
	gen quant_m2  = val_ref if c_variable == "VQ1B" 
	gen quant_m3  = val_ref if c_variable == "VQ1C" 
	gen quant_m4  = val_ref if c_variable == "VQ1D" 
	gen quant_m5  = val_ref if c_variable == "VQ1E" 
	//create lagged variables
	gen value_m2_lag  = val_ref_nmoins1 if c_variable == "VS2" 
	gen value_m3_lag  = val_ref_nmoins1 if c_variable == "VF1" 
	gen value_m4_lag  = val_ref_nmoins1 if c_variable == "VF2" 
	gen value_m5_lag  = val_ref_nmoins1 if c_variable == "VF3" 
	gen quant_m2_lag  = val_ref_nmoins1 if c_variable == "VQ1B" 
	gen quant_m3_lag  = val_ref_nmoins1 if c_variable == "VQ1C" 
	gen quant_m4_lag  = val_ref_nmoins1 if c_variable == "VQ1D" 
	gen quant_m5_lag  = val_ref_nmoins1 if c_variable == "VQ1E" 
	
	//assign values
	foreach var in value_m2 value_m3 value_m4 value_m5 quant_m2 quant_m3 quant_m4 quant_m5    {
		//contemporaneous 
		bysort siren product: egen temp = mean(`var')
		replace `var' = temp 
		drop temp 
		replace `var' = 0 if `var' == . 
		//lagged 
		bysort siren product_lag: egen temp = mean(`var'_lag)
		replace `var'_lag = temp 
		drop temp 
		replace `var'_lag = 0 if `var'_lag == . 
	}
	
	* Clean data
	duplicates drop siren product product_lag product_lag prodfra_n_moins1 , force
	drop c_variable val_ref  prodfra_n model

	* Calculate aggregates
	gen value_m2345 = value_m2 + value_m3 + value_m4 + value_m5
	gen value_m234  = value_m2 + value_m3 + value_m4
	gen value_m345  = value_m3 + value_m4 + value_m5
	gen value_m34   = value_m3 + value_m4 
	gen quant_m2345 = quant_m2 + quant_m3 + quant_m4 + quant_m5
	gen quant_m234  = quant_m2 + quant_m3 + quant_m4
	gen quant_m345  = quant_m3 + quant_m4 + quant_m5
	gen quant_m34   = quant_m3 + quant_m4 
	//lagged
	gen value_m2345_lag = value_m2_lag + value_m3_lag + value_m4_lag + value_m5_lag
	gen value_m234_lag  = value_m2_lag + value_m3_lag + value_m4_lag
	gen value_m345_lag  = value_m3_lag + value_m4_lag + value_m5_lag
	gen value_m34_lag   = value_m3_lag + value_m4_lag 
	gen quant_m2345_lag = quant_m2_lag + quant_m3_lag + quant_m4_lag + quant_m5_lag
	gen quant_m234_lag  = quant_m2_lag + quant_m3_lag + quant_m4_lag
	gen quant_m345_lag  = quant_m3_lag + quant_m4_lag + quant_m5_lag
	gen quant_m34_lag   = quant_m3_lag + quant_m4_lag 
	
	* Price at product level  
	gen price_m2345 = value_m2345 	/ quant_m2345
	gen price_m234 	= value_m234 	/ quant_m234
	gen price_m345 	= value_m345 	/ quant_m345
	gen price_m34 	= value_m34 	/ quant_m34
	//lagged 
	gen price_m2345_lag 	= value_m2345_lag  	/ quant_m2345_lag 
	gen price_m234_lag  	= value_m234_lag  	/ quant_m234_lag 
	gen price_m345_lag  	= value_m345_lag  	/ quant_m345_lag 
	gen price_m34_lag  		= value_m34_lag  	/ quant_m34_lag 
	
	* Product-level index 
	bysort product: egen price_index_m  			=   mean(price_m2345)
	bysort product: egen price_index_wm 			= 	wtmean(price_m2345) , weight(quant_m2345)
	bysort product_lag: egen price_index_lag_m  	=   mean(price_m2345_lag)
	bysort product_lag: egen price_index_lag_wm 	= 	wtmean(price_m2345_lag) , weight(quant_m2345_lag)
	bysort product8: egen price_index8_m  			=   mean(price_m2345)
	bysort product8: egen price_index8_wm 			= 	wtmean(price_m2345) , weight(quant_m2345)
	bysort product8_lag: egen price_index8_lag_m  	=   mean(price_m2345_lag)
	bysort product8_lag: egen price_index8_lag_wm 	= 	wtmean(price_m2345_lag) , weight(quant_m2345_lag)
	
	
	* Create standardized prices 
	foreach price in price_m2345 price_m234  price_m345  price_m34 { 
		gen `price'_std_m 		= `price' / price_index_m 
		gen `price'_std_wm 		= `price' / price_index_wm 
		//lag 
		gen `price'_std_lag_m 	= `price'_lag / price_index_lag_m 
		gen `price'_std_lag_wm 	= `price'_lag / price_index_lag_wm 
		//8 digit 
		gen `price'_std8_m 		= `price' / price_index8_m 
		gen `price'_std8_wm 	= `price' / price_index8_wm 
		//lag 
		gen `price'_std8_lag_m 	= `price'_lag / price_index8_lag_m 
		gen `price'_std8_lag_wm = `price'_lag / price_index8_lag_wm 
	}

	** Save
	drop val_ref_nmoins1 c_unite_var_n c_unite_var_nmoins1 prodfra_n_moins1
	tostring siren ,replace  
	save analysisdata\EAP_productlevel_`year', replace

}


* open first dataset 
use  analysisdata\EAP_productlevel_2009, clear 

* merge with each year  
forval year= 2010/2019 {
	append using  analysisdata\EAP_productlevel_`year'
}

* save 
save analysisdata\EAP_productlevel,replace 






*** 2. Produce Firm Level Dataset 

forval year = 2009/2019 {

	* load data 
	use analysisdata\EAP_productlevel_`year', replace
 
	
	* simple price index at the firm level
	foreach var in _m2345 _m234 _m345 _m34 {
		
		bysort siren: egen value_firm`var' = total(value`var')
		bysort siren: egen quant_firm`var' = total(quant`var')
		gen price_firm`var' = value_firm`var'/quant_firm`var'
		//lag
		bysort siren: egen value_firm`var'_lag = total(value`var'_lag)
		bysort siren: egen quant_firm`var'_lag = total(quant`var'_lag)
		gen price_firm`var'_lag = value_firm`var'_lag/quant_firm`var'_lag
	}

	
	* product-compared price index at the firm level 
	foreach var in _m2345 _m234 _m345 _m34 {
		bysort siren: egen price_firm_std`var'_m_m 			= mean(		price`var'_std_m )
		bysort siren: egen price_firm_std`var'_m_wm 		= wtmean(	price`var'_std_m ) , weight(value`var')
		bysort siren: egen price_firm_std`var'_wm_m 		= mean(		price`var'_std_wm )
		bysort siren: egen price_firm_std`var'_wm_wm 		= wtmean(	price`var'_std_wm ) , weight(value`var')
		//lag 
		bysort siren: egen price_firm_std`var'_lag_m_m 		= mean(		price`var'_std_lag_m )
		bysort siren: egen price_firm_std`var'_lag_m_wm 	= wtmean(	price`var'_std_lag_m ) , weight(value`var'_lag)
		bysort siren: egen price_firm_std`var'_lag_wm_m 	= mean(		price`var'_std_lag_wm )
		bysort siren: egen price_firm_std`var'_lag_wm_wm 	= wtmean(	price`var'_std_lag_wm ) , weight(value`var'_lag)
		//8 digit
		bysort siren: egen price_firm_std8`var'_m_m 		= mean(		price`var'_std8_m )
		bysort siren: egen price_firm_std8`var'_m_wm 		= wtmean(	price`var'_std8_m ) , weight(value`var')
		bysort siren: egen price_firm_std8`var'_wm_m 		= mean(		price`var'_std8_wm )
		bysort siren: egen price_firm_std8`var'_wm_wm 		= wtmean(	price`var'_std8_wm ) , weight(value`var')
		//lag 
		bysort siren: egen price_firm_std8`var'_lag_m_m 	= mean(		price`var'_std8_lag_m )
		bysort siren: egen price_firm_std8`var'_lag_m_wm 	= wtmean(	price`var'_std8_lag_m ) , weight(value`var'_lag)
		bysort siren: egen price_firm_std8`var'_lag_wm_m 	= mean(		price`var'_std8_lag_wm )
		bysort siren: egen price_firm_std8`var'_lag_wm_wm 	= wtmean(	price`var'_std8_lag_wm ) , weight(value`var'_lag)
		
		//the price in Aghion et al. (sales weighted mean in logs == geometric mean)
		*generate log price
		gen l_price`var' = log(price`var')
		*compute the revenue weighted average in logs
		bysort siren: egen l_price_firm_`var'_wm 	= wtmean(	l_price`var' ) , weight( value`var')
		*Transform back to level
		gen price_firm`var'_wm 	= exp(l_price_firm_`var'_wm)
		
	}
	* count obs per firm
	duplicates tag siren, gen(productcount) 
	
	* keep one observation per firm
	duplicates drop siren , force
	keep siren c_naf_rev2 c_ape_lanc  *firm* year productcount
	
	** Save
	save analysisdata\EAP_firmlevel_`year', replace
}


* open first dataset 
use  analysisdata\EAP_firmlevel_2009, clear 

* merge with each year  
forval year= 2010/2019 {
	append using analysisdata\EAP_firmlevel_`year'
}

* save 
save analysisdata\EAP_firmlevel ,replace 



** clean up
forval year= 2009/2019 {
	erase analysisdata\EAP_firmlevel_`year'.dta
}
forval year= 2009/2019 {
	erase analysisdata\EAP_productlevel_`year'.dta
}
erase analysisdata\EAP_productlevel.dta 
