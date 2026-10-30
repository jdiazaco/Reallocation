
foreach laghorizon in 1 5   { 


	*** Prepare data 

	use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

	* identify a firm's first year in the data 
	bysort siren year: egen yearrev = total(revenue)
	gen firmyear = year if yearrev != .  & yearrev  > 0 
	bysort siren: egen minyear = min(firmyear)
	bysort siren: egen maxyear = max(firmyear)

	* attach correct labels to products 
	xtset id year 
	//products that firm will continue to produce 
	gen 	jplus  	= ( revenue != . & f`laghorizon'.revenue != . & year != maxyear) 
	replace jplus 	= . if year == maxyear | revenue == .
	//products that firm continued to produce from last year 
	gen 	jminus  = ( revenue != . & l`laghorizon'.revenue != . & year != minyear)  
	replace jminus 	= . if year == minyear | revenue == .
	//products that the firm is going to lose
	gen 	lcurl   = ( revenue != . & f`laghorizon'.revenue == . & year != maxyear)
	replace lcurl 	= . if year == maxyear | revenue == . 
	//products that are new to the firm 
	gen 	ncurl 	= ( revenue != . & l`laghorizon'.revenue == . & year != minyear)  
	replace ncurl 	= . if year == minyear | revenue == .

	* calculate revenue at time t per group
	foreach var in jplus jminus lcurl ncurl { 
		gen temp  							= revenue*`var' 
		bysort year siren:	egen rev_`var' 	= total(temp) 
		drop temp 
	}

	
	
	* generate EAP revenue
	bysort year siren: egen revenue_EAP = total(revenue)

	* keep one obs per firm-year
	duplicates drop siren year , force 
	capture drop id 
	egen id = group(siren)
	xtset id year
	gen lrevenue_EAP = l`laghorizon'.revenue_EAP
	
	* make panel balanced
	save "$path/data/processed/temp_decomp" , replace 
	duplicates drop id , force 
	keep id 
	expand 10
	bysort id: gen year = _n +2009 
	merge 1:1 id year using "$path/data/processed/temp_decomp"
	erase "$path/data/processed/temp_decomp.dta"
	bysort id: egen exityyear_used = mean(exityearecon)
	bysort id: egen birthyear_used = mean(birthyearecon)

	

	* generate additional variables - haltiwanger decomposition 
	gen halt_revgrowth  	= (revenue_EAP-l`laghorizon'.revenue_EAP) 	/(0.5*(revenue_EAP+l`laghorizon'.revenue_EAP))
	gen halt_revgrowth_loss = (l`laghorizon'.rev_lcurl) 				/(0.5*(revenue_EAP+l`laghorizon'.revenue_EAP))
	gen halt_revgrowth_cont = (rev_jminus-l`laghorizon'.rev_jplus) 		/(0.5*(revenue_EAP+l`laghorizon'.revenue_EAP))
	gen halt_revgrowth_plus = (rev_ncurl) 								/(0.5*(revenue_EAP+l`laghorizon'.revenue_EAP))

	
	* fix haltiwanger variables for firms that have exited
	gen exited  = 1 if   exityyear_used  - year < 0 &  exityyear_used  - year  >= -`laghorizon' 
	gen entered = 1 if   birthyear_used  - year <=0 &  birthyear_used  - year  >  -`laghorizon' 
	replace halt_revgrowth  	= (0-l`laghorizon'.revenue_EAP) 	/(0.5*(0+l`laghorizon'.revenue_EAP)) if exited == 1 
	replace halt_revgrowth_loss = (l`laghorizon'.revenue_EAP) 		/(0.5*(0+l`laghorizon'.revenue_EAP)) if exited == 1 
	replace halt_revgrowth_cont = (0 ) 							    /(0.5*(0+l`laghorizon'.revenue_EAP)) if exited == 1 
	replace halt_revgrowth_plus = (0) 								/(0.5*(0+l`laghorizon'.revenue_EAP)) if exited == 1 
	replace halt_revgrowth  	= (revenue_EAP-0) 					/(0.5*(revenue_EAP+0)) if entered == 1
	replace halt_revgrowth_loss = (0) 								/(0.5*(revenue_EAP+0)) if entered == 1
	replace halt_revgrowth_cont = (0) 								/(0.5*(revenue_EAP+0)) if entered == 1
	replace halt_revgrowth_plus = (revenue_EAP) 					/(0.5*(revenue_EAP+0)) if entered == 1
	
	
	** haltiwanger decomposition 
	
	* shapley  - log 
	reg halt_revgrowth halt_revgrowth_plus halt_revgrowth_loss halt_revgrowth_cont 
	shapley2, stat(r2) 
	matrix temp = e(shapley) 
	matrix results`laghorizon'_halt= temp[1,1]\temp[2,1]\temp[3,1]

	* produce summary statistics for each of the terms 
	gen den = 0.5*(revenue_EAP+l`laghorizon'.revenue_EAP)
	replace halt_revgrowth_loss = -halt_revgrowth_loss
	capture matrix drop ss`laghorizon'
	foreach var in halt_revgrowth halt_revgrowth_plus halt_revgrowth_loss halt_revgrowth_cont {
		qui: su `var' 	[aweight=den]		, d
		matrix temp  			=  [r(mean)]
		matrix ss`laghorizon' 	=  nullmat(ss`laghorizon') , temp  
	}	
	
	
	
	** Hottman et al. (2016) variance decomposition 

	gen halt_revgrowth_loss_minus = halt_revgrowth_loss

	// no fixed effects 
	reg halt_revgrowth_plus 	halt_revgrowth, 
	matrix results= e(b)
	matrix errors = e(V)
	matrix results = results[1,1]
	matrix errors = sqrt(errors[1,1])
	reg halt_revgrowth_loss_minus 		halt_revgrowth, 
	matrix result= e(b)
	matrix error = e(V)	
	matrix results= results \ result[1,1]
	matrix errors = errors  \ sqrt(error[1,1])
	reg halt_revgrowth_cont 		halt_revgrowth, 
	matrix result= e(b)
	matrix error = e(V)	
	matrix results= results \ result[1,1]
	matrix errors = errors  \ sqrt(error[1,1])
	
	* save results 
	matrix results_decomp_lag`laghorizon' = results 
	matrix results_decomp_lag`laghorizon'_error = errors 

}

** export shapley-owen 
matrix results1_halt = results1_halt * 100
matrix results5_halt = results5_halt * 100
matrix results_halt = [ss1 \ ss5 \ 100, results1_halt' \ 100, results5_halt']

matrix colnames results_halt = "Overall Growth" "Product Innovation" "Product Destruction" "Continuing Products"
matrix rownames results_halt = "1-year" "5-year" "1-year-s" "5-year-s" 

esttab matrix(results_halt, fmt("3 3 1 1")) using "$path/output/table_3", replace booktabs label nomtitles unstack plain ///
	prehead("\begin{tabular}{l*{4}{c}} \toprule \toprule") posthead("\midrule") prefoot("\bottomrule") ///
	substitute("\midrule" "\midrule \textit{Mean value}  & & & &\\" "1-year-s" "\addlinespace \textit{Shapley-Owen Contribution } & & & & \\ 1-year" "5-year-s" "5-year")











