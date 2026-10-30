
* open dataset  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

* control for left cut-off
bysort id: egen minyear = min(year)
bysort siren: egen firmmaxyear = max(year)
bysort siren: egen firmminyear = min(year)
gen tenure = year - minyear 
replace tenure = . if minyear == $yr_start
replace tenure = . if minyear == firmminyear

* calculate average revenue per new product by firms 
xtset id year 
gen revenue_nc_per_nc = revenue_nc/n_c 
gen revgrowth_cont = (revenue-l.revenue)/(0.5*(revenue+l.revenue))

* firm-level 
preserve
	duplicates drop siren year, force 
	egen siren2 = group(siren)
	bysort siren2 year: egen revgrowth_cont_mean = mean(revgrowth_cont)
	bysort siren2 year: egen revtot = total(revenue)
	xtset siren2 year 
	gen lag_revenue_nc_per_nc = l.revenue_nc_per_nc
	gen lag_revgrowth_cont_mean = l.revgrowth_cont_mean 
	gen lag_revtot = l.revtot 
	gsort siren2 year  
	bysort siren2: carryforward lag_revenue_nc_per_nc, replace 
	keep siren year revenue_nc_per_nc lag_revenue_nc_per_nc lag_revgrowth_cont_mean lag_revtot
	save $path/data/processed/temp, replace 
restore 
merge m:1 siren year using  $path/data/processed/temp , nogen 
erase $path/data/processed/temp.dta 

* flag added products 
xtset id year 
gen newprod 	= 1 if year == minyear 		& year != firmminyear
replace newprod = 0 if year != minyear  	& year != firmminyear

* regression 
gen log_revenue = log(revenue) 
gen log_lag_revenue_nc_per_nc = log(lag_revenue_nc_per_nc)
winsor log_lag_revenue_nc_per_nc, g(log_lag_revenue_nc_per_nc_w) p(0.01)
winsor log_revenue , g(log_revenue_w) p(0.01)


* dependence on size etc
keep if age !=. & n_c != . &  nace4_rev2 !=.  

	
* binscatters

binscatter  log_revenue_w log_lag_revenue_nc_per_nc_w if newprod==1 , absorb(product) mcolor(blue) lcolor(gs10)  /// 
	xlabel(2.3025851  "10{superscript:1}"   4.6051702 "10{superscript:2}"   6.9077553 "10{superscript:3}"   9.2103404 "10{superscript:4}"   , labsize(medium)     ) /// 
	ylabel(2.3025851  "10{superscript:1}"   4.6051702 "10{superscript:2}"   6.9077553 "10{superscript:3}"   9.2103404 "10{superscript:4}"   , labsize(medium)     ) /// 
	ytitle("Revenue on New Product", size(medlarge))									///	
	xtitle("Revenue on New Products (avg), Previous Burst", size(medlarge))				///	
	nquantiles(30)
graph export "$path/output/figure_7a.pdf", replace

reghdfe log_revenue_w if newprod== 1, absorb( siren) resid 
predict log_revenue_w_fit, r  
reghdfe log_lag_revenue_nc_per_nc_w if newprod== 1, absorb( siren) resid
predict log_lag_revenue_nc_per_nc_w_fit, r 
qui: su log_revenue_w
replace log_revenue_w_fit = log_revenue_w_fit + r(mean)
qui: su log_lag_revenue_nc_per_nc_w
replace log_lag_revenue_nc_per_nc_w_fit = log_lag_revenue_nc_per_nc_w_fit + r(mean)
binscatter  log_revenue_w_fit log_lag_revenue_nc_per_nc_w_fit if newprod==1 , absorb(product) mcolor(blue) lcolor(gs10)  /// 
	xlabel(2.3025851  "10{superscript:1}"   4.6051702 "10{superscript:2}"   6.9077553 "10{superscript:3}"   9.2103404 "10{superscript:4}"   , labsize(medium)     ) /// 
	ylabel(2.3025851  "10{superscript:1}"   4.6051702 "10{superscript:2}"   6.9077553 "10{superscript:3}"   9.2103404 "10{superscript:4}"   , labsize(medium)     ) ///
	ytitle("Revenue on New Product", size(medlarge))									///	
	xtitle("Revenue on New Products (avg), Previous Burst", size(medlarge))				///	
	nquantiles(30)
graph export "$path/output/figure_7b.pdf", replace

