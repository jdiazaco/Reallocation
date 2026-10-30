
* open dataset  
use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

* control for left cut-off
bysort id: egen minyear = min(year)
bysort siren: egen firmmaxyear = max(year)
bysort siren: egen firmminyear = min(year)
gen tenure = year - minyear 
gen tenureflag = (minyear == 2010 | minyear == firmminyear) 
replace tenure = 10 if tenureflag == 1

* calculate average revenue per new product by firms 
xtset id year 
gen revenue_nc_per_nc 			= revenue_nc/n_c 
gen revgrowth_cont 				= (revenue-l.revenue)/(0.5*(revenue+l.revenue)) 
gen revgrowth_cont_nondavis 	= (revenue-l.revenue)/(l.revenue)
gen n_kept = lagN - n_d 
gen n_c_rate = n_c/lagN 

* new-product revenue compared to existing product revenue 
gen log_revenue_nc 		 	= log(revenue_nc)
gen revenue_nc_per_contrev  = (revenue_nc/n_kept)/(0.5*(revenue+l.revenue))  
gen revenue_nc_per_continitrev  = (revenue_nc/(n_kept))/l.revenue  

* take fixed effects 
reghdfe revgrowth_cont 	, resid noabsorb  
predict revgrowth_cont_1, r
reghdfe revgrowth_cont 	, resid absorb(lagN)  
predict revgrowth_cont_2, r
reghdfe revgrowth_cont 	, resid absorb(siren)  
predict revgrowth_cont_3, r
reghdfe revgrowth_cont 	, resid absorb(product)  
predict revgrowth_cont_4, r
reghdfe revgrowth_cont 	, resid absorb(product siren)  
predict revgrowth_cont_5, r
reghdfe revgrowth_cont 	, resid absorb(id)  
predict revgrowth_cont_6, r

* add additional controls 
reghdfe revgrowth_cont 	, resid absorb(age i.nace4_rev2#i.year tenure tenureflag				)  
predict revgrowth_cont_7, r
reghdfe revgrowth_cont 	, resid absorb(lagN age i.nace4_rev2#i.year tenure tenureflag			)  
predict revgrowth_cont_8, r
reghdfe revgrowth_cont 	, resid absorb(siren age i.nace4_rev2#i.year tenure tenureflag			)  
predict revgrowth_cont_9, r
reghdfe revgrowth_cont 	, resid absorb(product age i.nace4_rev2#i.year tenure tenureflag		)  
predict revgrowth_cont_10, r
reghdfe revgrowth_cont 	, resid absorb(product siren age i.nace4_rev2#i.year tenure tenureflag	)  
predict revgrowth_cont_11, r
reghdfe revgrowth_cont 	, resid absorb(id age i.nace4_rev2#i.year tenure tenureflag				)  
predict revgrowth_cont_12, r

* change mean
qui: su revgrowth_cont if n_c == 0 
forval controls = 1/12 {
	replace revgrowth_cont_`controls' =  revgrowth_cont_`controls' + `r(mean)'
} 
gen lag_ln_c = l.ln_c
gen lag_n_c  = l.n_c 



*** produce revenue comparison plot 
winsor log_revenue_nc 				, g(log_revenue_nc_w) 				p(0.01)
winsor revenue_nc_per_contrev 		, g(revenue_nc_per_contrev_w) 		p(0.01) 
winsor revenue_nc_per_continitrev 	, g(revenue_nc_per_continitrev_w) 	p(0.01)     
winsor revgrowth_cont_nondavis 		, g(revgrowth_cont_nondavis_w) 	p(0.01)     

	
// Table

reghdfe revgrowth_cont revenue_nc_per_contrev_w  , noabsorb vce(cluster siren year)
outreg2 using "$path/output/table_c6", replace label noobs nocon sdec(3) bdec(3)
reghdfe revgrowth_cont revenue_nc_per_contrev_w  , absorb(lagN) vce(cluster siren year)
outreg2 using "$path/output/table_c6", append label noobs nocon sdec(3) bdec(3)
reghdfe revgrowth_cont revenue_nc_per_contrev_w  , absorb(lagN product) vce(cluster siren year)
outreg2 using "$path/output/table_c6", append label noobs nocon sdec(3) bdec(3)
reghdfe revgrowth_cont revenue_nc_per_contrev_w  , absorb(lagN product siren) vce(cluster siren year)
outreg2 using "$path/output/table_c6", append label noobs nocon sdec(3) bdec(3)
reghdfe revgrowth_cont revenue_nc_per_contrev_w  , absorb(lagN product siren age tenure tenureflag	) vce(cluster siren year)
outreg2 using "$path/output/table_c6", append label noobs nocon sdec(3) bdec(3)
reghdfe revgrowth_cont revenue_nc_per_contrev_w  , absorb(lagN id age i.nace4_rev2#i.year tenure tenureflag	) vce(cluster siren year) 
outreg2 using "$path/output/table_c6", append label noobs nocon sdec(3) bdec(3) tex(frag)

erase "$path/output/table_c6.txt"

