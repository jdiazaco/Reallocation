
* use clean data

use "$path/data/processed/EAPcorrespc10_20102019_OnlyEAP1_firm_clean.dta", clear

* gen revenue per product
gen revenue_N = revenue/N

* Table 1 - summary stats

qui: su catotal , d   
matrix collection 	= [round(r(mean),1), round(r(sd),1) ,round(r(p50),1) ,round(r(p10),1), round(r(p90),1), round(r(p95),1), round(r(p99),1),r(N) ]

foreach var in age employees_fte revenue_N {
	qui: su `var' , d 
	matrix results =  [round(r(mean), 1), round(r(sd),1) ,round(r(p50),1) ,round(r(p10),1), round(r(p90),1), round(r(p95),1), round(r(p99),1), r(N) ]
	matrix collection = collection \ results 
}

foreach var in N n_c n_d n_cont {
	qui: su `var' , d 
	matrix results =  [round(r(mean),0.01), round(r(sd),0.01) ,round(r(p50),0.1) ,round(r(p10),0.1), round(r(p90),1), round(r(p95),1), round(r(p99),1), r(N) ]
	matrix collection = collection \ results 
}



matrix rownames collection = "Sales" "Age" "Employees" "Revenue per Product" "Number of Products" "New Products" "Lost Products" "Continuing Products" 
matrix colnames collection = "Mean" "St. Dev." "Median" "10th Pct." "90th Pct."  "95th Pct." "99th Pct." "Observations"

esttab matrix(collection) using "$path/output/table_1", replace booktabs label nomtitles unstack plain ///
	prehead("\begin{tabular}{l*{6}{r}} \toprule \toprule") posthead("\midrule") prefoot("\bottomrule") substitute("                    &" "\textit{Variable}   &")