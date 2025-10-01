*** table G1, table G2  


use analysisdata/data_smallest_for_python_p37_XTABOND, clear 


** generate variables 

* levels variables 
foreach var in v k m o s p q { 
	gen `var'_level = exp(`var')  
}
gen drop = 0 
xtset firmsId year 
foreach var in s m v k o { 
	replace drop = 1 if `var' ==.  
	replace drop = 1 if l.`var' ==. 
}
drop if drop == 1 

** drop non-manufacturing industries 
drop if naf2d >= 40 | naf2d < 10 

* summarize and produce table 

//initialize matrices 
qui: su s_level ,d 
matrix summary = [round(r(mean)),round(r(sd)),round(r(p50)),round(r(p10)),round(r(p90)),r(N)]

//produce summary statistics 
foreach var in q v k m o   { 
	qui: su `var'_level,d 
	matrix temp = [round(r(mean)),round(r(sd)),round(r(p50)),round(r(p10)),round(r(p90)),r(N)]
	matrix summary = summary \ temp 
}
qui: su p_level ,d 
matrix temp 	= [round(r(mean),.01),round(r(sd),.01),round(r(p50),.01),round(r(p10),.01),round(r(p90),.01),r(N)]
matrix summary 	= summary \ temp 

//assign names to matrix 
matrix rownames summary = "Revenue" "Quantity" "Wage Bill" "Capital" "Purchased Materials" "Purchased Services" "Standardized Price"
matrix colnames summary = "Mean" "St. Dev." "Median" "10th Pct." "90th Pct." "Observations"

//place in excel 
putexcel set output/tableG1, replace 
putexcel A1 = matrix(summary) , names   

 
 ** sector summarize 
 bysort naf2d_num: gen count = _N
 duplicates drop naf2d_num , force 
 keep naf2d_num count 
 export excel using output/tableG2 , replace 






