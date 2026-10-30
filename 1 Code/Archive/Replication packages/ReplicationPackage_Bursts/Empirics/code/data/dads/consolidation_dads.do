
* Consolidation historical groups
**********************************

//  ACCOR is not available for the whole period since it is not a manufacturing firm

* Drop profiled historical groups post 2012
drop if siren == "P12000020"
drop if siren == "P12000019" 
drop if siren == "P12000030"
drop if siren == "P12000022" 
drop if siren == "P12000021"

* Drop profiled historical groups before 2012
drop if siren == "044001113"
drop if siren == "044001121"
drop if siren == "044001014"
drop if siren == "044000917"
drop if siren == "044000925"

* Drop legal units of historical groups
merge m:1 siren using "$path/data/processed/histgroups_contour_2010_2019.dta", keep(1 3)
count if _m == 3
di "drop `r(N)' legal units of historical groups"
drop if _m == 3
drop _m sirus_id



* Consolidation profiled companies
**********************************

* There are 11 profiled companies in the EAP, starting showing from the year 2012:
* P12000001; P12000002; P12000003; P12000004; P12000032; P12000033; 
* P12000034; P12000035; P13000013; P13000017; P13000071

* Drop profiled companies bc is not possible to fully reconstruct their groups
count if substr(siren,1,1)=="P"
di "drop `r(N)' profiled companies"
drop if substr(siren,1,1)=="P"
assert substr(siren,1,1)!="P"

* Drop legal units belonging to these 11 profiled companies
merge m:1 siren using "$path/data/processed/profiledsirens_contour_2010_2019.dta", keep(1 3) // all profiled companies are merged!
count if _m == 3
di "drop `r(N)' legal units of profiled companies"
drop if _m == 3
drop _m sirus_id