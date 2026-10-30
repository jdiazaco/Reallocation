/*	============================================================
	This do file creates concorded PRODFRA 10 digit
	============================================================	*/
	
// This do file is adapted from Bergounhon, Lenoir & Mejean (2018)
// "A Guideline to French Firm-Level Trade Data"

// Define the different paths

global correspath "$path/data/external/concord_pc10/tablescorresp" // here the path to the correspondance tables 
global pc10path "$path/data/external/concord_pc10/nom_pc10" // path to the name of the product categories 
global outpath "$path/data/processed" //  output path



// prep correspondance tables

do "$path/code/data/concord_pc10/input_file_PC_over_time_edited.do"


// define period for the correspondance
local firstyr=$yr_start
local lastyr=$yr_end

local d = `firstyr'+1
local year0 = `firstyr' + 1 
local year1 = `lastyr' - 1 


// Here we import the nomenclature year by year 
// from Insee
cd "$pc10path"

forvalue x = `firstyr'(1)`lastyr' {

		import delimited pc10_`x'.csv, delimiter(",") stringcols(2) clear  
		drop v1
		rename v2 pc
		replace pc = subinstr(pc," ","",.)
		replace pc = subinstr(pc,".","",.)
		g _= length(pc)
		keep if _==10
		drop _
		g year0 = `x'
		g id0 = pc
		keep id0 year0 
		replace id0 = substr(id0,1,10)
		duplicates drop id0, force
		save pc`x', replace 
		
}

clear 
set obs 1 
g year0=1 
drop if year0==1 
save correspondance, replace 


forvalues x= `firstyr'(1)`lastyr' {
	
	use pc`x', clear 
	append using correspondance
	save correspondance, replace
	erase pc`x'.dta

}

// Here we append the annual correspondance tables 

cd "$correspath"
clear
set obs 1
g year1=1 
drop if year1==1
save corres_pc10_apppend, replace 

local year_m1 = `lastyr' - 1

forvalues year = `firstyr'(1)`year_m1' {
		
		local next_year = `year' + 1
		import delimited corres`year'.csv, delimiter(",") varnames(1) clear
		rename prodfra`year' id0
		rename prodfra`next_year' id1 
		g year1=`year' + 1
		g year0=`year'
		tostring id0, replace
		tostring id1, replace

		append using corres_pc10_apppend 
		save corres_pc10_apppend, replace
		erase corres`year'.csv
}

erase corres2009.csv
erase corres2019.csv

// Here we combine nomenclature and correspondance 
// Idea is to get info for products that change and that do not change 

cd "$outpath"

use "$pc10path/correspondance.dta", clear
keep if year0>`year0' - 2 & year0<`year1' + 1
*destring id0, replace
joinby id0 year0 using "$correspath/corres_pc10_apppend.dta", unm(both)
replace year1=year0 + 1 if year1==.
replace id1=id0 if id1==""
save correspondance, replace 
erase "$pc10path/correspondance.dta"


// Define a numerical identifier for each year product code

use correspondance, clear 
keep id0 year0 
rename id0 id 
rename year0 year 
save t1, replace

use correspondance, clear 
keep id1 year1 
rename id1 id 
rename year1 year 
append using t1 

duplicates drop id year, force 
egen num = group(id year) 

sort id year 
save id_num, replace 
erase t1.dta


// Prepare data for Matlab

use id_num, clear 
rename id id1 
rename year year1 
sort id1 year1 
save t1, replace 

use id_num, clear 
rename id id0 
rename year year0 
sort id0 year0 
save t0, replace 

use correspondance, clear 
drop _m
sort id0 year0 
merge m:1 id0 year0 using t0 
drop if _m==2
drop _m 
rename num num0 
sort id1 year1 
merge m:1 id1 year1 using t1 
drop if _m==2
drop _m 
rename num num1 
keep num1 num0 
g edge =1 
save edge, replace 
sum num* 
count
outsheet num0 num1 edge using "$outpath\edge.txt", replace nonames 
erase edge.dta 
erase t0.dta 
erase t1.dta

// Stata replacement for corresp_1.m + networkComponents.m

do "$path/code/data/concord_pc10/corresp_stata.do"

merge 1:1 num using id_num
* shall be a perfect match
drop _m num 
rename id pc10 
 
save "$outpath/corres_pc10_`firstyr'`lastyr'", replace 
sort pc10 year
sum id_conc 

compress id_conc
save "$outpath/corres_pc10_`firstyr'`lastyr'", replace

erase "$outpath/id_num.dta"
erase "$outpath/correspondance.dta"
erase "$correspath/corres_pc10_apppend.dta"
erase "$outpath/edge.txt"