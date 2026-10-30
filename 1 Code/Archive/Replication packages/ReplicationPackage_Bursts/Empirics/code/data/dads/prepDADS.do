/*	============================================================
	This do file creates a panel based on DADS postes raw files
	============================================================	*/

// create file list sirens from EAP

use "$path/data/processed/EAP_list_sirenyear.dta", clear
bys siren: keep if _n == 1
keep siren
tempfile listsiren_eap
save `listsiren_eap'


// clean raw dads data
quietly {
foreach year of numlist $yr_start/$yr_end  { 
	
	clear
	fs $path/data/raw/dads/`year'/post*.dta 
	
	local num_files: word count local `r(files)'
	noi di `num_files'-1 " files found in `year'" // It counts an additional word

	local i = 0

	foreach f in `r(files)' {
		
		local ++i
		
		noi di "Using file `f' in year `year'"
		use $path/data/raw/dads/`year'/`f', clear
		
		foreach v of varlist _all {
			cap rename `v', lower
		}
		
		order siren nic pcs s_brut typ_emploi 
		
		/*
		- The filt variable identify postes as 'non-annexes' (1), 'annexes' (2), and indemnités chômage (3)
		  We drop observations wih filt==3 or filt==0/"" (missing) 
		*/
		tostring filt, replace
		drop if filt=="3" |  filt=="0" 
		drop filt

		/*
		- Region files report both workers that were resident in the region X (in t or t-1) and those whose employer was located in region X (in t or in t-1).
		  In order to avoid duplicates, we only keep workers that are recorded as working in region X in t. 
		*/
		cap rename reg regt
		if `year'<=2017 {
			egen mode_regt=mode(regt)
			keep if mode_regt==regt
			drop mode_regt
		}		

		compress
		
		if `i' > 1 {
			append using "$path/data/processed/DADSPostes_`year'.dta"
		} 
		save "$path/data/processed/DADSPostes_`year'.dta", replace
		erase $path/data/raw/dads/`year'/`f' 
	}	
	
	// Num Plants
	use "$path/data/processed/DADSPostes_`year'.dta", clear
	keep siren nic
	duplicates drop
	gen nic1d = substr(nic, 1,1)
	drop if nic1d == "F"
	drop if nic1d == "9"
	gen index = 1
	bys siren nic: egen jobspells = total(index)
	drop index
	duplicates drop siren nic jobspells, force
	gen year = `year'
	compress
	save "$path/data/processed/DADSPostes_nplants_`year'.dta", replace
	
	
	// R&D
	use "$path/data/processed/DADSPostes_`year'.dta", clear

	* keep only firms in EAP
	merge m:1 siren using `listsiren_eap', keep(3) nogen
	
	* keep ordinary workers (exclude stagiare and appretenship)
	replace typ_emploi="O" if typ_emploi==""
	rename typ_emploi typ_emploi2
	gen typ_emploi=1 if typ_emploi2=="O"
	replace typ_emploi=2 if typ_emploi2=="A"
	replace typ_emploi=3 if typ_emploi2=="S"
	label define typ_emploilabel 1 "Ordinair" 2 "Apprenti" 3 "Stagiair" 
	label values typ_emploi typ_emploilabel
	cap drop typ_emploi2 	
	keep if typ_emploi == 1
	
	bysort ident_s siren nic: gen sum=_N
	count if sum>1
	local num = r(N)
	if `num'>0 {
		preserve
		keep if sum>1
		noi di "Eliminating duplicates in year `year'"
		noi duplicates drop  siren nic ident_s datdeb datfin nbheur cpfd s_brut, force
		tempfile tempdupl
		save `tempdupl'
		restore 

		keep if sum==1
		append using `tempdupl'
	}
	drop sum cpfd	
	
	* keep vars of interest
	keep siren s_brut pcs 

	* gen R&D worker
	gen pcs2d = substr(pcs,1,2)
	gen rd = 0
	replace rd = 1 if pcs2d == "38"
	
	foreach var in s_brut {
		gen `var'_rd = `var' if rd == 1
	}
	
	gcollapse (sum) *_rd , by(siren) fast
	
	gen year = `year'
	compress
	save "$path/data/processed/temp_rd`year'.dta", replace
	
	erase "$path/data/processed/DADSPostes_`year'.dta"
}

// append all years R&D
clear
forvalues year = $yr_start/$yr_end {

	append using "$path/data/processed/temp_rd`year'.dta"
	erase "$path/data/processed/temp_rd`year'.dta" 
}

order year siren
sort year siren
compress
save "$path/data/processed/dads_rd_${yr_start}${yr_end}.dta", replace

// append all years N plants

clear
foreach year of numlist $yr_start/$yr_end {
	append using "$path/data/processed/DADSPostes_nplants_`year'.dta"
	erase "$path/data/processed/DADSPostes_nplants_`year'.dta"
}

compress

* Consolidation
***************
// consistent with EAP
do "$path/code/data/dads/consolidation_dads.do"

// Cleaning
gen length_siren=length(siren)
noi di "Dropping wrong sirens"
noi drop if length_siren < 9 // Just one observation in 2017
noi drop if substr(siren, 1, 1) == "F" // Just one observation in 2013
drop length_siren

sort siren year nic
by siren year: gen temp = _N
noi di "Years with missing plant codes" // Some issues in 2013 and 2019
noi tab year if nic == ""
noi di "Dropping missing plant codes of multi-plant firms"
drop if temp > 1 & nic == ""
replace nic = "00000" if nic == "" & temp == 1
assert nic != ""
drop temp 

save "$path/data/processed/dads_noplants.dta", replace	

// gen firm's entry
bys siren : egen entrytemp = min(year)

* gen firms' exit
bys siren : egen exittemp = max(year)


// create a full matrix for identifying plant's entry/exit
	
egen id = group(siren nic)
xtset id year
tsfill, full
bysort id: carryforward siren, gen(firm2)
gsort id - year
bysort id: carryforward firm2, gen(firmfinal)
drop siren firm2
rename firmfinal siren
sort id (year)
bysort id: carryforward nic, gen(nic2)
gsort id - year
bysort id: carryforward nic2, gen(nicfinal)
drop nic nic2
rename nicfinal nic
sort id (year)
order id year siren nic
	

// gen number of new plants

* gen firm's number of plants
gen index = 0
replace index = 1 if jobspells != .
bys siren year : egen N_plants = total(index)

* gen firm's number of new plants
bys id (year): gen index_new = 1 if index == 1 & l1.index == 0
replace index_new = . if index_new == 1 & entrytemp == year
bys siren year : egen new_plants = total(index_new)

* gen firm's number of lost plants
bys id (year): gen index_des = 1 if l1.index == 1 & index == 0
replace index_des = . if index_des == 1 & exittemp == year
bys siren year : egen lost_plants = total(index_des)


gcollapse (first) N_plants new_plants lost_plants , by(year siren) fast

drop if N_plants == 0

compress
save "$path/data/processed/dads_noplants.dta", replace	

}

