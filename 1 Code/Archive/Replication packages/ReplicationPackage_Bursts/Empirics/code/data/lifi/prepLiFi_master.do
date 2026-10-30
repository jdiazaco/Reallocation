/*	============================================================
	This do file creates a panel based on LiFi raw files
	============================================================	*/

/* Creation of the Lifi dataset with the structure of the groups**/

local yr_first = $yr_start // There is break in the lifi data in 2014
local yr_last = $yr_end


forvalues yr=`yr_first'(1)`yr_last' {	

display "Year `yr'"

local extra_var
if `yr' < 2014 {
	use "$path/data/raw/lifi/lar_`yr'.dta", clear
	rename *, lower
	capture rename siren sirlifi
}
else if `yr' >= 2014 {
	use "$path/data/raw/lifi/liaison_reelle_`yr'.dta", clear
	rename *, lower
	rename id_ul sirlifi
	rename id_tg sirtg
	rename taux txcontr
	local extra_var contour 
}
compress
tostring sirlifi sirtg, replace
gen temp = real(substr(sirlifi,1,1))
drop if temp == . // I drop foreign firms, whose id start with a letter (from BvD)
drop temp
replace sirlifi = subinstr(sirlifi, " ", "", .)
replace sirtg = subinstr(sirtg, " ", "", .)
forval i = 1(1)7 { 
	replace sirtg = "0" + sirtg if length(sirtg) < 9
	replace sirlifi = "0" + sirlifi if length(sirlifi) < 9
}
assert length(sirlifi) == 9 

gen year = `yr'

gsort sirlifi txcontr -rpreap sirtg // in case of ties, I first pick the firm controlling more directly and if there remains ties I randomly pick the controlling firm with higher siren number
by sirlifi: keep if _n == _N // for each firm I keep only the investor with the largest stake 

keep year sirtg sirlifi txcontr `extra_var' // natact
// I don't keep natact and I go and pick it from tg because for the firms that are not controlled (<=50%), the variable is set to 0 so we don't know whether the investor is foreign or not
// These are the firms defined as in the contour elargi of a group


compress
if "`yr'" != "`yr_first'" {
	append using "$path/data/processed/LiFi.dta"
}
save "$path/data/processed/LiFi.dta", replace

}

rename sirlifi siren
replace txcontr = txcontr/100 if year < 2014 // It was multiplied by 100 before 2014
gen byte control = txcontr > 50
gen byte extended_group = contour == "CE" if contour != ""
assert extended_group == 1 if control == 0 & extended_group != . // The "contour elargi" or extended group is just when they are not majority owned
assert extended_group == 0 if control == 1 & extended_group != .
drop extended_group contour

sort siren year

by siren (year): gen temp = sirtg != sirtg[_n-1] if _n !=1
by siren: egen temp2 = total(temp)
gen byte no_change_mother = temp2 == 0
drop temp*

sort sirtg year siren
by sirtg year: gen num_firms_group = _N

sort siren year
by siren (year): gen temp = num_firms_group != num_firms_group[_n-1] if _n !=1
by siren: egen temp2 = total(temp)
gen byte no_change_Ngroup = temp2 == 0
drop temp*

gen byte no_change_mother_Ngroup = no_change_mother*no_change_Ngroup

compress
save "$path/data/processed/LiFi.dta", replace

