/*======================================================================
	Cleaning of variables with full dataset
===================================================================== */

*==============================================================================*

/* Cleaning Birth Year */

// replace implausible birthyears
replace birthyear = . if birthyear<1800
replace birthyear = . if birthyear>$yr_end	

// replace multiple birthyears with mode (min in case of tie)
bys siren: egen modebirth = mode(birthyear), minmode
bys siren: egen minyear = min(year)
replace birthyear = modebirth if modebirth!=. & modebirth <= minyear 
bys siren: egen minbirth = min(birthyear) // For the remaining firms I compute the min birthyear (for those that had been already assigned the minmode, the minbirthyear is the same of minmode)
replace birthyear = minyear if minbirth!=. & minbirth > minyear // I use the minyear only if minbirth !=. , meaning only if there is at least one nonmissing birthyear

// replace birthyear to first year it is observed in the dataset if the firm was created the year before
gen diff = minyear - birthyear if minyear > 2008 & birthyear >= 2008
noi tab diff
sum employees_hc if diff ==1, detail // most of them are self-employed and small firms, but look at outliers! 


// gen birthyear legal (as reported by the firm) and economic (as the first year in FARE) 
gen birthyearecon = birthyear
gen year2 = year if employees_hc > 0 & employees_hc !=.
bys siren: egen minyear2 = min(year2)
gen diff2 = minyear2 - birthyear if minyear2 > 2008 & birthyear >= 2008
replace birthyearecon = minyear2 if diff2 == 1 | diff2 == 2
noi tab diff2
gen diff_minyear = minyear2 - minyear
noi tab diff_minyear
drop minyear2 diff_minyear diff2 year2
label var birthyear "Legal birth year"
label var birthyearecon "Economic birth year"
tab year if birthyear == .
replace birthyear = minyear if birthyear == . & minyear!=2008
drop diff minyear modebirth minbirth 



*==============================================================================*

/* Cleaning Exit Year */
// Define exit as last year we observe positive employment

gen year2 = year if employees_hc > 0 & employees_hc != .
bys siren: egen maxyear = max(year2)
tab maxyear
gen exityearecon = .
replace exityearecon = maxyear if maxyear < $yr_end & maxyear != .
tab exityearecon
drop maxyear year2


*==============================================================================*

