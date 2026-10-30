
/*	=====================================================================
Variable transformation
(must be careful about when things are set to missing or zero)
===================================================================== */
args treatedYear

di "Var transformation for year `treatedYear'"


// Add a year variable to each dataset
generate year = `treatedYear'

// Composite variables
egen output = rowtotal(catotal /*autprex*/ prodsto prodimm) if( !missing(catotal)) 			// set output to missing if catotal is missing	
/* AUTPREX: Autres produits d'exploitation (dont reprises sur amortissements et provisions, transferts de charges) 
			There are parts of this variable that would be good to keep (e.g. concessions de licences d’exploitation et de procédés de fabrication industriels: brevets, marques etc.) 
			But overall we exclude it because parts such as reprises sur amortissements might be very problematic.
			Moreover the definition of VAHT (VA excluding taxes) computed in FICUS excludes it (as well as AUTCHEX below).
*/

egen intermediates = rowtotal(achamar achampr autacha /*autchex*/ varstma varstmp) if( !missing(achamar) | !missing(achampr) | !missing(autacha)), missing	// set to missing if all vars are missing or if none of the main variables are different from missing
/* AUTCHEX: Autres charges d'exploitation. We exclude it because not included in the definition of VA computed in FICUS. And also because it contains parts that we don't want such as jetons de presence that are more labour costs. */

gen labourcosts = saltrai + charsoc 									// set to missing if either is missing

egen capital = rowtotal(immocor immoinc), missing 						// set to missing if all vars are missing



// Renames
if `treatedYear' == 2008 {
	rename eff_3112 employees_hc
	rename eff_etp 	employees_fte
	rename effsalm  empl_fte_2008 // added to deal with missing employment of entrants in 2008		
}
else {
	rename effsalm_3112 employees_hc
	rename effsalm      employees_fte
	generate empl_fte_2008 = . // added to deal with missing employment of entrants in 2008		
}


// generate value added  or use one from data
generate valueadded = output - intermediates // This should correspond to VAHT as computed in FICUS. This should correspond to VA at producer's prices.


// Birth Year
tempvar datcrsub
gen `datcrsub' = substr(datcr, 1, 4)
destring `datcrsub', generate(birthyear) force // dates that cannot be destringed become missing

	

// Construct a firm id
// if $deleteEmptySIREN, then observations with empty SIREN are deleted, if not, they are filled with ifrp
// this applies when no subsampling is used
count if siren == ""
di "`r(n)' obs with missing siren"
drop if siren == ""


// keep vars needed for the analysis
keep year siren ape birthyear employees_hc employees_fte ///
	 catotal output intermediates labourcosts capital valueadded immoinc


