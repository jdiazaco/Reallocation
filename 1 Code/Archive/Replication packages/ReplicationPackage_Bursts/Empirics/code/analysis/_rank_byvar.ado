cap program drop _rank_byvar
program define _rank_byvar
syntax varlist(min=1 max=1 numeric), by_var(string) // varlist contains the firm identifier

	drop if `by_var' == . // I remove firms that exit
	
	collapse (count) id_count=`varlist', by(`by_var')

	egen dem = total(id_count) 
	gsort -`by_var'
	gen num = sum(id_count)
	sort `by_var'
	gen sh = num/dem
	
	gen l`by_var' = log(`by_var')			
	gen lsh = log(sh)

	** confidentiality
	if ${conf_check} == 1 {		
		gen conf = id_count if id_count <= 2
		// Binning to create non -confidential groups
		gsort -`by_var'
		gen bin = .			
		local tot = _N
		local bin_num = 1
		local bin_tot = 0
		forvalues i =1(1)`tot' {
			if conf[`i'] == . {
				if `bin_tot' > 0 & `bin_tot' < 3 { // we have an issue, I need to add it to a non-confidential groups
					replace bin = `bin_num' if _n == `i'
					replace conf = id_count if _n == `i' & conf == .
					// Since I add a non-confidential bin, the sum is certainly above 3
					local ++bin_num
					local bin_tot = 0
				}
			}
			else if conf[`i'] != . {
				local bin_tot = conf[`i'] + `bin_tot'
				replace bin = `bin_num' if _n == `i'
				if `bin_tot' >= 3 {
					local ++bin_num
					local bin_tot = 0
				}
			}
		}
		bysort bin: egen obs_bin = total(conf) if conf !=.
		assert obs_bin >= 3 if obs_bin != .
		by bin: egen l`by_var'_av = mean(l`by_var') if bin !=.
		by bin: egen lsh_av = mean(lsh) if bin !=.
		sort `by_var'
		
		replace l`by_var'_av = l`by_var' if bin ==. 
		replace lsh_av = lsh  if bin ==. 

		drop obs_bin 		
		
	}
	
	
end