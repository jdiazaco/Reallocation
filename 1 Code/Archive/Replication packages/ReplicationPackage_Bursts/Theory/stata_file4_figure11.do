
local nmc = real("$mc_draws")
if missing(`nmc') | `nmc' <= 0 | floor(`nmc') != `nmc' {
	di as err "Invalid global mc_draws: $mc_draws"
	exit 198
}

*** PART 1: Prepare data for each draw (main + kk)
foreach modelname in main kk {
	if "`modelname'" == "main" {
		local dataset "$input"
	}
	else {
		local dataset "${input}_kk"
	}

	forvalues mc = 1/ `nmc' {
		local suffix : di %02.0f `mc'
		local src "`dataset'/simulation_productlevel_`suffix'.csv"
		capture confirm file "`src'"
		if _rc {
			di as err "Missing required Monte Carlo file: `src'"
			exit 601
		}

		import delimited "`src'", clear

		* make variable names data consistent
		gen siren = firmid2
		gen id    = prodindex
		rename nprod nProd
		rename numprodadded numProdAdded

		* firm entry/exit in EAP
		bys siren: egen entrytemp = min(year)
		bys siren: egen exittemp = max(year)

		* create full panel for product IDs
		xtset id year
		tsfill, full
		bysort id: carryforward siren, gen(firm2)
		gsort id - year
		bysort id: carryforward firm2, gen(firmfinal)
		drop siren firm2
		rename firmfinal siren
		sort id (year)
		order id year siren

		* firm number of products
		gen index = 0
		replace index = 1 if revenue != .
		bys siren year: egen N = total(index)

		* number of new products
		bys id (year): gen index_new = 1 if index == 1 & l1.index == 0
		replace index_new = . if index_new == 1 & entrytemp == year
		bys siren year: egen n_c = total(index_new)

		* number of lost products
		bys id (year): gen index_des = 1 if l1.index == 1 & index == 0
		bys siren year: egen n_d = total(index_des)

		* revenue by new products
		gen temp_revenue_nc = revenue if index_new == 1
		bys year siren: egen revenue_nc = total(temp_revenue_nc)

		* revenue in year t-1 by lost products
		xtset id year
		gen temp_revenue_nd = l1.revenue if index_des == 1
		bys year siren: egen revenue_nd = total(temp_revenue_nd)

		* revenue continuing products
		bys id (year): gen index_cont = 1 if index_new == . & index_des == . & revenue != .
		replace index_cont = . if index_cont == 1 & entrytemp == year
		gen temp_revenue_cont = revenue if index_cont == 1
		bys year siren: egen revenue_cont = total(temp_revenue_cont)

		* collapse at year-firm level
		drop index index_new index_des temp_revenue_nc temp_revenue_nd index_cont temp_revenue_cont id
		collapse (sum) revenue (first) N n_c n_d revenue_nc revenue_nd revenue_cont numProdAdded (max) entrytemp exittemp, by(year siren) fast

		egen id = group(siren)
		order id
		xtset id year

		* active firm filtering
		replace revenue = . if revenue == 0
		drop if revenue == . & n_d == 0
		foreach var in N n_c revenue_nc revenue_cont {
			replace `var' = . if revenue == .
		}

		* initial year/entry handling
		foreach var in n_c n_d revenue_nc revenue_nd revenue_cont {
			replace `var' = . if year == $year_start
			replace `var' = . if year == entrytemp & `var' == 0
		}
		drop entrytemp exittemp

		* lags and logs
		foreach var in n_c n_d N {
			gen lag`var'  = l1.`var' if l1.`var' != .
			gen l`var'    = log(`var') if `var' != .
			gen lagl`var' = l1.l`var' if l1.l`var' != .
		}

		compress
		save "$output/nc_nd_temp_mc_`modelname'_`suffix'.dta", replace
	}
}

*** PART 2: Plot including Klette-Kortum and data
import delimited "${do}\plot_coordinates\figure_rank_new_prod_lagNAll_datapoints.csv", clear
save "$output/figure_rank_new_prod_lagNAll_datapoints", replace

local index = 1
foreach modelname in main kk {
	tempfile stack_curve
	forvalues mc = 1/1  { //only plot single draw, otherwise artificially many obs in far right tail (alternatively: drop randomly in prop. to draw freq., line 164)
		local suffix : di %02.0f `mc'
		use "$output/nc_nd_temp_mc_`modelname'_`suffix'.dta", clear

		drop if n_c == .
		gen sh = .
		levelsof n_c, local(levels)
		foreach n of local levels {
			count if n_c >= `n' & n_c != .
			local num = r(N)
			count
			local den = r(N)
			local nrow = `n' + 1
			if _N < `nrow' {
				set obs `nrow'
			}
			replace sh = `num'/`den' in `nrow' if `nrow'
		}

		gen n_nc = _n - 1
		replace n_nc = . if sh == .
		gen lsh = log(sh)
		gen ln_nc = log(n_nc)
		keep sh lsh n_nc ln_nc
		keep if ln_nc != .
		bys ln_nc: keep if _n == 1
		gen mc = `mc'

		if `mc' == 1 {
			save `stack_curve', replace
		}
		else {
			append using `stack_curve'
			save `stack_curve', replace
		}
	}

	use `stack_curve', clear
	//collapse (mean) lsh, by(ln_nc)
	collapse (mean) lsh (count) n_draws = mc, by(ln_nc) //careful! will always include any n_c present in any monte carlo draw. either plot 1 draw or apply correction on next line. 
	set seed 12345
	//drop if runiform() > n_draws / `nmc' //to fix the fact that exact obs in tails are random 
	sort ln_nc
	gen key = _n
	rename lsh lsh_`index'
	rename ln_nc ln_nc_`index'
	save "$output/temp_mc_`index'", replace
	local index = `index' + 1
}

use "$output/temp_mc_1", clear
merge 1:1 key using "$output/temp_mc_2"
append using "$output/figure_rank_new_prod_lagNAll_datapoints"

twoway (scatter lsh_1 ln_nc_1, mcolor(blue) msize(3)) ///
		(scatter lsh_2 ln_nc_2, mcolor(red) msymbol(s) msize(3)) ///
		(lfit lsh_1 ln_nc_1 if ln_nc_1 <= 4.8520303, lcolor(ltblue) lpattern(dash)) ///
		(scatter lsh_av_correspc10 lnew_prod_av_correspc10, msize(3) msymbol(Dh) mcolor(midgreen)), ///
		legend(order(1 2 4) label(1 "Innovation Bursts") label(2 "Poisson Innovation") label(4 "Data") position(1) ring(0)) ///
		xtitle("Number of Products Added", size(medlarge)) ///
		ytitle("Rank", size(medlarge)) ///
		xlabel(0 "1" .6931472 "2" 1.386294 "4" 2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.1588831 "64" 4.8520303 "128", labsize(medlarge)) ///
		ylabel(-2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}" -13.815510558 "10{superscript:-6}", labsize(medlarge))
graph export "$output/Figure_11_a.pdf", replace

*** PART 3: By lag N (only N = 1, 5, 10)
foreach N in 1 5 10 {
	import delimited "${do}\plot_coordinates\figure_rank_new_prod_lagN`N'_datapoints.csv", clear
	save "$output/figure_rank_new_prod_lagN`N'_datapoints", replace

	local index = 1
	foreach modelname in main kk {
		tempfile stack_curve_N
		forvalues mc = 1/`nmc' {
			local suffix : di %02.0f `mc'
			use "$output/nc_nd_temp_mc_`modelname'_`suffix'.dta", clear

			keep if lagN == `N'
			drop if n_c == .

			gen sh = .
			levelsof n_c, local(levels)
			foreach n of local levels {
				count if n_c >= `n' & n_c != .
				local num = r(N)
				count
				local den = r(N)
				local nrow = `n' + 1
				if _N < `nrow' {
					set obs `nrow'
				}
				replace sh = `num'/`den' in `nrow' if `nrow'
			}

			gen n_nc = _n - 1
			replace n_nc = . if sh == .
			gen lsh = log(sh)
			gen ln_nc = log(n_nc)
			keep sh lsh n_nc ln_nc
			keep if ln_nc != .
			bys ln_nc: keep if _n == 1
			gen mc = `mc'

			if `mc' == 1 {
				save `stack_curve_N', replace
			}
			else {
				append using `stack_curve_N'
				save `stack_curve_N', replace
			}
		}

		use `stack_curve_N', clear
		//collapse (mean) lsh, by(ln_nc) 
		collapse (mean) lsh (count) n_draws = mc, by(ln_nc) //careful! will always include any n_c present in any monte carlo draw. correction on next line. 
		set seed 12345
		drop if runiform() > n_draws / `nmc' //to fix the fact that exact obs in tails are random 		sort ln_nc
		gen key = _n
		rename lsh lsh_`index'
		rename ln_nc ln_nc_`index'
		save "$output/temp_mc_N`N'_`index'", replace
		local index = `index' + 1
	}

	use "$output/temp_mc_N`N'_1", clear
	merge 1:1 key using "$output/temp_mc_N`N'_2"
	append using "$output/figure_rank_new_prod_lagN`N'_datapoints"

	if `N' == 1 {
		twoway (scatter lsh_1 ln_nc_1 if lsh_1 >= -11.512925, mcolor(blue) msize(3)) ///
			(scatter lsh_2 ln_nc_2 if lsh_2 >= -15, mcolor(red) msymbol(s) msize(3)) ///
			(lfit lsh_1 ln_nc_1 if ln_nc_1 <= 4.8520303 & lsh_1 >= -11.512925, lcolor(ltblue) lpattern(dash)) ///
			(scatter lsh_av_correspc10 lnew_prod_av_correspc10, msize(3) msymbol(Dh) mcolor(midgreen)), ///
			legend(off) xtitle("Number of Products Added", size(medlarge)) ///
			ytitle("Rank", size(medlarge)) ///
			xlabel(0 "1" .6931472 "2" 1.386294 "4" 2.0794415 "8" 2.7725887 "16" 3.4657359 "32", labsize(medlarge)) ///
			ylabel(-2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}", labsize(medlarge))
			graph export "$output/Figure_11_b.pdf", replace
	}
	else if `N' == 5 {
		twoway (scatter lsh_1 ln_nc_1 if lsh_1 >= -11.512925, mcolor(blue) msize(3)) ///
			(scatter lsh_2 ln_nc_2 if lsh_2 >= -11.512925, mcolor(red) msymbol(s) msize(3)) ///
			(lfit lsh_1 ln_nc_1 if ln_nc_1 <= 4.8520303, lcolor(ltblue) lpattern(dash)) ///
			(scatter lsh_av_correspc10 lnew_prod_av_correspc10, msize(3) msymbol(Dh) mcolor(midgreen)), ///
			legend(off) xtitle("Number of Products Added", size(medlarge)) ///
			ytitle("Rank", size(medlarge)) ///
			xlabel(0 "1" .6931472 "2" 1.386294 "4" 2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.1588831 "64", labsize(medlarge)) ///
			ylabel(-2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}", labsize(medlarge))
	graph export "$output/Figure_c13_a.pdf", replace
	}
	else if `N' == 10 {
		twoway (scatter lsh_1 ln_nc_1 if lsh_1 >= -11.512925, mcolor(blue) msize(3)) ///
			(scatter lsh_2 ln_nc_2 if lsh_2, mcolor(red) msymbol(s) msize(3)) ///
			(lfit lsh_1 ln_nc_1 if ln_nc_1 <= 4.8520303 & lsh_1 >= -11.512925, lcolor(ltblue) lpattern(dash)) ///
			(scatter lsh_av_correspc10 lnew_prod_av_correspc10, msize(3) msymbol(Dh) mcolor(midgreen)), ///
			legend(off) xtitle("Number of Products Added", size(medlarge)) ///
			ytitle("Rank", size(medlarge)) ///
			xlabel(0 "1" .6931472 "2" 1.386294 "4" 2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.1588831 "64", labsize(medlarge)) ///
			ylabel(0 "10{superscript:0}" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}", labsize(medlarge))
		graph export "$output/Figure_c13_b.pdf", replace
	}
}

*** CLEANUP: Delete temporary files
capture erase "$output/temp_mc_1.dta"
capture erase "$output/temp_mc_2.dta"
capture erase "$output/temp_mc_N1_1.dta"
capture erase "$output/temp_mc_N1_2.dta"
capture erase "$output/temp_mc_N5_1.dta"
capture erase "$output/temp_mc_N5_2.dta"
capture erase "$output/temp_mc_N10_1.dta"
capture erase "$output/temp_mc_N10_2.dta"
capture erase "$output/figure_rank_new_prod_lagNAll_datapoints.dta"
capture erase "$output/figure_rank_new_prod_lagN1_datapoints.dta"
capture erase "$output/figure_rank_new_prod_lagN5_datapoints.dta"
capture erase "$output/figure_rank_new_prod_lagN10_datapoints.dta"

foreach modelname in main kk {
	forvalues mc = 1/`nmc' {
		local suffix : di %02.0f `mc'
		capture erase "$output/nc_nd_temp_mc_`modelname'_`suffix'.dta"
	}
}
