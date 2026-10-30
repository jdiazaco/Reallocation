
clear all
set more off

local nmc = real("$mc_draws")
if missing(`nmc') | `nmc' <= 0 | floor(`nmc') != `nmc' {
	di as err "Invalid global mc_draws: $mc_draws"
	exit 198
}

local input_folder "$input"
local output_folder "$output"
set sortseed 1234
capture mkdir "`output_folder'"
capture erase "`output_folder'/table_7_model.tex"
capture erase "`output_folder'/table_7_model.csv"


tempfile mc_tail
postfile posth int draw str30 variable double tail_param se r2 using `mc_tail', replace

//forvalues mc = 1/`nmc' {   
forvalues mc = 1/100 {
	local suffix : di %02.0f `mc'
	local src "`input_folder'/simulation_productlevel_`suffix'.csv"
	capture confirm file "`src'"
	if _rc {
		di as err "Missing required Monte Carlo file: `src'"
		exit 601
	}

	foreach var in revenue_firm  nprod rev_per_product {  // nprod rev_per_product 
		import delimited "`src'", clear varnames(1)

		sort prodindex firmid2 year
		duplicates drop firmid2 year, force

		//gen rd = (0.0467)^2 * nprod * 10.4
		//gen emp = rd + revenue_firm

		quietly summarize year
		keep if year == 10

		gen rev_per_product =  revenue_firm / nprod
		gen x_var = `var'

		quietly summarize x_var, detail
		local p95 = r(p90)

		keep x_var
		drop if x_var == .
		drop if x_var <  `p95'
		drop if x_var <= 0
		gsort -x_var
		gen rank = _n

		bysort x_var: egen rank_m = max(rank)
		duplicates drop x_var, force

		gen log_x_var = log(x_var)
		gen log_rank_min05 = log(rank-0.5)

		quietly count
		if r(N) >= 3 {
			quietly reg log_rank_min05 log_x_var, r
			local b = _b[log_x_var]
			local s = _se[log_x_var]
			local rr = e(r2)
		}
		else {
			local b = .
			local s = .
			local rr = .
		}

		post posth (`mc') ("`var'") (`b') (`s') (`rr')
	}
}

postclose posth
use `mc_tail', clear
collapse (mean) tail_param se r2, by(variable)

* labels for output
capture drop label
gen label = ""
replace label = "Revenue" if variable == "revenue_firm"
replace label = "Product Count" if variable == "nprod"
replace label = "Rev per Product" if variable == "rev_per_product"

order variable label tail_param se r2
sort variable
export delimited using "`output_folder'/table_7_model.csv", replace

