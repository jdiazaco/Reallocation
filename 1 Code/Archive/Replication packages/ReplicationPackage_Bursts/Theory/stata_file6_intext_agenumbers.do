
clear 
set more off

local nmc = real("$mc_draws")
if missing(`nmc') | `nmc' <= 0 | floor(`nmc') != `nmc' {
	di as err "Invalid global mc_draws: $mc_draws"
	exit 198
}

local input_folder "$input"
local output_folder "$output"

capture mkdir "`output_folder'"
capture erase "`output_folder'/table_age_largefirms_mcavg.tex"
capture erase "`output_folder'/table_age_largefirms_mcavg.csv"

tempfile mc_age
postfile posth int draw int cutoff str12 cutoff_label double mean_age using `mc_age', replace

forvalues mc = 1/`nmc' {
	local suffix : di %02.0f `mc'
	local src "`input_folder'/simulation_firmlevel_`suffix'.csv"
	capture confirm file "`src'"
	if _rc {
		di as err "Missing required Monte Carlo file: `src'"
		exit 601
	}

	import delimited "`src'", clear varnames(1)
	gen N = nprod
	keep if year == 1
	keep if age < 70

	foreach cutoff in 5 10 20 {
		quietly summarize age if N > `cutoff'
		post posth (`mc') (`cutoff') ("N > `cutoff'") (r(mean))
	}
}

postclose posth
use `mc_age', clear
collapse (mean) mean_age (sd) mc_sd = mean_age (count) n_draws_used = mean_age, by(cutoff cutoff_label)
sort cutoff
order cutoff cutoff_label mean_age mc_sd n_draws_used

export delimited using "`output_folder'/table_age_largefirms_mcavg.csv", replace

file open fh using "`output_folder'/table_age_largefirms_mcavg.tex", write replace
file write fh "\\begin{tabular}{lcc}" _n
file write fh "\\toprule" _n
file write fh "Cutoff & Mean age & MC SD \\\\" _n
file write fh "\\midrule" _n

quietly {
	forvalues i = 1/`=_N' {
		local lbl = cutoff_label[`i']
		local m = mean_age[`i']
		local s = mc_sd[`i']
		file write fh "`lbl' & " %9.4f (`m') " & " %9.4f (`s') " \\\\" _n
	}
}

file write fh "\\bottomrule" _n
file write fh "\\end{tabular}" _n
file close fh

di "Monte Carlo large-firm age table saved to: `output_folder'/table_age_largefirms_mcavg.csv"
di "Monte Carlo large-firm age table saved to: `output_folder'/table_age_largefirms_mcavg.tex"
