*** Run Monte Carlo CSV figure/table scripts with Stata 

** prepare stata
ssc install carryforward , replace 
//ssc install blindschemes
//set scheme plotplainblind
version 17 


** settings 

clear 
set more off
args repo_root
//local repo_root D:\Bursts_ReplicationPackage\Theory  

if `"`repo_root'"' == "" {
	di as err "Missing required repo_root argument."
	exit 198
}

*** Set directories from the repository root passed by MATLAB
global do `"`repo_root'"'
capture noisily cd `"$do"'
if _rc {
	di as err `"Unable to change directory to repo_root: $do"'
	exit _rc
}

global input `"$do\montecarlo\simulationoutput"'
global output `"$do/plots"'
global year_start 1
global year_end 10
if "$mc_draws" == "" {
	global mc_draws 100
}
local nmc = real("$mc_draws")
if missing(`nmc') | `nmc' <= 0 | floor(`nmc') != `nmc' {
	di as err "Invalid global mc_draws: $mc_draws"
	exit 198
}


** produce the figures 
do "${do}\stata_file2_figure9.do"
do "${do}\stata_file3_figure10.do"
do "${do}\stata_file4_figure11.do"
do "${do}\stata_file5_figure12.do"


** produce tables and numbers 
do "${do}\stata_file6_intext_agenumbers.do"
do "${do}\stata_file7_table7_model.do"
do "${do}\stata_file8_table8_model.do"
