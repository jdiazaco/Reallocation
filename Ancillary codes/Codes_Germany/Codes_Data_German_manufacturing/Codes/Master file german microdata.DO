
set seed 20091990
set more off 

global date ="2024-02-08 MMS I"
  
capture log close _all
cap noisily log using "Z:\GWFreigaben\3675-2018\04 KDFV\04-02 Log\\$date\\$date Fernrechnen.log", append





//--------------------------------\\
//---define data names & paths----\\
//********************************\\ Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV

//define datapath, savepath & adopath, have to be changed
// note the \ at the end of the path!
global datapath = "Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV\"
capture mkdir "Z:\GWFreigaben\3675-2018\04 KDFV\04-02 Log\\$date\\10_steller_BNR\"
global savepath = "Z:\GWFreigaben\3675-2018\04 KDFV\04-02 Log\\$date\\10_steller_BNR\" 
global syntaxpath = "Z:\GWFreigaben\3675-2018\04 KDFV\04-01 Syntax\\$date"

adopath + "Z:\GWFreigaben\3675-2018\03 GWAP\03-00 ado"
adopath + "Z:\GWFreigaben\3675-2018\03 GWAP\03-00 ado\plus"
adopath + "Z:\GWFreigaben\3675-2018\03 GWAP\03-00 ado\FiRe"
adopath + "Z:\GWFreigaben\3675-2018\03 GWAP\03-00 ado\NEW_ADOS"
adopath + "Z:\GWFreigaben\3675-2018\03 GWAP\03-00 ado\ado_mms2"


// define original filenames, have to be changed
global Produktdaten = "Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV\Korr14ET\Na3675_kdfv_prodmod"
global Firmendaten ="Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV\Korr14ET\Na3675_kdfv_basis" //"Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV\Neu\Na3675_kdfv_basis"
global VSE = "Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV\Korr14ET\Na3675_kdfv_vse_an"
global Amadeus =  "Z:\GWFreigaben\3675-2018\02 Ausgangsdaten\02-01 KDFV\Korr14ET\na3675_kdfv_amadeus.dta"




//--------------------------------\\
//--------define settings---------\\
//********************************\\

// define location of run - "Fernrechnen" is the actual full data at the statistical office with BY. IWH is the test data at our institute. Wissens_Arbeitspl is the data accessible on-site at the statistical office without BY.
global IWH_run = ""
global Wissens_Arbeitspl = ""
global Fernrechnen = "YES"
{
if "$IWH_run" == "YES" {
global datapath = "s:\Projekte\afid\Pseudo_Datensets\"
capture mkdir "s:\PROJEKTE\CompTrade\afid\Ergebnisse_Fake_AFID_mms\\$date"
global savepath = "s:\PROJEKTE\CompTrade\afid\Ergebnisse_Fake_AFID_mms\\$date"
global mergepath = "--"
global mergepath2 = "s:\Projekte\afid\merkmale_am_stala\"
global Produktdaten = "s:\Projekte\afid\Pseudo_Datensets\Fake-Datenset_Produktdaten.dta"
global Firmendaten = "s:\Projekte\afid\Pseudo_Datensets\Fake-Datenset_Firmendaten.dta"
global VSE = "s:\Projekte\afid\Pseudo_Datensets\vse_fake.dta"
global syntaxpath = "G:\MMS\dokumente\Dissertation\Markups\MU BG\stata code single paper\\$date"


//generate an fdzsum-command
******************************************
*             Program : fdzsum           *
******************************************
{

capture program drop fdzsum
program define fdzsum, rclass
args sum_var option 2option

su `sum_var' `option'`2option'

end
}






//generate an fdzsum-command
******************************************
*             Program : fdztabstat           *
******************************************
{

capture program drop fdztabstat
program define fdztabstat, rclass
syntax varlist [if] [aweight]   ,  [STAtistics(string)]  [by(string)]

tabstat `varlist' `if'  [`weight' `exp']   ,  statistics(`statistics') by(`by')

end
}


//


//FIX REGHDFE BUG FOR THE IWH!!!!!
*************************************
//depends on version of reghdfe
global iwh_run_reghdfe = "old"





}

if "$Wissens_Arbeitspl" == "YES" {
adopath + "Z:\03-00 ado\"
adopath + "Z:\03-00 ado\plus\"
adopath + "Z:\03-00 ado\FiRe"

global datapath = "Z:\03-01 Ausgangsdaten\"
global savepath = "Z:\03-02 Arbeitsbereich\Jobs MMS\results\\"


global Produktdaten = "Z:\03-01 Ausgangsdaten\na3675_gwap_prodmod"
global Firmendaten = "Z:\03-01 Ausgangsdaten\na3675_gwap_basis"
global VSE = "Z:\03-01 Ausgangsdaten\na3675_gwap_vse_an"
global syntaxpath = "Z:\03-02 Arbeitsbereich\Jobs MMS\code\\$date"





//generate an fdzsum-command
******************************************
*             Program : fdzsum           *
******************************************
{

capture program drop fdzsum
program define fdzsum, rclass
args sum_var option 2option

su `sum_var' `option'`2option'

end
}
//







//generate an fdzsum-command
******************************************
*             Program : fdztabstat           *
******************************************
{

capture program drop fdztabstat
program define fdztabstat, rclass
syntax varlist [if] [aweight]   ,  [STAtistics(string)]  [by(string)]

tabstat `varlist' `if'  [`weight' `exp']   ,  statistics(`statistics') by(`by')

end
}



}



// CREATE FOLDERS
// Folders for descriptive statistics
{
capture mkdir "$savepath\Freigabe"
capture mkdir "$savepath\Concordances\"
capture mkdir "$savepath\PriceindicesAndDepreciation\"
capture mkdir "$savepath\Freigabe\FIRST_STAGES\"
capture mkdir "$savepath\Freigabe\Descriptive"
capture mkdir "$savepath\Freigabe\Priceindices"
capture mkdir "$savepath\Freigabe\Capital_Stock"
capture mkdir "$savepath\Freigabe\Production Function Variables"
capture mkdir "$savepath\Freigabe\Proximity"
capture mkdir "$savepath\TFP\"


//folders technology LMP_models
capture mkdir "$savepath\techno_LMP\"
capture mkdir "$savepath\Freigabe\techno_LMP\"
capture mkdir "$savepath\Freigabe\techno_LMP\Graphs"
capture mkdir "$savepath\Freigabe\techno_LMP\Regs"
capture mkdir "$savepath\Freigabe\Freigabe_LOG"

}
//


// write a log for interesting things

log using "$savepath\Freigabe\Log_Error.log", replace name(Log_Error)
log close Log_Error
log using "$savepath\Freigabe\Log_Data_preparation.log", replace name(Log_Data_preparation)
log close Log_Data_preparation
log using "$savepath\Freigabe\Log_Descpritives.log", replace name(Log_Descriptives)
log close Log_Descriptives
log using "$savepath\Freigabe\Log_pmp_lmp.log", replace name(Log_pmp_lmp)
log close Log_pmp_lmp
log using "$savepath\Freigabe\Freigabe_LOG\Log_Freigabe.log", replace name(Log_Freigabe)
log close Log_Freigabe


//set color scheme
set scheme s1color
}
//


//--------------------------------\\
//-----define data set & task-----\\
//********************************\\
{

//Define Start and End year
global start_year_load = 1995
global start_year_estimate = 1995
global start_year_prodvars = $start_year_estimate 
global end_year_load = 2017

if  "$IWH_run" == "YES"  {
global start_year_estimate = 2002
global start_year_prodvars = $start_year_estimate 
global end_year_load = 2012
}


//define all specifications to be estimated
********************************************

global Capdefinitions  KMRSoft
global KMR_SOFT_fix = "YES"  


//decompose merge file
global decompose_wz_data = "YES"
global start_new_data = "YES"
global new_merkmale_wz_2020 = "YES"  ////not LS not east west
//***************************************************************************************************************************\\
//-----------------------------------------------------Data Preparation------------------------------------------------------\\
//***************************************************************************************************************************\\

//product data generation
global Transform_GP = "YES"
global purge_incomplete_reports = "YES"
global generate_proximity_measures ="YES"
global calculate_revenue_price_index = "YES"
global adjust_GP09V2 = "YES"


//Finalize production function data
global generate_capital_stocks = "YES"
global Prod_fun_variables ="YES"


//directed technological Change and LMP	 
global technology_LMP = "YES"

	
	 
}
//

//-------------------------------\\
//------derive system globals----\\
//-------------------------------\\
quietly {


//derive the panel legth of the sample
global panel_length = $end_year_load - $start_year_estimate 

//define MiddleYear
global MiddleYear = round(($end_year_load - $start_year_estimate )/2 + $start_year_estimate )

//end chapter
}
//



//This defines a series of user-written programs - some of which are used in this paper.
//-------------------------------\\
//--------initiate programs------\\
//-------------------------------\\
quietly {


******************************************
*     Program 1: make_varlist_unique     *
******************************************
{
/*
This program takes a list of words and returns the same list, but without any 
elements that repeat themselves
E.g: "unr jahr jahr Umsatz" ==> "unr jahr Umsatz"
*/

capture program drop make_varlist_unique
program define make_varlist_unique, rclass

args list_of_vars

//make each entry in the list unique
//-------------------------------------


	//check whether there is even more than 1 wordn the list
	local count = 0
	foreach entry in `list_of_vars' {
	local count = `count' + 1
	}


	//generate a local to gather the unique list
	local unique_list = "`list_of_vars'"
	if `count' > 1 {
	foreach argument in `list_of_vars' {
	local unique_list = subinstr(" `unique_list' ", " `argument' ", "  ", .)
	local unique_list = "`unique_list' `argument'"
	}
	}
	
return local unique_list = "`unique_list'"


end
}
//


******************************************
*       Program 2: Polynome_varlist      *
******************************************
{

/*
This program takes a of variables and constructs the appropriate translog function
It takes the list of variables in list_of_vars
This list should follow the following format: 
It should only contain logarithmic variables or Dummies.
Dummy variables' names should start with "D_"


It takes the kind of polynome as kind_of_translog. 
Possible values are: 
- 2, to generate a second order polynomial
- 2+, to generate a second order polynomial with three-variable simple interaction terms
- 3, to generate a third order polynomial


It generates the interaction-variables if status_generate includes the string "gen_terms"
It generates laggs for these variables if status_generate includes the string "gen_lags"

It returns a list of the variables and of the lagged variables
*/


capture program drop poly_varlist
program define poly_varlist, rclass

args list_of_vars kind_of_translog status_generate

//before start, make each entry in the list unique
	//generate a local to gather the unique list
	make_varlist_unique "`list_of_vars'"
	local list_of_vars = "`r(unique_list)'"
	
//before start, check whether the varlist does contain any entries
local check = length(subinstr("`list_of_vars'", " ", "", .))
if `check' > 0 {




//this part determines whether status_generate does contain the strings which trigger generation of variables
	//determines the length of status_generate
	local hlp = length("`status_generate'")
	//subinstrs the string triggering creation of interaction terms
	local terms = subinstr("`status_generate'", "terms", "", .)
	//if this results in a shorter string, set gen_inter_terms = "YES"
	local terms = length("`terms'")
	if `terms' < `hlp' {
	local terms = "YES"
	}


	//subinstrs the string triggering creation of interaction terms
	local lags = subinstr("`status_generate'", "lags", "", .)
	//if this results in a shorter string, set gen_inter_terms = "YES"
	local lags = length("`lags'")
	if `lags' < `hlp' {
	local lags = "YES"
	}

	display "`lags'"
	
//set the local in which all variables are gathered to ""
	local translogg_vars = "" 
//set a counting global to 0
	global controlvarcount = 0

	
//generate the basic lags so that they can be interacted further down
lag_varlist "`list_of_vars'" 1
//pick the first argument out of list_of_vars
foreach first_arg in `list_of_vars' {
	global controlvarcount = ${controlvarcount}+1
	//add own powers
	local translogg_vars = "`translogg_vars'" + "`first_arg' "
		if "`lags'" == "YES" {
		 capture gen l_`first_arg' = l.`first_arg'
		}
	if substr("`first_arg'",1,2) != "D_" {
	local translogg_vars = "`translogg_vars'" + " `first_arg'2  "
		if "`terms'" == "YES" {
		 capture gen `first_arg'2 = (`first_arg')^2
		}
		if "`lags'" == "YES" {
		 capture gen l_`first_arg'2 = (l_`first_arg')^2
		}
		if "`terms'" == "YES" & `kind_of_translog' == 3 {
		local translogg_vars = "`translogg_vars'" + " `first_arg'3  "
		 capture gen `first_arg'3 = (`first_arg')^3
		}
		if "`lags'" == "YES" & `kind_of_translog' == 3 {
		 capture gen l_`first_arg'3 = (l_`first_arg')^3
		}
		
	}
//add combination between own first power and all legit two power partners
	//check whether in this specification, this is actually required
	if `kind_of_translog' == 3 {
		local partnervarcount = 0
		foreach second_arg in `list_of_vars' {
			local partnervarcount = `partnervarcount'+1
			if substr("`second_arg'",1,2) != "D_" {
					if `partnervarcount' > ${controlvarcount} {
					local translogg_vars = "`translogg_vars' " + "   `first_arg'_`second_arg'2 "
						if "`lags'" == "YES" {
						 capture gen l_`first_arg'_`second_arg'2 = l_`first_arg' * (l_`second_arg')^2
						 
						}
						if "`terms'" == "YES" {
						 capture gen `first_arg'_`second_arg'2 = `first_arg' * (`second_arg')^2
						 
						}
					}
			}
		}

	}
//add combination between own first power and all legit one power partners
	local partnervarcount = 0
	foreach second_arg in `list_of_vars' {
		local partnervarcount = `partnervarcount'+1
			if `partnervarcount' > ${controlvarcount} {
			local translogg_vars = "`translogg_vars' " + " `first_arg'_`second_arg' "
				if "`lags'" == "YES" {
				 capture gen l_`first_arg'_`second_arg' = l_`first_arg' * (l_`second_arg')
				}
				if "`terms'" == "YES" {
				 capture gen `first_arg'_`second_arg' = `first_arg' * (`second_arg')
				}
			}
	}
//add combination of own second power with all legit partners
	//check whether in this specification, this is actually required
	if `kind_of_translog' == 3 {
		local partnervarcount = 0
		if substr("`first_arg'",1,2) != "D_" {
			foreach second_arg in `list_of_vars' {
				local partnervarcount = `partnervarcount'+1
				if `partnervarcount' > ${controlvarcount} {
					local translogg_vars = "`translogg_vars' " + " `first_arg'2_`second_arg' "
					if "`lags'" == "YES" {
						 capture gen l_`first_arg'2_`second_arg' = (l_`first_arg')^2 * l_`second_arg'
						}		
					if "`terms'" == "YES" {
						 capture gen `first_arg'2_`second_arg' = (`first_arg')^2 * `second_arg'
						}
				}
			}
		}
	}
//add combination of three variables
	//check whether in this specification, this is actually required
	if `kind_of_translog' == 3 | "`kind_of_translog'" == "2+" {
		local combothreecount = 0 
		foreach second_arg in `list_of_vars' {
			local combothreecount = `combothreecount' + 1
			local combothreecount2 = 0
			if `combothreecount' > ${controlvarcount} {
				foreach third_arg in `list_of_vars' {
					local combothreecount2 = `combothreecount2' +1
					if `combothreecount2' > `combothreecount'{
						local shortfirst_arg = subinstr("`first_arg'","ln", "", .)
						local shortsecond_arg = subinstr("`second_arg'","ln", "", .)
						local shortthird_arg = subinstr("`third_arg'","ln", "", .)
						local translogg_vars = " `translogg_vars' " + " `shortfirst_arg'_`shortsecond_arg'_`shortthird_arg' "
						
						if "`lags'" == "YES" {
							 capture gen l_`shortfirst_arg'_`shortsecond_arg'_`shortthird_arg' = l_`first_arg' * l_`second_arg' * l_`third_arg'
							}		
						if "`terms'" == "YES" {
							 capture gen `shortfirst_arg'_`shortsecond_arg'_`shortthird_arg' = `first_arg' * `second_arg' * `third_arg'
							}
						}
					}
				}
		}
	}
	

//repeat for other "first" variables to cycle through all combinations
}


//generate a local that contains not the variables, but the lagged variables, in case this should be returned
local lagged_trans_vars = ""
foreach var in `translogg_vars'{
local lagged_trans_vars = "`lagged_trans_vars' l_`var'"
}


}
return local lagged_trans_vars = "`lagged_trans_vars'"
return local translog_vars = "`translogg_vars'"

end
}
//


******************************************
*          Program 3: Lag_varlist        *
******************************************
{
/*
This programm laggs all variables presented in a varlist
*/

capture program drop lag_varlist
program define lag_varlist, rclass

args list_of_vars number_of_lags option


if "`list_of_vars'" != "" { 

if "`number_of_lags'" ==""{
local number_of_lags = 1
}

//before start, make each entry in the list unique
	//generate a local to gather the unique list
	make_varlist_unique "`list_of_vars'"
	local list_of_vars = "`r(unique_list)'"

//before start, check whether the varlist does contain any entries
local check = length(subinstr("`list_of_vars'", " ", "", .))
if `check' > 0 {


//start a local to capture all the lagged elements
local lagged_list = ""
//lagg all entries
foreach element of varlist `list_of_vars' {
	if `number_of_lags' > 1{
		if "`option'" != "nogen" {
		 capture gen l`number_of_lags'_`element' = l`number_of_lags'.`element'
		}
	local lagged_list = "`lagged_list' l`number_of_lags'_`element'"
	}
	if `number_of_lags' < 2 {
		if "`option'" != "nogen" {
		 capture gen l_`element' = l.`element'
		}
	local lagged_list = "`lagged_list' l_`element'"
	}
}
}
return local lagged_list = "`lagged_list'"

}


end
}
//



******************************************
*          Program 3: diff_varlist        *
******************************************
{
/*
This programm laggs all variables presented in a varlist
*/

capture program drop diff_varlist
program define diff_varlist, rclass

args list_of_vars number_of_lags option


if "`list_of_vars'" != "" { 

if "`number_of_lags'" ==""{
local number_of_lags = 1
}

//before start, make each entry in the list unique
	//generate a local to gather the unique list
	make_varlist_unique "`list_of_vars'"
	local list_of_vars = "`r(unique_list)'"
	
//before start, check whether the varlist does contain any entries
local check = length(subinstr("`list_of_vars'", " ", "", .))
if `check' > 0 {


//start a local to capture all the lagged elements
local lagged_list = ""
//lagg all entries
foreach element of varlist `list_of_vars' {
	if `number_of_lags' > 1{
		if "`option'" != "nogen" {
		capture gen d`number_of_lags'_`element' = d`number_of_lags'.`element'
		}
	local lagged_list = "`lagged_list' l`number_of_lags'_`element'"
	}
	if `number_of_lags' < 2 {
		if "`option'" != "nogen" {
		capture gen d_`element' = d.`element'
		}
	local lagged_list = "`lagged_list' d_`element'"
	}
}
}
return local lagged_list = "`lagged_list'"

}


end
}
//


******************************************
*          Program 4: Ln_varlist         *
******************************************
{
/*
This programm laggs all variables presented in a varlist
*/

capture program drop ln_varlist
program define ln_varlist, rclass

args list_of_vars option



//before start, make each entry in the list unique
	//generate a local to gather the unique list
	make_varlist_unique "`list_of_vars'"
	local list_of_vars = "`r(unique_list)'"
	
//before start, check whether the varlist does contain any entries
local check = length(subinstr("`list_of_vars'", " ", "", .))
if `check' > 0 {

//start a local to capture all the lagged elements
local lned_list = ""

foreach element in `list_of_vars' {

	if "`option'" != "nogen" {
		capture gen ln`element' = ln(`element')
		}
	local lned_list = "`lned_list' ln`element'"
	}
}
return local lned_list = "`lned_list'"

end
}
//


******************************************
*       Program 5: gen_weights_reg       *
******************************************
{
global year_no_weight = 1999
global year_no_weight_test = 1999
/*
This program takes a list of variables, checks for each observation whether they exist and thus whether they will be used.

It takes as arguments 1 the original regression command
*/

capture program drop gen_weights_reg
program define gen_weights_reg, rclass

args estimation

capture `estimation'

//if estimation works, construct the weights
if _rc == 0 {
gen flag_valid = e(sample)


quietly bysort unr jahr flag_valid: gen number = _n
quietly gen valid_unique_obs = 1 if flag_valid == 1 & number==1
quietly replace valid_unique_obs = 0 if valid_unique_obs ==.
quietly bysort size_bin_labor wz_u jahr: egen num_valid_obs = total(valid_unique_obs)

quietly gen weight = true_num_firms/num_valid_obs if flag_valid == 1
replace weight =1 if $start_year_estimate <= $year_no_weight
drop num_valid_obs flag_valid valid_unique_obs number

return local weight_status = "succeeded"
}



if _rc != 0 {

log using "$savepath\freigabe\Log_Error.log", append name(Log_Error)
/*
This log has been initiated by gen_weights_reg because the command failed to compute weights.
this happened because the initial reg-command failed for some reason. The following log 
should help clarify the cause of the error

*/
noisily display "Programm gen_weights_reg has failed to drop singleton dummies"
noisily display "the regression command called did not work, an error message follows"
noisily display "The estimation command was: `estimation'"
noisily display "`r(N)' observations have all variables necessary for estimation (before dropping singletons"
capture noisily `estimation'

return local weight_status = "failed"
log close Log_Error
}



end
}

//


******************************************
*       Program 6: aggregate_data        *
******************************************
{
/*
This programm aggregates all data which shares the value of new_uniqui_ident to one observation
1. Some variables, given in sum_vars, will be summed up over all observations and this sum will become the new value for the "master" observation.
2. The master observation will get the mean over all variables it replaces for the variables detailed in mean_vars.
3. The master observation will get the most prevalent value for the variables detailed in prevalent_vars. 
Prevalence will be decided upon based on importance_var or. If the user provides "count" to this argument, prevalence will be decided upon based on frequency.
4. All other variables WILL BE DROPPED because they are not aggregated
5. If some varlist are supposed to be empty, write "none" into the field
*/

capture program drop aggregate_data
program define aggregate_data, rclass

args new_unique_ident sum_vars mean_vars prevalent_vars importance_var similar_vars

display "--------------------------------------"
display "Program aggregate_data has been called"
display "--------------------------------------"

display "This programm aggregates different observations, all of which share the variables `new_unique_ident'"
display "Specifically, the new observation will contain the sum of the values of the variables: `sum_vars'"
display "The new observation will contain the mean of the values of the variables: `mean_vars'"
display "The new observation will contain the most prevalent value for these variables: `prevalent_vars'"
if "`importance_var'" == "count"  {
display "Prevalence is decided upon based on how many observations contain each value of `prevalent_vars'"
}
else {
display "Prevalence is decided upon based on how much of `importance_var' the various values represent"
}
display "The following variables should should only have one value within the group "
display "and will be kept if this is true: `similar_vars'"


***************************************************************************************************************************************
* Diagnosis Part:
* Here we the code prepares some statistics to be compared with the final dataset
* This should allow to find errors in the code/data
quietly count
local obs_at_start = `r(N)'


//generate sums of all summable variables in order to compare pre and post and check for error
//-------------------------------------------------------------------------------------------
//before start, make each entry in the list of checked vars is is unique (to prevent double creation of variables and save time)
	//generate a local to gather the unique list
	local unique_list = "`sum_vars' `importance_var'"
	foreach argument in `sum_vars' `importance_var' {
	local unique_list = subinstr("`unique_list'", "`argument' ", "  ", .)
	local unique_list = subinstr("`unique_list'", " `argument'", "  ", .)
	local unique_list = "`unique_list' `argument'"
	}
	
//generate the totals for each of these vars and safe the names of the vars containing these totals
global checkvars = ""
foreach checkablevar in `unique_list' {
//local t_`checkablevar' = total(`checkablevar')
egen t_`checkablevar' = total(`checkablevar')
global checkvars = "$checkvars t_`checkablevar'"
}


****************************************************************************************************************************************







*********************************************-//-------------------------------\\-******************************************************
*********************************************-//------actual aggregation-------\\-******************************************************
*********************************************-//-------------------------------\\-******************************************************
sort `new_unique_ident'
by `new_unique_ident': gen N_of_ident = _N



//aggregate the mean_vars
foreach meanvar in `mean_vars' {
by `new_unique_ident': egen total_importance = total (`importance_var')
by `new_unique_ident': egen help = total (`meanvar' * `importance_var' / total_importance )
replace `meanvar' = help
drop help total_importance 
}

//aggregate the prevalent_vars
foreach prevalencevar in `prevalent_vars' {
	
	
	//if importance == count, aggregate using #obs
	if "`importance_var'" == "count" {
	by `new_unique_ident': egen help = mode (`prevalentvar'), missing
	replace `prevalencevar' = help
	drop help
	}

	//if importance != count, aggregate using `importance_var'
	if "`importance_var'" != "count" {
	//generate total `prevalence_var per importance_var'
	bys `new_unique_ident' `prevalencevar': egen help = total(`importance_var')
	//finding the maximum of importance_var
	by `new_unique_ident': egen hlp2 = max(help)
	//generate new variable that captures the prevalence_var which has won the comparison
	gen hlp3 = `prevalencevar' if  hlp2 == help
	//spread this information over the whole group
	by `new_unique_ident': egen final_hlp = mode(hlp3)
	replace `prevalencevar' = final_hlp
	drop help hlp2 hlp3 final_hlp
	}
	

}


//aggregate the similar_vars
foreach similar_var in `similar_vars' {
	

	by `new_unique_ident': egen help = mode (`similar_var'), missing
	count if help != `similar_var'
	if `r(N)' ==0 {
	replace `similar_var' = help
	}
	if `r(N)' != 0 {
	log using "$savepath\Freigabe\Log_Error.log", append name(Log_Error)
	display "aggregate_data expected the following variables to be similar within the group to be aggregated: `similar_vars'"
	display "For variable `similar_var', this was not the case, at least in `r(N)' cases."
	display "file: $S_FN"
	log close Log_Error	
	error 1
	}
	drop help

}



//aggregate the sum_vars
foreach sumvar in `sum_vars' {
by `new_unique_ident': egen help = total (`sumvar')
replace `sumvar' = help
drop help
}

foreach other_variable of varlist _all {
//look whether the variable has allready been switched
local already_covered = "no"
foreach switched_var in `new_unique_ident' `unique_list' `mean_vars' `prevalent_vars'  `similar_vars' $checkvars N_of_ident {
if "`switched_var'" == "`other_variable'" {
local already_covered = "YES"
}
}



if "`already_covered'" == "no" {
capture noisily replace `other_variable' = . if N_of_ident > 1
capture noisily replace `other_variable' = "" if N_of_ident > 1
}
}


//closing lines of the code, wrapping up the aggregation, dropping unaggregated variables

duplicates drop 



***************************************************************************************************************************************
* Diagnosis Part:
* Here we the code compares statistics from the beginning and the end 
* This should allow to find errors in the code/data


//generate sums of all summable variables in order to compare pre and post and check for error
//-------------------------------------------------------------------------------------------
//generate the totals for each of these vars
foreach checkablevar in `unique_list' {
//local t_`checkablevar' = total(`checkablevar')
egen t2_`checkablevar' = total(`checkablevar')
gen test = t2_`checkablevar'/t_`checkablevar'
if test < 0.998 | test > 1.002 {
display "In variable `checkablevar', program aggregate_data has detected changes in the total level, which hints add failed aggregation"
error 1
}
drop test
}


count
local obs_at_end = `r(N)'
local percentage_obs_shrinkage = 1 - `obs_at_end'/`obs_at_start'
display "observations shrank from `obs_at_start' to `obs_at_end' (by `percentage_obs_shrinkage'%)"

keep `new_unique_ident' `sum_vars' `mean_vars' `prevalent_vars' `importance_var' `similar_vars'

bysort `new_unique_ident':gen N = _N

count if N >1 
if `r(N)' > 0 {
display "detected duplicates in `new_unique_ident' AFTER aggregation"
error 1
}
drop N
end
}
//


******************************************
*       Program 7: start_diagnosis       *
******************************************
{
/* 
This program prepares some statistics to be compared with a later version of a data set (or another one)
This does NOT compare anything by itself, it requieres end_diagnosis, the next program
This should allow to find errors in the code/data
*/

capture program drop start_diagnosis
program define start_diagnosis, rclass
	args sum_vars
	

display "start_diagnosis has been called. The program computes the total of `sum_vars' and"
display "counts observations. The program end_diagnosis will again count observations and"
display "compute the total of the same vars. If both have not changed, it will delete all "
display "variables created by these programs and state that diagnosis has found nothing wrong"

quietly count
local obs_at_start = `r(N)'

global results_start_diagnosis = "`r(N)' ~"
//generate sums of all summable variables in order to compare pre and post and check for error
//-------------------------------------------------------------------------------------------
//before start, make each entry in the list of checked vars is is unique (to prevent double creation of variables and save time)
	//generate a local to gather the unique list
	local unique_list = "`sum_vars'"

	
	local listcount = 0
	foreach argument in `unique_list' {
	local listcount = `listcount' +1
	}
	
	
	if `listcount' > 1 {
	foreach argument in `unique_list' {
	local unique_list = subinstr("`unique_list'", "`argument' ", "  ", .)
	local unique_list = subinstr("`unique_list'", " `argument'", "  ", .)
	local unique_list = "`unique_list' `argument'"
	}
	}
//generate the totals for each of these vars 
foreach checkablevar in `unique_list' {
//local t_`checkablevar' = total(`checkablevar')
quietly su `checkablevar'
global results_start_diagnosis = "$results_start_diagnosis `checkablevar'#`r(sum)'_`r(Var)'"
}

display "$results_start_diagnosis"
end
}
//


******************************************
*        Program 8: end_diagnosis        *
******************************************
{
/*
Here we the code compares statistics from the beginning and the end 
This should allow to find errors in the code/data
*/


capture program drop end_diagnosis
program define end_diagnosis, rclass
	args obs_equal
// must either be obs_equal or anything else

display "end_diagnosis has been called. The program will again count observations and "
display "compute the total of the same vars. If both have not changed compared to last, it will delete all "
display "variables created by these programs and state that diagnosis has found nothing wrong"
***************************************************************************************************************************************
* Diagnosis Part:
* Here we the code compares statistics from the beginning and the end 
* This should allow to find errors in the code/data


//read out the diagnosis global
//-----------------------------

			
			
		//if it is empty, terminate and write an error log
		if "$results_start_diagnosis" == "" {
		log using "$savepath\Freigabe\Log_Error.log", replace name(Log_Error)
			
		//display error message
		display "end_diagnosis has been called, but found global varlist_start_diagnosis empty"
		display "this can happen when some start_diagnosis has not been called before or"
		display "all globals have been erased for some reason. No check took place"
		display "file: $S_FN"
			
		log close Log_Error
		}
		//
			
			
			
			
			
		//if it is not empty, read out, produce the same statistics and compare
		if "$results_start_diagnosis" != "" {
		
		//generate a local to count the discrepancies
		local differences = 0
				
				
				
				*********Number of observations*********
				//produce number of obs
				quietly count
				//read out the olde number of obs and compare
				tokenize "$results_start_diagnosis", p("~")
				local percentage_obs_shrinkage = 1 - `r(N)'/`1'
				display "observations shrank from `1' to `r(N)' (by `percentage_obs_shrinkage'%)"
				
				if `r(N)' != `1' {
					if "`obs_equal'" == "obs_equal" {
						local differences = `differences' + 1
						log using "$savepath\Freigabe\Log_Error.log", append name(Log_Error)
						display "program end_diagnosis expected `1' observations, but found `r(N)'"
						display "file: $S_FN"
						log close Log_Error
						}
					}
				
				
				
				********Stats for each variable********
				display "`3'"
				foreach checkvariable in `3' {
				tokenize "`checkvariable'", p(#)
				quietly sum `1' 
				local sum = `r(sum)'
				local complete_check = "`r(sum)'_`r(Var)'"
				
				
				
				if "`obs_equal'" == "obs_equal" {
					if "`complete_check'" != "`3'" {
						local differences = `differences' + 1
						log using "$savepath\Freigabe\Log_Error.log", append name(Log_Error)
						display "program end_diagnosis expected variable `1' to have the following stats:"
						display "`3'"
						display "instead, it had:"
						display "`complete_check'"
						display "file: $S_FN"
						log close Log_Error		
						}
					}

					
				if "`obs_equal'" != "obs_equal" {
				local variable_checked = "`1'"
					tokenize "`3'", p(_) 
					if "`sum'" != "`1'" {
						local differences = `differences' + 1
						log using "$savepath\Freigabe\Log_Error.log", append name(Log_Error)
						display "program end_diagnosis expected variable `variable_checked' to have the following stats:"
						display "`1'"
						display "instead, it had:"
						display "`sum'"
						display "file: $S_FN"
						log close Log_Error		
						}
					}
					
					
					
					
					
				}
				
				
				
			//set the global varlist_start_diagnosis to zero: the comparison is over
			global results_start_diagnosis =""
			//display differences
			display "diagnosis has found `differences' differences between the two compared files"
			return local diffs = `differences'
			}
			
			
end
}
//



******************************************
*          Program 9: switch_GPs         *
******************************************
{

/*
* This program takes care of the switching of product codes to a different GP
* CARE: It requieres adequatly reshaped concordances
*/
capture program drop switch_GPs
program define switch_GPs, rclass
	args source_var from to portfolio_holder outcome_var 
	
display "The program switch_GPs has been called. It checks product codes in subsequent years to "
display "find out whether codes in one code can be redirected to another. "
display "Program is set to switching GP codes, so it does know when 'breaks' in the codes occur"
display "This instance of the program switches codes from `from' to `to'. It considers observations which"
display "share `portfolio_holder' as a portfolio. The resulting new product code is stored in `outcome_var'."
display "The product code is taken from `source_var'"

//---------------------------generate helpvars and globals-------------------------------\\
//---------------------------------------------------------------------------------------\\

display "-----preparing data------"
quietly {

//depending on the source variable, data preparation is slightly different
//pe_13 does not contain information on the GP-classification used and 
//has the code in digit 1-10, while final_gp02 has information on the 
//classification and contains the code in digits 3-12
	
	
	//However, this first part is the same:
	//We delete all old information on codes etc. 
	//This makes it impossible that some variables with Pe_13 information
	//pollute final_gp02 estimation or vice versa
	
	foreach necessary_vars in flag_switch_group02 flag_switch_group09 flag_GP95 flag_GP02 flag_GP09 {
	capture drop `necessary_vars'
	}
	//merge information regarding which code was reported in which classification and which bnrs were present when
	merge m:1 bnr jahr using "$savepath\GP-flags_program_switch_GPs"	
	drop if _merge ==2
	drop _merge
	
	
	
				//This is the data preparation for pe_13
				//--------------------------------------
				
				if "`source_var'" == "pe_13" {

				//generate the product codes that can be switched, dependent on what is the source variable
				gen `from' = substr(pe_13, 1, 9) if flag_`from' == 1
				gen `to' = substr(pe_13, 1, 9) if flag_`to' == 1
				gen `from'_last_digit = substr(pe_13, 10, 1) if flag_`from' == 1
				gen `to'_last_digit = substr(pe_13, 10, 1) if flag_`to' == 1
				}
				
				
				
				
				
				
				//This is the data preparation for final_gp02
				//--------------------------------------
				
				if "`source_var'" == "final_gp02" {
				//however, we do not want to know the "old" information on which obs was originally reported in which classification.
				//We instead refer to how things are reported/computed now, after the final_gp02 has allready been computed the first time
				drop flag_GP95 flag_GP02 flag_GP09
				
				
				//generate flags for GP95
				gen flag_GP95 = 1 if substr(final_gp02,1,2)=="95"
				replace flag_GP95 = 0 if substr(final_gp02,1,2)!="95"
				
				//generate flags for GP95
				gen flag_GP02 = 1 if substr(final_gp02,1,2)=="02"
				replace flag_GP02 = 0 if substr(final_gp02,1,2)!="02"
				
				//generate flags for GP95
				gen flag_GP09 = 1 if substr(final_gp02,1,2)=="09"
				replace flag_GP09 = 0 if substr(final_gp02,1,2)!="09"
			
				//generate the product codes that can be switched, dependent on what is the source variable
				gen `from' = substr(final_gp02, 3, 9) if flag_`from' == 1
				gen `to' = substr(final_gp02, 3, 9) if flag_`to' == 1
				gen `from'_last_digit = substr(final_gp02, 12, 1) if flag_`from' == 1
				gen `to'_last_digit = substr(final_gp02, 12, 1) if flag_`to' == 1
				}
				

				
				
				
				
				
	if "`source_var'" != "final_gp02" & "`source_var'" != "pe_13" {
	display "invalid source variable: `source_var'"
	display "program does not know which digits form a valid GP-code"
	display "please edit program"
	error 1
	}
	
	
//To compress the runtime, We try to exclude as many observations from computation as possible.
//Because we run a forvalues loop over all observations, this just means sorting them in the back 
//and letting the forvalues loop only run over the first X observations. For this we use the 
//flag_switch_groupXX variable. Only observations which have a 1 there will be looped over.
//They will also be sorted according to which should be switched and which gives the possible codes
//This is done by defining direction_switch.


	
	
	if "`from'`to'" == "GP95GP02" | "`from'`to'" == "GP02GP95" {
	gen flag_switch_group_prog = flag_switch_group02
	}
	if "`from'`to'" == "GP02GP09" | "`from'`to'" == "GP09GP02" {
	gen flag_switch_group_prog = flag_switch_group09
	}
	if "`from'`to'" == "GP95GP09" | "`from'`to'" == "GP09GP95" {
	gen flag_switch_group_prog = .
	}
	gen direction_switch = 1 if flag_switch_group_prog == 1 & flag_`from' ==1
	replace direction_switch = 2 if flag_switch_group_prog == 1 & flag_`to' ==1
	
	
	

			
			
	//determine whether a switch group actually has both `from' and `to' observations in it
	//excluding them lowers runtime and leads to correct results (because they would mix up the sorting)
	sort flag_switch_group_prog `portfolio_holder' direction_switch
	by flag_switch_group_prog `portfolio_holder': egen max_from = max(flag_`from')
	by flag_switch_group_prog `portfolio_holder': egen max_to = max(flag_`to')
	replace flag_switch_group_prog = . if max_to != 1 | max_from !=1
	
	
	
//merge with the appropriate concordance to have candidates for product code switching 
//------------------------------------------------------------------------------------



	if "`from'" == "GP09" & "adjust_GP09V2" == "YES" {
	
	gen GP09V2 =  GP09 if jahr >= 2012
	
		rename GP09 GP09_temp
		
	merge m:1 GP09V2 using "$savepath\Concordances\\Konkordanz GP09V2 to GP09.dta" 
	drop if _merge == 2
	
	replace GP09_temp = GP09 if jahr >= 2012 & GP09 != "" 
	drop GP09V2
	drop _merge
	drop GP09
	
	rename GP09_temp GP09
	
	}
	
	
	
	merge m:1 `from' using "$savepath\Concordances\\wide_`from'_to_`to'"
	drop if _merge == 2

	

	
	
	
	
	
//-----------------------------------------------------------------------------------------------\\	
//sort to allow for cycling through all observations (sorting MUST NOT BE CHANGED during program)\\
//-----------------------------------------------------------------------------------------------\\
sort flag_switch_group_prog `portfolio_holder' direction_switch


	
//generate helpful variables & globals for the loop
//------------------------------------------------

	//to have information on which observation of a portfolio we are currently cycling through
	by flag_switch_group_prog `portfolio_holder' direction_switch: gen n = _n
	by flag_switch_group_prog `portfolio_holder' direction_switch: gen N = _N
	//To determine up to which observation we must cycle through the data
	quietly count if flag_switch_group_prog == 1
	global num_of_obs_to_compute = `r(N)' 
	global third_of_computation = round($num_of_obs_to_compute / 3)
	global two_thirds_of_computation = round($num_of_obs_to_compute * 2/ 3)
	//This constructs a helpvar determining how many ambigvars this particular observation actually has
	gen `to'abig_count = 0
	foreach ambigvar of varlist `to'_ambig* {
	replace `to'abig_count = `to'abig_count + 1 if `ambigvar' !=""
	}
	//construct some reporting globals
	global products_at_the_border = 0
	global unambigious_prod = 0
	global switched_ambigious_prod = 0
	global ambigious_prod_after_eli = 0
	global unswitched_prod = 0
	global obs_not_in_concordance_at_rift = 0
	//finally, generate the outcome variable:
	gen `outcome_var' = ""
	gen `outcome_var'_4digit = ""
	gen `outcome_var'_3digit = ""
	gen `outcome_var'_2digit = ""

}	
//------------start going over all observations in order to check the concordance---------\\
//----------------------------------------------------------------------------------------\\

display "-----initiating loop------"
//cycling through all observations which need computation, the code component all this has been building to
forvalues count_trough_all_obs = 1/ $num_of_obs_to_compute {




				//a crude progressbar: display if a third and two thirds of all observations have been reached
				if `count_trough_all_obs' == $third_of_computation {
				display "loop: progress = 33%"
				}
				if `count_trough_all_obs' == $two_thirds_of_computation {
				display "loop: progress = 66%"
				}
				if `count_trough_all_obs' == $num_of_obs_to_compute {
				display "loop: progress = 100%"
				}


//Because cycling through observations is computationally expensive, 
//before the main code starts, the code looks whether the current
//observation belogs to the group that should be switched
//obviously, this is also important for the results, but checking it this early is for performance
if direction_switch[`count_trough_all_obs'] == 1 {


	****************************************************************************************************************************************
	//This is the heart of the code: The actual switching routine. It takes all the possible codes this GP could have, stored in the ambig_*
	// variables and compares them with all the GP-codes which the `portfolio_holder' has in next years' portfolio
	//It consists of two parts: 
			//1. In the first, some locals storing portfolio borders are defined whenever the loop hits a new portfolio_holder to be switched
			
			//2. In the second, the code checks whether the observation is ambigous and then, if it is, it compares the ambigous codes with all
			//codes in next years' portfolio to find out about continuation. It uses this information to compute the outcome_var as well as 
			//the industry code

	
	
	//FIRST PART
	//if n == 1, define the locals which store the portfolio borders in which to to be check for the 'right' code
		if n[`count_trough_all_obs'] == 1  {
			local start_portfolio1 = `count_trough_all_obs'
			local end_portfolio1 = `count_trough_all_obs' + N[`count_trough_all_obs'] - 1
			local start_portfolio2 = `count_trough_all_obs' + N[`count_trough_all_obs']
			local end_portfolio2 = `start_portfolio2' + N[`start_portfolio2'] -1
			}

	
	//SECOND PART
	//Only execute when there are different ambigous codes in the data (otherwise, trying to exclude some is pointless)
		if `to'abig_count[`count_trough_all_obs'] > 1 {
		
		
		
		
					* 1. Find all codes which match the portfolio at the other side of the switch
					//since it truly seems unavoidable, the code now cycles through each of the amigvars, 
					//trying to find the GP code in the portfolio next year
					
					//this local count how many of the ambigious codes were "hit", i.e. present in the next year (idealy, it would only be one)
					local hit_codes = 0
					
					
						foreach ambigvar of varlist `to'_ambig* {

						
								//having selected a variable, we now compare it with next years' portfolio
								//However, to save on computing time, let us first ckeck whether it is not missing
								if `ambigvar'[`count_trough_all_obs'] != "" {
								
									//Operationalizing the comparison, we count how many times we find a matching code in the portfolio of the 
									//same portfolio-holder, but in the other year
									local hit = ""
									forvalues portfolio2_count = `start_portfolio2' / `end_portfolio2' {
										if `ambigvar'[`count_trough_all_obs']  == `to'[`portfolio2_count'] {

											local hit = "hit"
											}
										}
									
									
									//if, during one of these comparisons, `hitcount' was set to "hit", 
									//this should be counted as a hit and the relevant ambigvar should be changed
									if "`hit'" == "hit" {
									local hit_codes = `hit_codes' + 1
									quietly replace `ambigvar' = "`hit'" + `ambigvar'[`count_trough_all_obs'] in `count_trough_all_obs'
									}

									//close the if loop checking whether ambigvar was empty
									}
							//pick the next ambigvar and repeat the proccess
							}
						

					
			//close the loop requesting ambigous codes, because we have now (hopefully) resolved these problems
			}	

			
	//THIRD PART
	//Using the information gathered previously, try to construct the outcome_vars
	//----------------------------------------------------------------------------
		//counting how many products were at the border
		global products_at_the_border = $products_at_the_border + 1
	

		//UNAMBIGIOUS CODES:
		//------------------
			//if there was only code to begin with, than it easy: just use this code
			if `to'abig_count[`count_trough_all_obs'] == 1 {
			//count how often this happens
			global unambigious_prod = $unambigious_prod + 1
			foreach ambigvar of varlist `to'_ambig* {
			quietly replace `outcome_var' = substr("`to'",3,2) + substr(`ambigvar'[`count_trough_all_obs'],1,9) + `from'_last_digit[`count_trough_all_obs'] if substr(`ambigvar'[`count_trough_all_obs'],1,3)!="" in `count_trough_all_obs'
			}
			//from this, one can deduce the industry codes
			quietly replace `outcome_var'_4digit = substr(`outcome_var'[`count_trough_all_obs'], 3, 4) in `count_trough_all_obs'
			quietly replace `outcome_var'_3digit = substr(`outcome_var'[`count_trough_all_obs'], 3, 3) in `count_trough_all_obs'
			quietly replace `outcome_var'_2digit = substr(`outcome_var'[`count_trough_all_obs'], 3, 2) in `count_trough_all_obs'
			}
			
		
		//AMBIGIOUS CODES:
		//----------------
			//if the codes were ambigious, there are unfortunately a couple of cases:
			if `to'abig_count[`count_trough_all_obs'] > 1 {
			

			
				
				//1st case: 
				//--------
				//if only one code was hit, then it is easy: just replace the code
				if `hit_codes' == 1 {
				//count how often this happens
				global switched_ambigious_prod = $switched_ambigious_prod + 1
				foreach ambigvar of varlist `to'_ambig* {
				quietly replace `outcome_var' = substr("`to'",3,2) + substr(`ambigvar'[`count_trough_all_obs'],4,9) + `from'_last_digit[`count_trough_all_obs']  if substr(`ambigvar'[`count_trough_all_obs'],1,3)=="hit" in `count_trough_all_obs'
				}
				//from this, one can deduce the industry codes
				quietly replace `outcome_var'_4digit = substr(`outcome_var'[`count_trough_all_obs'], 3, 4) in `count_trough_all_obs'
				quietly replace `outcome_var'_3digit = substr(`outcome_var'[`count_trough_all_obs'], 3, 3) in `count_trough_all_obs'
				quietly replace `outcome_var'_2digit = substr(`outcome_var'[`count_trough_all_obs'], 3, 2) in `count_trough_all_obs'
				}
		
		
		
		
		
				//2nd case:
				//--------
				//if more than one code was hit, then it is unclear and we cannot merge the GP-code directly. 
				//However, we can still look whether at least the industry code is unique
				if `hit_codes' > 1 {
				//count how often this happens
				global ambigious_prod_after_eli = $ambigious_prod_after_eli + 1
				
				
				
				
				//collect all non-empty ambig-codes in a local
					//generate local for counting purposes
					local count_ambigs_compare = 0
					//count through all ambig-variables, see wheter they are filled, see whether they are legit, add them to a local
					foreach ambigvar of varlist `to'_ambig* {
					if substr(`ambigvar'[`count_trough_all_obs'],1,3)=="hit" {
						local count_ambigs_compare = `count_ambigs_compare' + 1
						if `count_ambigs_compare' == 1 {
							local testlocal1 = "substr(`ambigvar'[`count_trough_all_obs'],4 , wz_length )"
							}
						if `count_ambigs_compare' == 2 {
							local testlocal = "`testlocal1' == substr(`ambigvar'[`count_trough_all_obs'],4 , wz_length )"
							}
						if `count_ambigs_compare' > 2 {
							local testlocal = "`testlocal'" + "& `testlocal1' == substr(`ambigvar'[`count_trough_all_obs'],4 , wz_length )"
							}
						}
					}
				
				//4 digit industry code
				local var_first_wz_code = subinstr("`testlocal1'", "wz_length", "4", .)
				local condition = subinstr("`testlocal'", "wz_length", "4", .)
				quietly replace `outcome_var'_4digit = `var_first_wz_code' if `condition' in `count_trough_all_obs'
				quietly replace `outcome_var'_4digit = "unclear" if `outcome_var'_4digit[`count_trough_all_obs']=="" in `count_trough_all_obs'				
				//3 digit industry code
				local var_first_wz_code = subinstr("`testlocal1'", "wz_length", "3", .)
				local condition = subinstr("`testlocal'", "wz_length", "3", .)
				quietly replace `outcome_var'_3digit = `var_first_wz_code' if `condition' in `count_trough_all_obs'
				quietly replace `outcome_var'_3digit = "unclear" if `outcome_var'_3digit[`count_trough_all_obs']=="" in `count_trough_all_obs'
				//2 digit industry code
				local var_first_wz_code = subinstr("`testlocal1'", "wz_length", "2", .)
				local condition = subinstr("`testlocal'", "wz_length", "2", .)
				quietly replace `outcome_var'_2digit = `var_first_wz_code' if `condition' in `count_trough_all_obs'
				quietly replace `outcome_var'_2digit = "unclear" if `outcome_var'_2digit[`count_trough_all_obs']=="" in `count_trough_all_obs'
					
		
				//close the if `hit_codes' > 1 -clause
				}
						
						
				//3rd case
				//--------
				//Lastly, it might also be that no code was hit and that the comparison did not help at all 
				//However, we can still look whether at least the industry code is unique among the codes not hit
				if `hit_codes' == 0  & _merge[`count_trough_all_obs'] == 3 {
				//count how often this happens
				global unswitched_prod = $unswitched_prod + 1
				
				
				
				//collect all non-empty ambig-codes in a local
					//generate local for counting purposes
					local count_ambigs_compare = 0
					//count through all ambig-variables, see wtheter they are filled, see whether they are legit, add them to a local
					foreach ambigvar of varlist `to'_ambig* {
					if substr(`ambigvar'[`count_trough_all_obs'],1,3)=="hit" {
					display "found a hit ambigvar where local hit_codes was 0"
					error 1
					}
					if `ambigvar'[`count_trough_all_obs'] != "" {
						local count_ambigs_compare = `count_ambigs_compare' + 1
						if `count_ambigs_compare' == 1 {
							local testlocal1 = "substr(`ambigvar'[`count_trough_all_obs'],1 , wz_length )"
							}
						if `count_ambigs_compare' == 2 {
							local testlocal = "`testlocal1' == substr(`ambigvar'[`count_trough_all_obs'],1 , wz_length )"
							}
						if `count_ambigs_compare' > 2 {
							local testlocal = "`testlocal'" + "& `testlocal1' == substr(`ambigvar'[`count_trough_all_obs'],1 , wz_length )"
							}
						}
					}
				
				//4 digit industry code
				local var_first_wz_code = subinstr("`testlocal1'", "wz_length", "4", .)
				local condition = subinstr("`testlocal'", "wz_length", "4", .)
				quietly replace `outcome_var'_4digit = `var_first_wz_code' if `condition' in `count_trough_all_obs'
				quietly replace `outcome_var'_4digit = "unclear" if `outcome_var'_4digit[`count_trough_all_obs']=="" in `count_trough_all_obs'				
				//3 digit industry code
				local var_first_wz_code = subinstr("`testlocal1'", "wz_length", "3", .)
				local condition = subinstr("`testlocal'", "wz_length", "3", .)
				quietly replace `outcome_var'_3digit = `var_first_wz_code' if `condition' in `count_trough_all_obs'
				quietly replace `outcome_var'_3digit = "unclear" if `outcome_var'_3digit[`count_trough_all_obs']=="" in `count_trough_all_obs'
				//2 digit industry code
				local var_first_wz_code = subinstr("`testlocal1'", "wz_length", "2", .)
				local condition = subinstr("`testlocal'", "wz_length", "2", .)
				quietly replace `outcome_var'_2digit = `var_first_wz_code' if `condition' in `count_trough_all_obs'
				quietly replace `outcome_var'_2digit = "unclear" if `outcome_var'_2digit[`count_trough_all_obs']=="" in `count_trough_all_obs'
				

				//close the if `hit_codes' > 1 -clause
				}	
				
				
			//this closes the chapter on how to deal with ambigous codes in variable construction
			}
			
			
		//This is generally the end of the third chapter: outcome_var variable creation
	
		
	****************************************************************************************************************************************		
			
			
	//close the *main*, i.e. second part of the loop (= the if-statement determining whether this observation should be treated at all)
	}

//close the loop cycling through all observations (i.e pick the next observation)
}


//change the outcome_var for observations not at the rift
//-------------------------------------------------------
		display "-----computing outcome vars------"

	*1. Fill in the classification by taking the changes made at the rift
	
		//only execute this part if there were actually any changes at the rift
		if $num_of_obs_to_compute > 0 {
		quietly {
		//Make a group variable gathering all observations of a unique product in unbroken chain of reporting
		//Because we assume that all observations part of such a group can safely take the `outcome_var' value 
		//of the observation in the group that was part of the rift.
		//this is tricky because there is no guarantee that "bnr jahr final_gp02" is unique and we thus cannot use xtset
		
		//to still be able to xtset, we have to find a unique variable combination for xtset 
		//Thus, we define an additional varibale that gives each occurence of the same `source_var' `portfolio_holder' a number
		sort `source_var' `portfolio_holder' jahr
		by `source_var' `portfolio_holder' jahr: gen final_gp_help_n = _n
		egen ID_var_progr_hlp = group(`source_var' `portfolio_holder' final_gp_help_n)

		//using this new "identifier", we can now define the uninterrupted string of product reports belongs to
		//however, this will only work for the first occurrence of each product in a portfolio, because we artificially gave 
		//the other strings a different identifier
		xtset ID_var_progr_hlp jahr
		by ID_var_progr_hlp: gen n_end_of_prog = _n if final_gp_help_n ==1
		gen another_hlp_ID = jahr - n_end_of_prog - $start_year_load + 2 if final_gp_help_n ==1
		egen ID_for_strings_of_products = group (ID_var_progr_hlp another_hlp_ID) if final_gp_help_n ==1
		
		
		//next, we have to solve this problem that some observations had the "wrong" identifier and did not get 
		//information on to which string they belong
		//Thus, we spread this information to all varibales which belong to the same product in the same year
		
		//First, we find out whether that actually happens
		quietly su final_gp_help_n 
		
		//If this did happen and pe_13 was the source_var, this is impossible, we should terminate and check
		if "`source_var'" == "pe_13" & `r(max)' > 1 {
			noisily display "program switch GPs has found repeated observations of the same product in the same year"
			noisily display "this happened when spreading the information about products obtained at the rift to more observations"
			noisily display "it should be impossible. Terminated program"
			error 1
			}
		//If this did happen and final_gp02 was the source_var, we should spread the info and check whether that worked
		if "`source_var'" == "final_gp02" & `r(max)' > 1 {
			//spread the information
			sort `source_var' `portfolio_holder' jahr
			by `source_var' `portfolio_holder' jahr: egen spreader= max(ID_for_strings_of_products)
			replace ID_for_strings_of_products = spreader if ID_for_strings_of_products ==.
			drop spreader
		
				//check whether all obs now have a string of products
				quietly count if ID_for_strings_of_products ==.
				if `r(N)' > 0 {
				noisily display "still missing observations in the ID_for_strings_of_products after spread"
				error 1
				}
		}

		
		
		//before we replace, let us count the number of outcome_vars for documentational reasons
		count if `outcome_var' != ""
		local hlpcounting = `r(N)'
		
		
			//replace the vars

			bys ID_for_strings_of_products: egen `outcome_var'_spreader = mode(`outcome_var')
			by ID_for_strings_of_products: egen `outcome_var'_2digit_spreader = mode(`outcome_var'_2digit)
			by ID_for_strings_of_products: egen `outcome_var'_3digit_spreader = mode(`outcome_var'_3digit)
			by ID_for_strings_of_products: egen `outcome_var'_4digit_spreader = mode(`outcome_var'_4digit)
			
			
			gen marker_existing_outcome = 1 if `outcome_var' !=""
			bys ID_for_strings_of_products: egen control_Nr_outcomes = count( marker_existing_outcome)
			quietly su control_Nr_outcomes
			if `r(max)' > 1 {
			
			noisily display "more than one outcome per group where it should be spread. Something seems wrong"
			noisily display "if the source var was final_gp02, this might be legit because of two observations at the rift year"
			noisily display "source var:`source_var'"
			noisily display "However, these two observations should be identical in terms of outcome_var. "
			noisily display "After all, they have the same code and the same possibilities. This is counted here:"
			noisily count if (`outcome_var'_spreader != `outcome_var' & `outcome_var' !="") | (`outcome_var'_2digit_spreader != `outcome_var'_2digit & `outcome_var'_2digit !="") /*
			*/ | (`outcome_var'_3digit_spreader != `outcome_var'_3digit & `outcome_var'_3digit !="") | (`outcome_var'_4digit_spreader != `outcome_var'_4digit & `outcome_var'_4digit !="")
			
			}
			replace `outcome_var' = `outcome_var'_spreader
			replace `outcome_var'_2digit = `outcome_var'_2digit_spreader
			replace `outcome_var'_3digit = `outcome_var'_3digit_spreader
			replace `outcome_var'_4digit = `outcome_var'_4digit_spreader
		
		
		//after the replace, let us again count `outcome_var' to see how many were replaced due to the outcome_var generated at the rift
		count if `outcome_var' != ""
		local obs_dragged_from_rift	= `r(N)' - `hlpcounting'
		
		}
		}
		
		

	*2. Fill in the outcome_var if the classification is unambigious anyway
		
		quietly {
		//before we replace, let us count the number of outcome_vars for documentational reasons
		count if `outcome_var' != ""
		local hlpcounting = `r(N)'
		foreach ambigvar of varlist `to'_ambig* {
			replace `outcome_var' = substr("`to'",3,2) + substr(`ambigvar',1,9) + `from'_last_digit  if `ambigvar' !="" & `to'abig_count == 1 & `outcome_var' ==""
			}
		replace `outcome_var'_4digit = substr(`outcome_var',3,4) if `outcome_var'_4digit ==""  & `to'abig_count == 1
		replace `outcome_var'_3digit = substr(`outcome_var',3,3) if `outcome_var'_3digit ==""  & `to'abig_count == 1
		replace `outcome_var'_2digit = substr(`outcome_var',3,2) if `outcome_var'_2digit ==""  & `to'abig_count == 1
		
		//after the replace, let us again count `outcome_var' to see how many were replaced by this
		count if `outcome_var' != ""
		local obs_then_unambigious	= `r(N)' - `hlpcounting'
		}

		
	*3. Fill in the outcome_var if the observation has the right classification anyway
	
		quietly {
		//before we replace, let us count the number of outcome_vars for documentational reasons
		count if `outcome_var' != ""
		local hlpcounting = `r(N)'
		//replace
		replace `outcome_var' = substr("`to'",3,2) + `to' + `to'_last_digit if flag_`to' == 1
		replace `outcome_var'_4digit = substr(`outcome_var',3,4) if `outcome_var'_4digit =="" & flag_`to' == 1
		replace `outcome_var'_3digit = substr(`outcome_var',3,3) if `outcome_var'_3digit =="" & flag_`to' == 1
		replace `outcome_var'_2digit = substr(`outcome_var',3,2) if `outcome_var'_2digit =="" & flag_`to' == 1
		//after the replace, let us again count `outcome_var' to see how many were replaced by this
		count if `outcome_var' != ""
		local obs_right_from_start	= `r(N)' - `hlpcounting'
		}

	*4. Give up on filling in the outcome_var
		
		quietly {
		//before we replace, let us count the number of outcome_vars for documentational reasons
		count if `outcome_var' != ""
		local hlpcounting = `r(N)'
		replace `outcome_var' = substr("`from'",3,2) + `from' + `from'_last_digit if flag_`from' == 1 & `outcome_var' =="" & _merge ==3
		//after the replace, let us again count `outcome_var' to see how many were replaced by this
		count if `outcome_var' != ""
		local obs_undetermined	= `r(N)' - `hlpcounting'
		
		//however, all is not lost. Let us see how many wz-s we can actually compute
		//eventhough we might have more than one code, they might still all be in the same wz
		
		//therefore, here we construct an if-clause that controls whether all ambigvars have the same wz_length digit
		local testlocal = ""
		foreach ambigvar of varlist `to'_ambig* {
			if "`ambigvar'" != "`to'_ambig1" {
				replace `ambigvar' = subinstr(`ambigvar', "hit", "", .)
				local testlocal = `"`testlocal'"' + `"& ((substr(`to'_ambig1 ,1 , wz_length ) == substr(`ambigvar' ,1 , wz_length ))| `ambigvar' == "" ) "'
				}
			}
		
		//try the wz4
		local condition_wz4 = subinstr(`"`testlocal'"', `"wz_length"', `"4"', .)
		replace `outcome_var'_4digit = substr(`to'_ambig1,1,4) if `outcome_var'_4digit == "" & substr(`outcome_var',1,2) == substr("`from'",3,2) & _merge ==3 `condition_wz4'
		replace `outcome_var'_4digit = "unclear" if `outcome_var'_4digit == "" & substr(`outcome_var',1,2) == substr("`from'",3,2) & _merge ==3
		
		//try the wz3
		local condition_wz3 = subinstr(`"`testlocal'"', "wz_length", "3", .)
		replace `outcome_var'_3digit = substr(`to'_ambig1,1,3) if `outcome_var'_3digit == "" & substr(`outcome_var',1,2) == substr("`from'",3,2) & _merge ==3 `condition_wz3'
		replace `outcome_var'_3digit = "unclear" if `outcome_var'_3digit == "" & substr(`outcome_var',1,2) == substr("`from'",3,2) & _merge ==3
		
		//try the wz2
		local condition_wz2 = subinstr(`"`testlocal'"', "wz_length", "2", .)
		replace `outcome_var'_2digit = substr(`to'_ambig1,1,2) if `outcome_var'_2digit == "" & substr(`outcome_var',1,2) == substr("`from'",3,2) & _merge ==3 `condition_wz2'
		replace `outcome_var'_2digit = "unclear" if `outcome_var'_2digit == "" & substr(`outcome_var',1,2) == substr("`from'",3,2) & _merge ==3
		
		
		count if `outcome_var' !="" & _merge !=3 & flag_`from' == 1 
		if `r(N)' >0 {
		display "some codes were switched where _merge was not 3. THis indicates a problem with the program"
		error 1
		}
		
		count if _merge !=3 & flag_`from' == 1 
		local concordance_failures = `r(N)'
		replace `outcome_var' = substr("`from'",3,2) + `from' + `from'_last_digit if flag_`from' == 1 & `outcome_var' =="" & _merge !=3
		replace `outcome_var'_4digit = "unclear" if `outcome_var'_4digit == "" & _merge !=3 & flag_`from' == 1
		replace `outcome_var'_3digit = "unclear" if `outcome_var'_3digit == "" & _merge !=3 & flag_`from' == 1 
		replace `outcome_var'_2digit = "unclear" if `outcome_var'_2digit == "" & _merge !=3 & flag_`from' == 1

		}
		
		
//dropping variables created during the process


//this variable only exists if there was indeed a rift and some info was carried over
capture drop ID_var_progr_hlp final_gp_help_n n_end_of_prog another_hlp_ID ID_for_strings_of_products /*
*/ marker_existing_outcome control_Nr_outcomes `outcome_var'_spreader /*
*/ `outcome_var'_2digit_spreader `outcome_var'_3digit_spreader `outcome_var'_4digit_spreader 

//these can always be dropped
drop `from' `to' `from'_last_digit `to'_last_digit flag_switch_group_prog direction_switch `to'_ambig* n N `to'abig_count /*
*/ max_from max_to  _merge


//this is just a report for the user at the end
display "At the rift, it tried to switch $products_at_the_border codes."
display "Of these, $unambigious_prod were unambigious according to the concordance"
display "Of the others, $switched_ambigious_prod were unambigious after eliminating possibile codes not "
display "present in the other year of the rift. "
display "$ambigious_prod_after_eli codes could not be switched, because more than one possible code "
display "also showed up next year."
display "$unswitched_prod codes could not be switched because the production portfolio at theother side "
display "of the rift had none of the codes that the concordance suggested. Among these, there are the" 
display "truly discontinued products"
display "-----------------------------------------------------------------------------------------------"
display "Concerning the data not at the rift:"
display "`obs_dragged_from_rift' codes could be switched by taking the results from the rift comparison"
display "`obs_then_unambigious' additional codes could be switched because they were unambigious"
display "`obs_right_from_start' had the right classification anyway"
display "`obs_undetermined' had to be left unchaged"
display "of these, the codes of `concordance_failures' obs could not be found in the concordance"

//
end
}
//



******************************************
*          Program 10: gpTreeDist         *
******************************************
{
/*
* program berechnet die Distanz zwischen 2 GP-SchlÃ¼sseln 
* -> GP-Systematik als Baumstruktur
* -> wenn keine Ãœbereinstimmung in oberster Ebene (1. Stelle im GP-SchlÃ¼ssel) -> Distanz = 8
* -> wenn vollstÃ¤ndige Ãœbereinstimmung -> Distanz = 0
*/
capture program drop gpTreeDist
program define gpTreeDist, rclass
	args g1 g2 variable
	
	local tree_dist = 0
	display `variable'[`g1']
	display `variable'[`g2']
	
	
	************************************************************************************************
	* beginne an der letzten Position des GP-SchlÃ¼ssels und Vergleiche die codes bis zu diesem Punkt
	// wenn der ganze SchlÃ¼ssel bis zum 10-Steller gleich ist, dann ist die Distanz 0
	************************************************************************************************
	
	//depending on which variable contains the GP code, different parts of the variable contain it
	//this if-clause contains the right comparison for a final_gp02, where the first two digits contain
	//information on the classification and should not feature in the GP-comparison
	if "`variable'" == "final_gp02" {

			forvalues i = 9(-1)2 {
					if substr(`variable'[`g1'], 3, `i') == substr(`variable'[`g2'], 3, `i') {
					local tree_dist = 8 - `i'
					continue, break
					}
				}
		
				if substr(`variable'[`g1'], 3 , 2) != substr(`variable'[`g2'], 3, 2) {
				local tree_dist = 8
				}
		}


	//depending on which variable contains the GP code, different parts of the variable contain it
	//this if-clause contains the right comparison for a pe_13, where the GP-codes is the start of the
	//variable
	if "`variable'" == "pe_13" {
			forvalues i = 9(-1)2 {
					if substr(`variable'[`g1'], 1, `i') == substr(`variable'[`g2'], 1, `i') {
					local tree_dist = 8 - `i'
					continue, break
					}
				}
		
				if substr(`variable'[`g1'], 1 , 2) != substr(`variable'[`g2'], 1, 2) {
				local tree_dist = 8
				}
		}
	
	display "GP-Tree-Distance between obs `g1' and obs `g2' is `tree_dist'"
	
	return local tree_dist = `tree_dist'
	
end
}
//


******************************************
*     Program 11: portfolio_movement     *
******************************************
{
/*
* program berechnet die durchschnittlich Distanz zwischen zwei Produktportfolios
* diese ist definitert als Summe der Distanzen von allen unwichtiger werdenden 
* GÃ¼tern zu allen wichtiger werdenden, gewichtet mit der Bedeutung der jeweiligen Bewegung
*/
capture program drop portfolio_movement
program define portfolio_movement, rclass
	args portfolio_holder portfolio_sequencer unique_product_ident importance_signifier outcome_var
	
display "The program portfolio_movement has been called. It computes the average distance "
display "in product codes to move from one portfolio to the other, given that the portfolio"
display "share of each discontinued product is distributed equally amongst the new products."
display "Portfolios which have `portfolio_holder' in common, but are seperated by different"
display "values of `portfolio_sequencer' will be compared. A unique product is given by the "
display "combination of `unique_product_ident'.  The importance of each product in each "
display "portfolio is derived from `importance_signifier'. The resulting portfolio movement "
display " is stored in `outcome_var'"

//check whether the product code is contained in pe_13 or in final_gp02

foreach potential_GP_var in `unique_product_ident' {
if "`potential_GP_var'" == "pe_13" {
local GP_var = "pe_13"
}
if "`potential_GP_var'" == "final_gp02" {
local GP_var = "final_gp02"
}
}


display "`potential_GP_var'"

quietly {

//generate a variable denoting a unique product
egen unique_product_ident = group( `unique_product_ident' `portfolio_holder')


//insert earlier products with weight zero to allow an xtset to reference to discontinued products
quietly xtset unique_product_ident `portfolio_sequencer'
expand 2 if l.unique_product_ident == .
bysort unique_product_ident `portfolio_holder' `portfolio_sequencer' : gen n_duplis = _n
foreach var in  `importance_signifier'  {
replace  `var' = 0 if  n_duplis == 2
}
replace `portfolio_sequencer' = `portfolio_sequencer' - 1 if n_duplis == 2



//insert unexisting products with weight zero one year before their launch to allow an xtset to reference to products launched later
quietly xtset unique_product_ident `portfolio_sequencer'
expand 2 if f.unique_product_ident == .
bysort unique_product_ident `portfolio_holder' `portfolio_sequencer' : gen n_duplis2 = _n
foreach var in  `importance_signifier'  {
replace  `var' = 0 if  n_duplis2 == 2
}
replace `portfolio_sequencer' = `portfolio_sequencer' +1 if n_duplis2 == 2


//-------------generate a marker designating the portfolio and other portfolio information
//----------------------------------------------------------------------------------------

//generate a marker for each portfolio to use with bysort-commands
egen portfolio_NR_`portfolio_holder' = group(`portfolio_holder' `portfolio_sequencer' )
//generate portfolio positions for individual products
sort portfolio_NR_`portfolio_holder' unique_product_ident
by portfolio_NR_`portfolio_holder' : gen n = _n
by portfolio_NR_`portfolio_holder' : gen N = _N




* Anteil des Produktes (GP95) am gesamten Absatzproduktionswertes fÃ¼r das jeweilige Jahr
by portfolio_NR_`portfolio_holder': egen weight_product = sum(`importance_signifier')  
replace weight_product = `importance_signifier' / weight_product
replace weight_product = 0 if weight_product == .



//generate a variable denoting the product weight change
quietly xtset unique_product_ident `portfolio_sequencer'
gen product_change = f.weight_product - weight_product
replace product_change = 0 if f.weight_product == .
//replace product_change = f.product_change

//generate a measure of the relative importance of the various codes within the family of codes with a growing portfolio share
sort portfolio_NR_`portfolio_holder' unique_product_ident
by portfolio_NR_`portfolio_holder': egen importance_within_growing = total(product_change) if product_change>0
replace importance_within_growing = product_change/importance_within_growing


//generate control variable
by portfolio_NR_`portfolio_holder': egen total_product_change = total(product_change)

//control whether total_product_change is either zero (continuation), 1 (entry) or -1 (exit)
count if (total_product_change < 0.99 & total_product_change > 0.01) | (total_product_change > - 0.99 & total_product_change < -0.01)
if `r(N)' > 0 {
display "ERROR MESSAGE"
display "The program portfolio_movement has been called. It computes the average distance "
display "in product codes to move from one portfolio to the other, given that the portfolio"
display "share of each discontinued product is distributed equally amongst the new products."
display "Portfolios which have `portfolio_holder' in common, but are seperated by different"
display "values of `portfolio_sequencer' will be compared. A unique product is given by the "
display "combination of `unique_product_ident'.  The importance of each product in each "
display "portfolio is derived from `importance_signifier'. The resulting portfolio movement "
display " is stored in `outcome_var'"
display "-------------------------------------------------------------------------"

display "WARNING: Product portfolio changes do not add up to legit values (-1;0;1). Computation seems of"
display "Parameters: `portfolio_holder' `portfolio_sequencer' `unique_product_ident' `importance_signifier' `outcome_var'"
kdensity total_product_change
}

replace total_product_change = 0 if total_product_change <0.01 & total_product_change > -0.01
replace total_product_change = 1 if total_product_change <1.01 & total_product_change > 0.99
replace total_product_change = -1 if total_product_change > - 1.01 & total_product_change < -0.99
tab total_product_change

//to compress runtime, delete the observations generated solely for xtset
by portfolio_NR_`portfolio_holder': egen total_portfolio_weight = total(pe_15)
drop if total_portfolio_weight == 0
drop  n_duplis n_duplis2 total_portfolio_weight
//control values again
tab total_product_change

//generate target variable (portfolio spread between the old and the new portfolio)
gen portfolio_spread = .

//cycle through all observations
sort portfolio_NR_`portfolio_holder' n
}
//generate a helplocal for a crude progressbar 
local next_progress_display = 10
//find the number of observations for the loop
quietly count
global num_of_obs = `r(N)' 

forvalues count_trough_all_obs = 1/ $num_of_obs {

//display a crude progressbar: show which observation has been reached
local progress = `count_trough_all_obs' / $num_of_obs *100
if `progress' > `next_progress_display' {
display "`next_progress_display'%"
local next_progress_display = `next_progress_display' + 10
}


//if n == 1, close the old comparison and save it in the previous observation
	if n[`count_trough_all_obs'] == 1  {

		//save old local 
		if `count_trough_all_obs' > 1 {
		local hlp = `count_trough_all_obs' - 1
			quietly replace portfolio_spread = `portfolio_spread' in `hlp'
			}

			
		local portfolio_spread = 0
		local start_portfolio = `count_trough_all_obs'
		local end_portfolio = `count_trough_all_obs' + N[`count_trough_all_obs'] - 1

		}

		
//measure the avg distance from all products losing portfolio shares to those that gained them, weighted with the importance of the link
local product_dist = 0
if product_change[`count_trough_all_obs'] < 0 {
	forvalues count_portfolio = `start_portfolio' / `end_portfolio' {
		if  product_change[`count_portfolio'] > 0 {
			quietly gpTreeDist `count_trough_all_obs' `count_portfolio' `GP_var'
			local product_dist = `product_dist ' + `r(tree_dist)' * importance_within_growing[`count_portfolio']
			}
		//close the loop through the products which experienced positive changes in their importance
		}
	//weight the sum of these products with
	local portfolio_spread = `portfolio_spread' + (`product_dist' * (-1) * product_change[`count_trough_all_obs'])
	}
//close the loop cycling through all observations
}
//generate the outcome variable and clean up the dataset

kdensity portfolio_spread 

su portfolio_spread 

by portfolio_NR_`portfolio_holder': egen hlp = max(portfolio_spread)

//gen the final outcome var
gen `outcome_var' = hlp

//cleanup all variables created during the programm
drop hlp portfolio_spread unique_product_ident n N weight_product product_change importance_within_growing total_product_change

end
}
//


******************************************
*    Program 12: gen_weights_reg_plus    *
******************************************
{

/*
This program takes a list of variables, checks for each observation whether they exist and thus whether they will be used.

It takes as arguments 1 the original regression command
*/

capture program drop gen_weights_reg_plus
program define gen_weights_reg_plus, rclass

args estimation

capture  `estimation'

//if estimation works, construct the weights
if _rc == 0 {
cap drop flag_valid
gen flag_valid = e(sample)

cap drop weight
quietly bysort unr jahr flag_valid: gen number = _n
quietly gen valid_unique_obs = 1 if flag_valid == 1 & number==1
quietly replace valid_unique_obs = 0 if valid_unique_obs ==.
quietly bysort size_bin_labor wz_u jahr: egen num_valid_obs = total(valid_unique_obs)

quietly gen weight = true_num_firms/num_valid_obs if flag_valid == 1
replace weight =1 if $start_year_estimate <= $year_no_weight

if "$IWH_run" == "YES"   {

replace weight =1
}

drop num_valid_obs flag_valid valid_unique_obs number


capture noisily drop residual
capture noisily drop ydfe 
capture noisily drop wzdfe
capture noisily drop BLdfe
 
local estimation2 = subinstr("`estimation'" , "," , " if weight !=. [pweight = weight], " , .)
local estimation2 = subinstr("`estimation2'" , " if " , " %%% " , 1)
local estimation2 = subinstr("`estimation2'" , " if " , " & " , .)
local estimation2 = subinstr("`estimation2'" , "%%%" , " if " , 1)
display "THIS IS THE WEIGHTED REGRESSION NOW"
display `" `estimation2'"'

 capture noisily `estimation2'  // in case after weights it breaks down ( weights != . can create singleton)
			 if _rc == 0 {
			 return local weight_status = "succeeded"
			 }
			 
			  if _rc != 0 {
			 return local weight_status = "failed"
			 }
 
}
//


if _rc != 0 {

log using "$savepath\freigabe\Log_Error.log", append name(Log_Error)
/*
This log has been initiated by gen_weights_reg because the command failed to compute weights.
this happened because the initial reg-command failed for some reason. The following log 
should help clarify the cause of the error

*/
capture noisily drop residual
capture noisily drop ydfe 
capture noisily drop wzdfe
capture noisily drop BLdfe

noisily display "Programm gen_weights_reg has failed to drop singleton dummies"
noisily display "the regression command called did not work, an error message follows"
noisily display "The estimation command was: `estimation'"

 noisily display "`estimation' "
 noisily display "WENT WRONG"

return local weight_status = "failed"

log close Log_Error
}
//


end
}
//



******************************************
*    Program 13: outreg2_all_stages      *
******************************************
{

/*
This program takes a list of variables, checks for each observation whether they exist and thus whether they will be used.

It takes as arguments 1 the original regression command
*/

capture program drop outreg2_all_stages
program define outreg2_all_stages, rclass

args location outreg_options



capture noisily {

forvalues double_space_count = 1/10 {
local location = subinstr("`location'","  "," ",.)
}

//only proceed if the previous regression worked
if "`r(weight_status)'" != "failed" {
	display "within if clause"
//prepare first stage regressions
//------------------------------
	//read out the number of first-stage-regressions we need to capture
	if "`e(endog_ct)'" != "" {
			local max_first_stage_count = `e(endog_ct)'
			//generate a filename for them
			local location_first_stage = subinstr("`location'","${savepath}\Freigabe","${savepath}\Freigabe\FIRST_STAGES",.)
			local location_first_stage = subinstr("`location_first_stage'",".xls","FIRST_STAGES.xls",.)
			//This generates the appropriate folder in which to store these estimates
			local path = subinstr("`location_first_stage'","${savepath}\Freigabe\FIRST_STAGES\","",.)
			tokenize "`path'", p("\")
			local path = "${savepath}\Freigabe\FIRST_STAGES\\\`1'"
			capture mkdir "`path'"
			display "made path"
			
			//check whether file already exists and store number of first free file
			//-------------------------------------------------------------
			local first_free_file = 0
			forvalues checkcount = 1/200 {
			
				//confirm that the file exists
				local location_first_stage = subinstr("`location_first_stage'","FIRST_STAGES.xls","FIRST_STAGES`checkcount'.xls",.)
				forvalues double_space_count = 1/10 {
					local location_first_stage = subinstr("`location_first_stage'","  "," ",.)
					}

				capture confirm file `"`location_first_stage'"'
				
				//If he did not find the file, it is still free. Save this number
				//since all the following files will also be free, 
				//we must prevent him from overwriting this number later
				if _rc != 0 & `first_free_file' == 0{
					local first_free_file = `checkcount'
					}
					
				//reset the local with the location so that the next loop can insert the next number again
				local location_first_stage = subinstr("`location_first_stage'","FIRST_STAGES`checkcount'.xls","FIRST_STAGES.xls",.)
				}

			//since  the location-local contains now the maximum number of slots plus one, we need to change that to the free slot
			local location_first_stage = subinstr("`location_first_stage'","FIRST_STAGES.xls","FIRST_STAGES`first_free_file'.xls",.)
			
			}

//outreg the original estimate
//-------------------------------
display "debug"


if "`e(widstat)'" =="" {
local outreg_options = subinstr("`outreg_options'", "Weak Ident Test, `e(widstat)'", "Weak Ident Test, 0",.)
}

capture	noisily outreg2 using "`location'"  `outreg_options'

display `"capture	noisily outreg2 using "`location'"  `outreg_options'"'

//check whether any variables that should be there were omitted in the estimation, which would mess up the count of first stages

display " foreach endog_regressor in `e(instd)' {"
foreach endog_regressor in `e(instd)' {
	display "if  _b[`endog_regressor'] != 0 {"
	display  _b[`endog_regressor']
	if  _b[`endog_regressor'] != 0 {
	local con_`endog_regressor' = "YES"
	}
	}
	
//set the options to replace for the first fistr-stage file
local outreg_options = subinstr( "`outreg_options'" , "append" , "replace" ,.)
//first stages do not have a weak ident test
local outreg_options = subinstr( "`outreg_options'" , "`e(widstat)'" , "0" ,.)


//outreg the first stages subsequently
//-------------------------------
local first_stage_count = 0
foreach endog_regressor in `e(instd)' {
	//count so that we know the number of first stages we are at now
	local first_stage_count = `first_stage_count' +1
	display "saving first-stage estimates `first_stage_count'"
	
	//only try to find a first stage if the variable is not ommited
	if "`con_`endog_regressor''"  == "YES" {
		display "`endog_regressor' not ommited"
	
		//load the appropriate first stage into active memory
		capture noisily estimates restore _ivreg2_`endog_regressor'
		if _rc != 0 {
			capture noisily estimates restore reghdfe_first`first_stage_count'
			if _rc != 0 {
				capture noisily estimates dir
				}
			}
	
			
		//overwrite the "replace" command for all but the first first stage. 
		//Otherwise, there would be only one equation for the first regression stored in the file
		if `first_stage_count' > 1 {
			local outreg_options = subinstr( "`outreg_options'" , "replace" , "append" ,.)
			}
		
		local outreg_options = subinstr("`outreg_options'", "addtext(",       "addtext(endog_reg, y:`endog_regressor' , "                              , .)
		//outreg the regression in active memory
		capture noisily outreg2 using "`location_first_stage'"  `outreg_options'
		local outreg_options = subinstr("`outreg_options'"       ,            "addtext(endog_reg, y:`endog_regressor' , "           , "addtext("       , .)
		}
}


}



}
capture noisily estimates drop *

capture noisily drop weight

end
}
//


******************************************
*    Program 14: bs_weights_reg_plus    *
******************************************
{

/*
This program takes a list of variables, checks for each observation whether they exist and thus whether they will be used.
ori_id and new_id and xtset,clear need to be defined beforehand
It takes as arguments 1 the original regression command
*/

capture program drop bs_weights_reg_plus
program define bs_weights_reg_plus, rclass

args estimation

capture  `estimation'

//if estimation works, construct the weights
if _rc == 0 {
cap drop flag_valid
gen flag_valid = e(sample)

preserve

keep if flag_valid ==1

bootstrap,  cluster(ori_id) idcluster(new_id) rep(5) seed(20091990): gen_weights_reg_plus "`estimation'"


restore





 
}
//


if _rc != 0 {

/*
This log has been initiated by gen_weights_reg because the command failed to compute weights.
this happened because the initial reg-command failed for some reason. The following log 
should help clarify the cause of the error

*/
capture noisily drop residual
capture noisily drop ydfe 
capture noisily drop wzdfe
capture noisily drop BLdfe

noisily display "Bootstrap could not be performed"
noisily display "*********************************"



}
//


end
}
//






}
//







//////////////////////////ACTUAL DATA WORK STARTS HERE


//LOADING IN MAIN DATASET AND REDUCING IT
{
  
  
//----------Firm Data-----------\\
//-------------------------------\\
{


if  "$IWH_run" != "YES"  &  "$start_new_data" == "YES"    {



use  unr bnr jahr mb* kse_* pib_*  piu_* wz_* ags_* bl_* /*
*/  jbu_* ieu_* kse_* pe pe_* vse_ef* ieu ieb kse jbu if  jahr >= $start_year_load  & $end_year_load >= jahr   using "$Firmendaten" , clear


cap noisily drop ags_b
cap noisily drop land_b
cap noisily drop ade_b
cap noisily drop wz_b
cap noisily drop hg_b


ren pib_ags ags_b
ren pib_land land_b 
ren pib_ade ade_b
ren pib_wz  wz_b
ren pib_hg  hg_b

ren piu_ags ags_u
ren piu_land land_u 
ren piu_ade ade_u
ren piu_wz  wz_u
ren piu_hg  hg_u
ren piu_bgkl bgkl_u




drop if ieu == . & kse ==. & pe ==.  & mb ==. & ieb ==.



order unr bnr jahr





duplicates drop

compress _all



save "$savepath\working_file_firms.dta", replace



global Firmendaten "$savepath\working_file_firms.dta"

keep if  jahr >= $start_year_load  & $end_year_load >= jahr 


use  "$Produktdaten" , clear

destring pe_16 , replace
destring pe_21 , replace
destring pe_26 , replace


duplicates drop
compress _all


save "$savepath\working_file_products.dta", replace

global Produktdaten "$savepath\working_file_products.dta"



}

if  "$IWH_run" != "YES"  &  "$start_new_data" != "YES"    {
global Firmendaten "$savepath\working_file_firms.dta"
global Produktdaten "$savepath\working_file_products.dta"

}



}
//close decompose firm data

  
 
  
//In diesem Abschnitt werden die merge-Tabellen aufgespalten. Dies erlaubt einen einfacheren 
//Zugang zu den Daten und ein schnelleres Mergen, da nicht mehr die ganz Datei durchsucht werden muss.
if "$decompose_wz_data" == "YES" {

//----------------------------------------------------------------------\\
//------------------------------Rebase indices--------------------------\\
//----------------------------------------------------------------------\\
{




global Merkmale_WZ = "WZ_Merkmale 2020-06-08"

use  "$datapath\\\${Merkmale_WZ}.dta" , clear




foreach index in  Price_Index_Invest_Anlagen Price_Index_Invest_Ausr Price_Index_Invest_Bauten /*
*/ Price_Index_Invest_Sonstige Abschreibungspreisindex Price_Index_Rev   /*
*/ Vorleistungsindex CPI_2001_Aggregate  {

	quietly gen  hlp = `index' if jahr == $start_year_estimate
	sort WZ2008 jahr
	quietly by WZ2008: egen index_base = max(hlp)
	drop hlp
	
	
	
	//count whether this would destroy information
	quietly count if index_base == . & `index' != .
	local test = `r(N)'
	if `test' != 0 {
		display "`index' could not be rebased. Was set to missing were necessary"
		}
		
		
		

	
	quietly replace `index' = `index'   *   100/index_base 
	
	drop index_base
	}
	//
	

save  "$savepath\Merkmale_WZ" , replace
}
//



//----------------------------------------------------------------------\\
//------------------------------Concordances----------------------------\\
//----------------------------------------------------------------------\\
{
	
	//get concordance for ags by using purpose =  "AGS (year) to AGS (2018)" 
	// where Codetype 1 is the year of ags (code1) to be translated in into ags 2018 (code2)
	//codetype 2 is the factor, i.e. the share of the population that was sorted into the new ags
	
	
	

use "$datapath\Merkmalstabelle Concordances", clear





//create a global containing a list of all Concordances in the Concordances 
levels (Purpose), local(PurposeList)

foreach Purpose in `PurposeList' {






use "$datapath\Merkmalstabelle Concordances" if Purpose == "`Purpose'" ,clear





if Purpose != "AGS (year) to AGS (2018)"  {

//check whether Concordance only contains one kind of codes (Code1)
	egen mode_Code1Type = mode(Code1Type)
	count if mode_Code1Type != Code1Type
		if `r(N)'>0 {
		display "more than one kind of code detected in Code1Type"
		error 1
		}
//check whether Concordance only contains one kind of codes (Code2)
	egen mode_Code2Type = mode(Code2Type)
	count if mode_Code2Type != Code2Type
		if `r(N)'>0 {
		display "more than one kind of code detected in Code2Type"
		error 1
		}

//rename variables and drop every variable but the two codes
local name_code1 = Code1Type
rename Code1 `name_code1'
local name_code2 = Code2Type
rename Code2 `name_code2'

//clean the first code by eliminating commas, dots etc
replace `name_code1' = subinstr(`name_code1',".","",.)
replace `name_code1' =  subinstr(`name_code1'," ","",.)
replace `name_code1' =  subinstr(`name_code1',",","",.)
replace `name_code1' =  subinstr(`name_code1',";","",.)
//clean the second code by eliminating commas, dots etc
replace `name_code2' = subinstr(`name_code2',".","",.)
replace `name_code2' = subinstr(`name_code2'," ","",.)
replace `name_code2' = subinstr(`name_code2',",","",.)
replace `name_code2' = subinstr(`name_code2',";","",.)

keep `name_code1' `name_code2'

save "$savepath\Concordances\Konkordanz `Purpose'", replace

}
}






//clean the GP02 concordance
//open the GP02 to GP09 concordance to create reshaped ones for the switch_GP program
use "$savepath\Concordances\Konkordanz GP02 to GP09.dta", clear
drop if length(GP09) != 9 
save "$savepath\Concordances\Konkordanz GP02 to GP09.dta", replace


//Create wide concordances to use with the switch_GP program
//open each concordance to create a new one


//open the GP02 to GP09 concordance to create reshaped ones for the switch_GP program
use "$savepath\Concordances\Konkordanz GP02 to GP09.dta", clear

sort GP09 GP02
duplicates drop 
by GP09 : gen n = _n
reshape wide GP02, i( GP09 ) j( n)

foreach var of varlist GP02* {
local name = "GP02_ambig" + subinstr("`var'", "GP02", "", .)
rename `var' `name'
}

save "$savepath\Concordances\wide_GP09_to_GP02.dta", replace


use "$savepath\Concordances\Konkordanz GP02 to GP09.dta", clear

sort GP02 GP09 
duplicates drop 
by GP02 : gen n = _n
reshape wide GP09, i( GP02 ) j( n)

foreach var of varlist GP09* {
local name = "GP09_ambig" + subinstr("`var'", "GP09", "", .)
rename `var' `name'
}

save "$savepath\Concordances\wide_GP02_to_GP09.dta", replace




//open the GP95 to GP02 concordance to create a reshaped one
use "$savepath\Concordances\Konkordanz GP95 to GP02.dta", clear

sort GP95 GP02
duplicates drop 
by GP95 : gen n = _n
reshape wide GP02, i( GP95 ) j( n)

foreach var of varlist GP02* {
local name = "GP02_ambig" + subinstr("`var'", "GP02", "", .)
rename `var' `name'
}

save "$savepath\Concordances\wide_GP95_to_GP02.dta", replace

//create a concordance for concording unresolved GP95s to GP2009
use "$savepath\Concordances\Konkordanz GP95 to GP02.dta", clear
joinby GP02 using "$savepath\Concordances\Konkordanz GP02 to GP09.dta", unmatched (master)
keep GP95 GP09
duplicates drop 
sort GP95 GP09
by GP95 : gen n = _n
reshape wide GP09, i( GP95 ) j( n)

foreach var of varlist GP09* {
local name = "GP09_ambig" + subinstr("`var'", "GP09", "", .)
rename `var' `name'
}

save "$savepath\Concordances\wide_GP95_to_GP09.dta", replace



// create concordance for GP09V2 to GP09
use "$savepath\Concordances\Konkordanz GP09V2 to GP09.dta", clear
drop if GP09 == "" &  GP09V2 == ""
save "$savepath\Concordances\Konkordanz GP09V2 to GP09.dta" , replace


sort GP09V2 GP09 
duplicates drop 
by GP09V2 : gen n = _n
reshape wide GP09, i( GP09V2 ) j( n)

foreach var of varlist GP09* {
local name = "GP09_ambig" + subinstr("`var'", "GP09", "", .)
rename `var' `name'
}

save "$savepath\Concordances\wide_GP09V2_to_GP09.dta", replace


}
//

//----------------------------------------------------------------------\\
//-------------------------------By WZ----------------------------------\\
//----------------------------------------------------------------------\\

{
// Hier passen wir die Datei Merkmale_WZ-.dta so an, dass diese ð¢¥² wz und jahr eindeutig identifiziert / gemerged werden kann. 
// Dazu wird der CPI identifier (COICPO) in die WZ2008 / WZ2003 Spalte geschoben, analog zum bisherigen vorgehen fð² –ariablen, 
// welche eigentlich keinen WZ-Identifier besitzen. 

// CPI als seperate Dabei 
use CPI_2001_Aggregate jahr using "$savepath\Merkmale_WZ.dta", clear
drop if jahr ==.
drop if CPI_2001_Aggregate==.
duplicates drop 
save "$savepath\CPI.dta", replace 


use jahr DollarForEuro DM_for_Dollar using "$savepath\Merkmale_WZ.dta", clear
duplicates drop
drop if jahr ==.
drop if DollarForEuro ==. & DM_for_Dollar ==.
replace DollarForEuro = 1/( DM_for_Dollar/1.95583)  if DollarForEuro == .
drop DM_for_Dollar
save "$savepath\PriceindicesAndDepreciation\Wechselkurse.dta", replace

}
//
}
//close decompose wz data


}
//


//-------------------------------\\
//---------Do-File-Run-----------\\
//-------------------------------\\
{


//preparation-do-file
do "$syntaxpath\Preparation german microdata.do"

//estimation
do "$syntaxpath\Replication german microdata.do"







}
//


capture log close _all





