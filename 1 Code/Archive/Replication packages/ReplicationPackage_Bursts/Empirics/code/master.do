

* ============================================================================= *

// set up environment

clear all
capture log close
set matsize 11000
set more off
matrix drop _all

// packages
ssc install carryforward	, replace
ssc install shapley2		, replace
ssc install gtools			, replace
ssc install boottest		, replace
ssc install ftools			, replace
ssc install ppmlhdfe 		, replace
ssc install winsor			, replace
ssc install reghdfe			, replace
ssc install outreg2			, replace
ssc install estout			, replace
ssc install binscatter		, replace
ssc install fs				, replace
ssc install cleanplots		, replace

// seed
set seed 28112024

* ============================================================================= *


// set working directories 

global path "-- FOR DATA EDITOR --" 
global sas 	"-- FOR DATA EDITOR --" // Local sas exe file
global raw 	"-- FOR DATA EDITOR --"

// set graph style
set scheme cleanplots, perm
graph set window fontface "Arial Narrow"
set graphics off // on // 

// set ado path
adopath ++ "$path/code/analysis"


* ============================================================================= *


// set parameters

* period of analysis
global yr_start 2010
global yr_end 2019

* Confidentiality. Set to one to implement confidentiality checks on graphs
global conf_check 1 // 0 1 // 

// set working directories
mkdir "$path/output/"
mkdir "$path/data/raw/"
mkdir "$path/data/raw/contours"
mkdir "$path/data/raw/dads"
mkdir "$path/data/raw/eap"
mkdir "$path/data/raw/fare"
mkdir "$path/data/raw/gecir"
mkdir "$path/data/raw/lifi"
mkdir "$path/data/processed/"


* ============================================================================= *

// run SAS files

shell "$sas" "$path/code/sas/exportSAS_contours.sas" 	-sysparm "${raw}|${path}/data/raw/contours" -log NUL -nosplash -nologo
shell "$sas" "$path/code/sas/exportSAS_DADSpostes.sas" 	-sysparm "${raw}|${path}/data/raw/dads" 	-log NUL -nosplash -nologo
shell "$sas" "$path/code/sas/exportSAS_eap.sas" 		-sysparm "${raw}|${path}/data/raw/eap" 		-log NUL -nosplash -nologo
shell "$sas" "$path/code/sas/exportSAS_fare.sas" 		-sysparm "${raw}|${path}/data/raw/fare" 	-log NUL -nosplash -nologo
shell "$sas" "$path/code/sas/exportSAS_gecir.sas" 		-sysparm "${raw}|${path}/data/raw/gecir" 	-log NUL -nosplash -nologo
shell "$sas" "$path/code/sas/exportSAS_LiFi.sas" 		-sysparm "${raw}|${path}/data/raw/lifi" 	-log NUL -nosplash -nologo


* ============================================================================= *

// prep data for analysis

do "$path/code/data/contours/prepContour_master.do"
do "$path/code/data/corresp_pc8_bec/prepCorres_pc8_bec_master.do"
do "$path/code/data/concord_pc10/prepConcord_pc10_master.do"
do "$path/code/data/fare/prepFARE_master.do" 
do "$path/code/data/lifi/prepLiFi_master.do"
do "$path/code/data/gecir/prepGECIR_rd.do"
do "$path/code/data/dads/prepDADS.do"

do "$path/code/data/eap/1_EAPpanel_preprawdata_firmprod.do"
do "$path/code/data/eap/2_EAPpanel_prepsample_firmprod.do"
do "$path/code/data/eap/3_1_EAPpanel_prepworkdata_firm.do"
do "$path/code/data/eap/3_1_1_EAPpanel_prepworkdata_mode3_firm.do"
do "$path/code/data/eap/3_1_2_EAPpanel_prepworkdata_noreturnprod_firm.do"
do "$path/code/data/eap/3_2_EAPpanel_prepworkdata_firmprod.do"



* ============================================================================= *


// run scripts tabs/figs main text

do "$path/code/analysis/Table_1.do"
do "$path/code/analysis/Figure_1a.do"
do "$path/code/analysis/Figure_1b.do"
do "$path/code/analysis/Table_2.do"
do "$path/code/analysis/Figure_2.do"
do "$path/code/analysis/Figure_3.do"
do "$path/code/analysis/Figure_4.do"
do "$path/code/analysis/Figure_5.do"
do "$path/code/analysis/Table_3.do"
do "$path/code/analysis/Figure_6.do"
do "$path/code/analysis/Figure_7.do"
do "$path/code/analysis/Figure_8.do"
do "$path/code/analysis/Table_7.do"
do "$path/code/analysis/Table_8.do"
do "$path/code/analysis/Table_intextnumbers.do"


// run scripts tabs/figs Appendix

do "$path/code/analysis/Table_c1.do"
do "$path/code/analysis/Table_c2.do"
do "$path/code/analysis/Table_c3.do"
do "$path/code/analysis/Table_c4.do"
do "$path/code/analysis/Figure_c1.do"
do "$path/code/analysis/Figure_c2.do"
do "$path/code/analysis/Figure_c3.do"
do "$path/code/analysis/Figure_c4.do"
do "$path/code/analysis/Figure_c5.do"
do "$path/code/analysis/Figure_c6.do"
do "$path/code/analysis/Figure_c7.do"
do "$path/code/analysis/Figure_c8.do"
do "$path/code/analysis/Figure_c9.do"
do "$path/code/analysis/Figure_c10.do"
do "$path/code/analysis/Figure_c11.do"
do "$path/code/analysis/Figure_c12.do"
do "$path/code/analysis/Table_c5.do"
do "$path/code/analysis/Table_c6.do"
do "$path/code/analysis/Table_d1.do"
do "$path/code/analysis/Figure_d1.do"
do "$path/code/analysis/Figure_d2.do"

do "$path/code/analysis/rename_output_files.do"

/* END */