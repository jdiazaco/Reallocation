/*	====================================================
	Export SAS datasets to Stata datasets

	==================================================== 	*/ 

/* Folders */
%let SASdatasetsFolder 	= %scan(&sysparm., 1, |);
%let dataExportFolder 	= %scan(&sysparm., 2, |);

/* Export raw data to Stata */

libname gecir "&SASdatasetsFolder/BGCI_GECIR_GECIR";

/* 2008 */

proc export data=gecir.gecir_2008
outfile="&dataExportFolder/gecir2008.dta"
replace;
run;


/* 2009 */

proc export data=gecir.gecir_2009
outfile="&dataExportFolder/gecir2009.dta"
replace;
run;


/* 2010 */

proc export data=gecir.gecir_2010
outfile="&dataExportFolder/gecir2010.dta"
replace;
run;


/* 2011 */

proc export data=gecir.gecir_2011
outfile="&dataExportFolder/gecir2011.dta"
replace;
run;


/* 2012 */

proc export data=gecir.gecir_2012
outfile="&dataExportFolder/gecir2012.dta"
replace;
run;


/* 2013 */

proc export data=gecir.gecir_2013
outfile="&dataExportFolder/gecir2013.dta"
replace;
run;


/* 2014 */

proc export data=gecir.gecir_2014
outfile="&dataExportFolder/gecir2014.dta"
replace;
run;


/* Version 2020 */

proc export data=gecir.dec_2069a_092020
outfile="&dataExportFolder/gecirv2020.dta"
replace;
run;


/* Version 2023 */

proc export data=gecir.d_2069_liasses_082023_diff
outfile="&dataExportFolder/gecirv2023.dta"
replace;
run;


/* Version 2025 */

proc export data=gecir.dec_2069_052025_diff
outfile="&dataExportFolder/gecirv2025.dta"
replace;
run;
