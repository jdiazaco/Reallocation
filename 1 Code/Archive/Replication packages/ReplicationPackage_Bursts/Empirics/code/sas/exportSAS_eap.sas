/*	====================================================
	Export SAS datasets to Stata datasets

	==================================================== 	*/ 

/* Folders */
%let SASdatasetsFolder 	= %scan(&sysparm., 1, |);
%let dataExportFolder 	= %scan(&sysparm., 2, |);

/* Export raw data to Stata year by year */

libname eap09 "&SASdatasetsFolder/EAP_EAP_2009";

proc export data=eap09.eap2009
outfile="&dataExportFolder/eap2009.dta"
replace;
run;


libname eap10 "&SASdatasetsFolder/EAP_EAP_2010";

proc export data=eap10.eap2010
outfile="&dataExportFolder/eap2010.dta"
replace;
run;

libname eap11 "&SASdatasetsFolder/EAP_EAP_2011";

proc export data=eap11.eap2011
outfile="&dataExportFolder/eap2011.dta"
replace;
run;

libname eap12 "&SASdatasetsFolder/EAP_EAP_2012";

proc export data=eap12.eap2012
outfile="&dataExportFolder/eap2012.dta"
replace;
run;

libname eap13 "&SASdatasetsFolder/EAP_EAP_2013";

proc export data=eap13.eap2013
outfile="&dataExportFolder/eap2013.dta"
replace;
run;

libname eap14 "&SASdatasetsFolder/EAP_EAP_2014";

proc export data=eap14.eap2014
outfile="&dataExportFolder/eap2014.dta"
replace;
run;

libname eap15 "&SASdatasetsFolder/EAP_EAP_2015";

proc export data=eap15.eap2015
outfile="&dataExportFolder/eap2015.dta"
replace;
run;

libname eap16 "&SASdatasetsFolder/EAP_EAP_2016";

proc export data=eap16.eap2016
outfile="&dataExportFolder/eap2016.dta"
replace;
run;

libname eap17 "&SASdatasetsFolder/EAP_EAP_2017";

proc export data=eap17.eap2017
outfile="&dataExportFolder/eap2017.dta"
replace;
run;

libname eap18 "&SASdatasetsFolder/EAP_EAP_2018";

proc export data=eap18.eap2018
outfile="&dataExportFolder/eap2018.dta"
replace;
run;

libname eap19 "&SASdatasetsFolder/EAP_EAP_2019";

proc export data=eap19.eap2019
outfile="&dataExportFolder/eap2019.dta"
replace;
run;


