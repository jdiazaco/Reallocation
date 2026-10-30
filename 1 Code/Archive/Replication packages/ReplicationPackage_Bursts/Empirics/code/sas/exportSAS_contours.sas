/*	====================================================
	Export SAS datasets to Stata datasets

	==================================================== 	*/ 

/* Folders */
%let SASdatasetsFolder 	= %scan(&sysparm., 1, |);
%let dataExportFolder 	= %scan(&sysparm., 2, |);

/* Export raw data to Stata year by year */

libname data09 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2009";

proc export data=data09.contour09
outfile="&dataExportFolder/contour_2009.dta"
replace;
run;


libname data10 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2010";

proc export data=data10.contour10
outfile="&dataExportFolder/contour_2010.dta"
replace;
run;

libname data11 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2011";

proc export data=data11.contour11
outfile="&dataExportFolder/contour_2011.dta"
replace;
run;

libname data12 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2012";

proc export data=data12.contour12
outfile="&dataExportFolder/contour_2012.dta"
replace;
run;

libname data13 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2013";

proc export data=data13.contour13
outfile="&dataExportFolder/contour_2013.dta"
replace;
run;

libname data14 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2014";

proc export data=data14.contour14
outfile="&dataExportFolder/contour_2014.dta"
replace;
run;

libname data15 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2015";

proc export data=data15.contour15
outfile="&dataExportFolder/contour_2015.dta"
replace;
run;

libname data16 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2016";

proc export data=data16.contour16
outfile="&dataExportFolder/contour_2016.dta"
replace;
run;

libname data17 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2017";

proc export data=data17.contour17
outfile="&dataExportFolder/contour_2017.dta"
replace;
run;

libname data18 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2018";

proc export data=data18.contour18
outfile="&dataExportFolder/contour_2018.dta"
replace;
run;

libname data19 "&SASdatasetsFolder/Statistique annuelle d'entreprise_Contours des entreprises profilées_2019";

proc export data=data19.contour19
outfile="&dataExportFolder/contour_2019.dta"
replace;
run;


