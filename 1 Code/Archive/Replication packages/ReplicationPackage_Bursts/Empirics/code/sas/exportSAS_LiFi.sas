
/*	====================================================
	Export SAS datasets to Stata datasets
	==================================================== 	*/ 

/* Folders */
%let SASdatasetsFolder 	= %scan(&sysparm., 1, |);
%let dataExportFolder 	= %scan(&sysparm., 2, |);

/* 2010 - 2013 */

%macro year_loop2;
%do yearval=2010 %to 2013;

	%put Current Year=&yearval;
	
	%if &yearval < 2012 %then %do;
		libname year "&SASdatasetsFolder/LIFI_Enquête LIFI_&yearval";
	%end;
	%else %if &yearval >= 2012 %then %do;
		libname year "&SASdatasetsFolder/LIFI_LIFI_&yearval";
	%end;
	proc export data=year.lar
	outfile="&dataExportFolder/lar_&yearval..dta"
	replace;
	run;

%end;
%mend year_loop2;

%year_loop2;


/* 2014 - 2019 */

%macro year_loop2;
%do yearval=2014 %to 2019;

	%put Current Year=&yearval;
	
	libname year "&SASdatasetsFolder/LIFI_LIFI_&yearval";
	
	proc export data=year.liaison_reelle
	outfile="&dataExportFolder/liaison_reelle_&yearval..dta"
	replace;
	run;

%end;
%mend year_loop2;

%year_loop2;
