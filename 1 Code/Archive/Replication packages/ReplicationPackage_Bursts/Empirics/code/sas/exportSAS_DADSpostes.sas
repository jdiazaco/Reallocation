
/*	====================================================
	Export SAS datasets to Stata datasets
	==================================================== 	*/ 

/* Open code here and I can define macros*/

/* Folders */
%let SASdatasetsFolder 	= %scan(&sysparm., 1, |);
%let dataExportFolder 	= %scan(&sysparm., 2, |);

%let DADSVars = SIREN NIC TYP_EMPLOI S_BRUT PCS FILT REGT IDENT_S DATDEB DATFIN NBHEUR CPFD; /* APET */


/*Department values */
%let depvalues2010 = 11aa 11bb 11cc 11dd 21 22 23 24 25 26 31 41 42 43 52 53 54 72 73 74 82a 82b 83 91 93 94 97 99;
%let depvalues2011 = 11 21 22 23 24 25 26 31 41 42 43 52 53 54 72 73 74 82 83 91 93 94 97 99;
%let depvalues2012 = 21 22 23 24 25 26 31 41 42 43 52 53 54 72 73 74 82 83 91 93 94 97 99;
%let depvaluesidf = 75 77 78 91 92 93 94 95;
%let depvaluesreg = 24 27 28 32 44 52 53 75 76 84 93 94 97 99;




/*Data null step to create folders*/

data _null_;
	do yr = 2010 to 2019; /*%by 1*/
		folder_name = put(yr, 4.); /*converts the numeric year to a 4-character string*/
		rc = dcreate(folder_name, "&dataExportFolder");
	end;

run;


/*Macro for 2010-2011*/
%macro export_data_2010_2011;

%do yr = 2010 %to 2011; 
	libname year "&SASdatasetsFolder/DADS_DADS Postes_&yr";
	%do i = 1 %to %sysfunc(countw(&&depvalues&yr));
		%let value = %scan(&&depvalues&yr, &i);
		%let dataset_name = post&value.(keep= &DADSVars);
		
		/* The code does not remove all duplicate records, so I do it directly in Stata
		proc sort data=year.&dataset_name
			out=work.dadsnodupl 
			noduprecs; 
			by _all_;
		run;
		*/

		proc export data=year.&dataset_name
			outfile="&dataExportFolder/&yr/post&value..dta"
			replace;
		run;

	%end;
%end;

%mend export_data_2010_2011;

%export_data_2010_2011;


/*Macro for 2012-2013*/
%macro export_data_2012_2013;

%do yr = 2012 %to 2013; 
	libname year "&SASdatasetsFolder/DADS_DADS Postes_&yr";
	%do i = 1 %to %sysfunc(countw(&depvalues2012));
		%let value = %scan(&depvalues2012, &i);
		%let dataset_name = post&value.(keep= &DADSVars);

		proc export data=year.&dataset_name
			outfile="&dataExportFolder/&yr/post&value..dta"
			replace;
		run;

	%end;

	libname year "&SASdatasetsFolder/DADS_DADS Postes_&yr/Ile de France";
	%do i = 1 %to %sysfunc(countw(&depvaluesidf));
		%let value = %scan(&depvaluesidf, &i);
		%let dataset_name = post&value.(keep= &DADSVars);

		proc export data=year.&dataset_name
			outfile="&dataExportFolder/&yr/post&value.idf.dta"
			replace;
		run;

	%end;

%end;

%mend export_data_2012_2013;

%export_data_2012_2013;


/*Macro for 2014-2017*/
%macro export_data_2014_2017;

%do yr = 2014 %to 2017; 
	libname year "&SASdatasetsFolder/DADS_DADS Postes_&yr/Régions";
	%do i = 1 %to %sysfunc(countw(&depvaluesreg));
		%let value = %scan(&depvaluesreg, &i);
		%let dataset_name = post&value.(keep= &DADSVars);

		proc export data=year.&dataset_name
			outfile="&dataExportFolder/&yr/post&value..dta"
			replace;
		run;

	%end;

	libname year "&SASdatasetsFolder/DADS_DADS Postes_&yr/Départements";
	%do i = 1 %to %sysfunc(countw(&depvaluesidf));
		%let value = %scan(&depvaluesidf, &i);
		%let dataset_name = post&value.(keep= &DADSVars);

		proc export data=year.&dataset_name
			outfile="&dataExportFolder/&yr/post&value.idf.dta"
			replace;
		run;

	%end;

%end;

%mend export_data_2014_2017;

%export_data_2014_2017;


/*Macro for 2018-2019*/
%macro export_data_2018_2019;

%do yr = 2018 %to 2019; 
	libname year "&SASdatasetsFolder/DADS_DADS Postes_&yr";
	%do i = 1 %to 4;
		%let value = &i;
		%let dataset_name = post_&value.(keep= &DADSVars);

		proc export data=year.&dataset_name
			outfile="&dataExportFolder/&yr/post&value..dta"
			replace;
		run;

	%end;
%end;

%mend export_data_2018_2019;

%export_data_2018_2019;

/*End*/

