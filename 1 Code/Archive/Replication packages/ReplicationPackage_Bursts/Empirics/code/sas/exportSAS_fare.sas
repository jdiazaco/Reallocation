/*	====================================================
	Export SAS datasets to Stata datasets

	==================================================== 	*/ 

/* Folders */
%let SASdatasetsFolder 	= %scan(&sysparm., 1, |);
%let dataExportFolder 	= %scan(&sysparm., 2, |);

/* List of vars to keep */

%let FARE2008Vars = 	siren_08 IFRP_08 NOM_08 reg_08 DEP_08 NACE_08 EFF_ETP_08 EFF_3112_08
						appgr_08 TYPIMPO_08 DATCR_08 DATCS_08 APE_08 mdst_08 CATOTAL_08
						AUTPREX_08 PRODIMM_08 PRODSTO_08  VARSTMA_08 VARSTMP_08
						ACHAMAR_08 ACHAMPR_08 AUTACHA_08 AUTCHEX_08
						SALTRAI_08 CHARSOC_08
						IMMOCOR_08 IMMOINC_08
						VABCF_08 VAHT_08  EFFSALM_08;

%let FARE200919Vars = 	siren ape_diff ent_cre_daaaammjj immo_corp immo_inc stat_etat
						redi_r310 redi_r312 redi_r311 redi_r211 redi_r211 redi_r213 redi_r210
						redi_r212 redi_r214 redi_r222 redi_r216 redi_r217 redi_r003
						r004 redi_e001 redi_e200 liasse_source;

%let FARE2017Vars = 	siren ape_diff ent_cre_daaaammjj immo_corp immo_inc stat_etat
						redi_r310 redi_r312 redi_r311 redi_r211 redi_r211 redi_r213 redi_r210
						redi_r212 redi_r214 redi_r222 redi_r216 redi_r217 redi_r003
						r004 /*redi_e001*/ redi_e200 liasse_source;


/* Export raw data to Stata year by year */

libname fare08 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2008";
proc export data=fare08.tab(keep= &FARE2008Vars)
outfile="&dataExportFolder/fare2008.dta"
replace;
run;


libname fare09 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2009";

proc export data=fare09.tab(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2009.dta"
replace;
run;


libname fare10 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2010";

proc export data=fare10.fare2010(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2010.dta"
replace;
run;


libname fare11 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2011";

proc export data=fare11.fare2011(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2011.dta"
replace;
run;


libname fare12 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2012";

proc export data=fare12.fare2012_meth2012(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2012.dta"
replace;
run;


libname fare13 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2013";

proc export data=fare13.fare2013meth2013(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2013.dta"
replace;
run;


libname fare14 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2014";

proc export data=fare14.fare2014meth2014(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2014.dta"
replace;
run;


libname fare15 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2015";

proc export data=fare15.fare2015meth2015(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2015.dta"
replace;
run;


libname fare16 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2016";

proc export data=fare16.fare2016meth2016(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2016.dta"
replace;
run;


libname fare17 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2017";

proc export data=fare17.fare2017meth2017(keep= &FARE2017Vars)
outfile="&dataExportFolder/fare2017.dta"
replace;
run;


libname fare18 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2018";

proc export data=fare18.fare2018meth2018(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2018.dta"
replace;
run;


libname fare19 "&SASdatasetsFolder/Statistique annuelle d'entreprise_FARE_2019";

proc export data=fare19.fare2019meth2019(keep= &FARE200919Vars)
outfile="&dataExportFolder/fare2019.dta"
replace;
run;


