/*******************************************************

Product data importer 
Maarten de Ridder
29-01-2019 

This file:
	- Imports CIS data for each year 
	- Exports as CSV 
********************************************************/

*2009;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2009"; 
proc export data=input.eap2009 
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2009.csv'
	dbms=csv
	replace;
run; 

*2010;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2010"; 
proc export data=input.eap2010 
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2010.csv'
	dbms=csv
	replace;
run; 

*2011;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2011"; 
proc export data=input.eap2011
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2011.csv'
	dbms=csv
	replace;
run; 

*2012;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2012"; 
proc export data=input.eap2012
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2012.csv'
	dbms=csv
	replace;
run; 

*2013;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2013"; 
proc export data=input.eap2013
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2013.csv'
	dbms=csv
	replace;
run; 

*2014;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2014"; 
proc export data=input.eap2014
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2014.csv'
	dbms=csv
	replace;
run; 

*2015;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2015"; 
proc export data=input.eap2015
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2015.csv'
	dbms=csv
	replace;
run;

*2016;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2016"; 
proc export data=input.eap2016
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2016.csv'
	dbms=csv
	replace;
run;  

*2017;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2017"; 
proc export data=input.eap2017
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2017.csv'
	dbms=csv
	replace;
run;  

*2018;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2018"; 
proc export data=input.eap2018
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2018.csv'
	dbms=csv
	replace;
run;  

*2019;
libname input "C:\Users\Public\Documents\DGM_ECMA\rawdata\EAP_EAP_2019"; 
proc export data=input.eap2019
	outfile='C:\Users\Public\Documents\DGM_ECMA\rawdata\eap2019.csv'
	dbms=csv
	replace;
run;  
