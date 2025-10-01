/* 	Data creation replication master
	Hitchhiker's Guide to Markup Estimation
	De Ridder, Grassi, Morzenti (2025)
*/ 


*** produce markups

* Optional: take EAP files from Burstein et al (2024) and save them as CSV (alternatively, copy them by hand to 'rawdata')
shell "C:\Program Files\SASHome\SASFoundation\9.4\sas.exe" "C:\Users\Public\Documents\DGM_ECMA\datagen\sasimport_april21.sas"

* prepare the EAP data
do datagen/loadEAP 

* take panels from Burstein et al (2024) and produce joint EAP-FARE data
do datagen/prepare_datasets 

* obtain xtabond coefficients + produce the `smallest for python' files  
do datagen/markup_estimation_bb

* obtain production function coefficients 
do datagen/markup_estimation_dgmacf 

* produce markups from elasticity estimates (previously called seperate calculation)
do datagen/markup_calculation 