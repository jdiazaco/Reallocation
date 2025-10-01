/* 	Run this file to reproduce 
	Hitchhiker's Guide to Markup Estimation
	De Ridder, Grassi, Morzenti (2025)
*/ 


** preamble
clear all 
set more off 
global directory 
cd "C:\Users\Public\Documents\DGM_ECMA" 

** menu
global code 37_XTABOND_andSTATA_DGM    
set max_memory 50g 
set matsize 11000

** produce contents of analysisdata
do datagen/master_datagen 

** product contents of output
do code/master_code 