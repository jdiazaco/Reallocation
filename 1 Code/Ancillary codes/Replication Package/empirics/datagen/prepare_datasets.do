*** merge panel from Burstein et al. (2024) with EAP data 





** open raw data 
use rawdata/ficusfare_profil_wof_19942019_v0222, clear 

***** Sector classification
gen naf2d = substr(naf2003_single,1,2)
destring naf2d, gen(naf2d_num)
drop if naf2d=="."

* manufacturing indicator
gen manuf =0
replace manuf = 1 if naf2d_num>=10 & naf2d_num<=33
label var manuf "firms in 10<=2d<=33"

*Change the variable typimpo (harmonized across FICUS and FARE)
replace typimpo = 2 if typimpo == 11

* clean up 
capture drop gr_*  //redundant 
capture drop lag_* //redundant 
drop istrash degage
drop prodtot_ cote autprex prodimm autchex dotamor dotprov impotax ventmar prodven prodsto ///only in FICUS that is before 2009 

***** Label some variables
label variable cogs "sal+acha6 = sal+achamar+achampr+autacha+varstma+varstmp"
label variable cogs_newdef "sal+acha4 = sal+achamar+achampr+varstma+varstmp"

rename naf2003 naf
label variable naf "Naf.rev.2 classification"

rename naf2003_single naf_single
label variable naf_single "Unique Naf rev 2 classification (over time)"

***** Select years *******************************************************************
*drop if year<2009 // about 11.2 millions observations dropped

gen sample_years =0
replace sample_years = 1 if year>=2009 
label var sample_years "year>=2009"


************** Merge with price/quantity panel ***************************************
***** actual merge
merge 1:1 siren  year using analysisdata\EAP_firmlevel  
drop if _merge == 2

***** indicator of matching
gen matchEAP_FARE = 1 if _merge==3
replace matchEAP_FARE = 0 if _merge !=3
label var matchEAP_FARE "=1 if both in EAP and FARE"


***** housekeeping
*cleaning after yourself
drop _merge

*drop lag information in EAP (that are not so good)
drop *_lag *_lag_*



************** Deflate numerical variables ****************************************************
*** Deflate
merge m:1 code year using rawdata/gdp_deflator_euklems_nov21
drop if _merge==2
drop _merge

gen catotalR 	= catotal/sectordeflator_GO_P
gen salR 		= sal/deflator
foreach acha in 1 2 3 4 5 6 {
	gen acha`acha'R = acha`acha'/sectordeflator_II_P 
}
gen autachaR = autacha/sectordeflator_II_P 
gen immocorR = immocor/deflator

//housekeeping 
drop _*	*deflator* IP_GFCF_market 


************** Select the Sample of firms ******************************************************

***** generate a dummy for non-missing price
	gen sample_firm_price = 0
	replace sample_firm_price = 1 if price_firm_std_m2345_wm_wm!=.

***** Add firms with full info on inputs & output
	gen sample_firm_inputs = 0
	replace sample_firm_inputs = 1 if (catotalR>0 & salR>0 & immocorR>0 & acha4R>0 & autachaR>0)
	label var sample_firm_inputs "=1 if (catotalR>0 & salR>0 & immocorR>0 & acha4R>0 & autachaR>0)"

	*check the number of zero/negative of each variables
	foreach var in catotal sal immocor acha4 autacha{
		gen neg_`var'R = .
		replace neg_`var'R = -1 if  `var'R<0
		replace neg_`var'R = 0 if  `var'R==0
		replace neg_`var'R = 1 if  `var'R>0
		ta neg_`var'R, m
		
		gen l_`var'R = ln(`var'R)
	}
	

***** Add firms with full info on inputs & output with more than 1000euros
	gen sample_firm_inputs1 = 0
	replace sample_firm_inputs1 = 1 if (catotalR>=1 & salR>=1 & immocorR>=1 & acha4R>=1 & autachaR>=1)
	label var sample_firm_inputs1 "=1 if (catotalR>=1 & salR>=1 & immocorR>=1 & acha4R>=1 & autachaR>=1)"

	*check the number of less than 1 of each variables
	foreach var in catotal sal immocor acha4 autacha{
		gen less1_`var'R = .
		replace less1_`var'R = -1 if  `var'R<1
		replace less1_`var'R = 0 if  `var'R==1
		replace less1_`var'R = 1 if  `var'R>1
		ta less1_`var'R
		
	}
	

	


***** Firms with at least 2 employees

	gen sample_firm_emp2 = 0
	replace sample_firm_emp2=1 if emp>2
	label var sample_firm_emp2 "=1 if emp>2"

	
***** Firms with positive value-added
	gen sample_va = 0
	replace sample_va=1 if va>0
	label var sample_va "=1 if va>0"

	
***** Firms with cj 5
	gen sample_firm_cj5 = 0
	replace sample_firm_cj5 = 1 if (cj>4999 & cj<6000)
	replace sample_firm_cj5 = . if cj ==.
	label var sample_firm_cj5 "=1 if 4999<cj<6000"
	
	
***** Single Product Firms
	*rescale productcount to reflect the number of products rather than the number of duplicates
	gen product_number = productcount + 1
	*some table
	sum product_number, d
	drop productcount
	*the indicator that check the number of products
	gen sample_firm_uprod = 0
	replace sample_firm_uprod = 1 if product_number==1

	
****** Final sample indicator
	gen sample_firm = sample_firm_inputs*sample_firm_price*sample_firm_emp2 //=1 if firms satisfy the three criteria
	
	gen sample_firm_u = sample_firm_inputs*sample_firm_price*sample_firm_emp2*sample_firm_uprod //=1 if firms satisfy the three criteria
	
****** Other sample (more than 2 employee and non-zero inputs)
	gen  sample_firm_inputs_emp2 = sample_firm_inputs*sample_firm_emp2
	
	
**** indicators for firms appearing only once
	bysort siren: egen count=count(catotal)
	gen sample_firm_1year = 0
	replace sample_firm_1year = 1 if count==1
	drop count


*housekeeping
	capture drop neg_* less1_* bunching_*

	
**************  Choose the relevent sectors *****************************************************

***** Generate the sample of sectors
*start from manufacturing
gen sample_sectors = manuf
*"Manufacturing of tobacco product"
replace sample_sectors = 0 if naf2d=="12" // only 50 years*observations
*"Manufacturing of beverage"
replace sample_sectors = 0 if naf2d=="11" // only 21 years*observations with info on price
*"Manufacturing of food products"
replace sample_sectors = 0 if naf2d=="10" // only 134 years*observations with info on price
*"Manufacturing of coke and refined petroleum products"
replace sample_sectors = 0 if naf2d=="19" // only 153 years*observations with info on price
*"Manufacture of Basic Pharmaceutical products and pharmaceutical preparations"
replace sample_sectors = 0 if naf2d=="21" // only 664 years*observations with info on price


***** Generate an alternative sample of sectors
*start from the previous sample
gen sample_sectors2 = sample_sectors
*"Other Mining and quarrying"
replace sample_sectors2 = 1 if naf2d=="08" 
*"Specialised construction"
replace sample_sectors2 = 1 if naf2d=="43" 
*"Wholesale (except mothor vehicle)"
replace sample_sectors2 = 1 if naf2d=="46" 
*"Head office and Consultancy"
replace sample_sectors2 = 1 if naf2d=="70" 
*"Repair of computer and personal and households goods"
replace sample_sectors2 = 1 if naf2d=="95" 


***** Generate the largest sample of sectors
*start from the previous sample
gen sample_sectors3 = 1
*
replace sample_sectors3 = 0 if naf2d=="05"
*
replace sample_sectors3 = 0 if naf2d=="06"
*
replace sample_sectors3 = 0 if naf2d=="07"
*
replace sample_sectors3 = 0 if naf2d=="09"
*
replace sample_sectors3 = 0 if naf2d=="12"
*
replace sample_sectors3 = 0 if naf2d=="02"      



****** Sample of sectors that do not have enough firms in each years
	**indicator of bad sectors (manually)
	gen sample_sector_5d = 1
	foreach naf_sec in 13.96Z 16.22Z 24.46Z 27.31Z 27.33Z 28.24Z 28.99A 30.12Z 31.09A 33.11Z 33.13Z 33.17Z 33.19Z 33.20B 33.20D 43.11Z 43.21B 46.12A 46.47Z 46.48Z 46.65Z 46.73B{
		replace sample_sector_5d = 0 if naf_single=="`naf_sec'" 
	}
	




***** compare sectors index variables
*naf classification
gen naf_simple= subinstr(naf, ".", "",1) // get rid of the . in the naf2003 variable

gen test_naf = 1 if (naf_simple == c_naf_rev2) & (matchEAP_FARE==1) 
replace test_naf = 0 if naf_simple != c_naf_rev2 & (matchEAP_FARE==1)

*ape classification
gen test_ape = 1 if (ape == c_ape_lanc) & (matchEAP_FARE==1) 
replace test_ape = 0 if (ape != c_ape_lanc) & (matchEAP_FARE==1)



*2d classification
*naf
gen naf2d_simple =  substr(naf_simple,1,2) //coming from FARE (NAF)
gen c_naf_rev2_2d =  substr(c_naf_rev2,1,2) //coming from EAP (NAF)

gen test_naf2d = 1 if (naf2d_simple == c_naf_rev2_2d) & (matchEAP_FARE==1) 
replace test_naf2d = 0 if (naf2d_simple != c_naf_rev2_2d) & (matchEAP_FARE==1)


*2d classification
*ape
gen ape2d_simple =  substr(ape,1,2) //coming from FARE
gen c_ape_lanc_2d =  substr(c_ape_lanc,1,2) //coming from EAP

gen test_ape2d = 1 if (ape2d_simple == c_ape_lanc_2d) & (matchEAP_FARE==1) 
replace test_ape2d = 0 if (ape2d_simple != c_ape_lanc_2d) & (matchEAP_FARE==1)

*cleaning after yourself
drop test_naf test_ape test_naf2d c_naf_rev2_2d naf2d_simple naf_simple test_ape2d ape2d_simple c_ape_lanc_2d



**************  Age of a firm (based on datcr date of creation) *********************************
*year of creation (does not work for years prior to 2009, need more work to uniformized the variables datcr)
gen yearcr = substr(datcr,1,4) if year>=2009

*fix some errors in the data
replace yearcr = "." if yearcr=="1111"

*compute age
destring yearcr, replace force
gen age = year-yearcr+1 //number of years since creations

*housekeeping
drop yearcr





**************  Market-Share *************************************************************************

*Market size
*market size on all the firms
bysort year naf_single: egen catotal_tot = total(catotal)
*market size on firms with emp>2
bysort year naf_single: egen catotal_tot_emp = total(catotal) if sample_firm_emp2==1
*market size on firms with emp>2 & greater than 1 input/sale
bysort year naf_single: egen catotal_tot_emp_input = total(catotal) if sample_firm_emp2==1 & sample_firm_inputs ==1
*market size on firms with emp>2 & greater than 1 input/sale & and price
bysort year naf_single: egen catotal_tot_emp_input_price = total(catotal) if sample_firm_emp2==1 & sample_firm_inputs ==1 & sample_firm_price  ==1


*Market share
*market share on all the firms
 gen ms5d = catotal/catotal_tot
*market share on firms with emp>2
 gen ms5d_emp = catotal/catotal_tot_emp if sample_firm_emp2==1
*market share on firms with emp>2 & greater than 1 input/sale
 gen ms5d_emp_input = catotal/catotal_tot_emp_input if sample_firm_emp2==1 & sample_firm_inputs ==1
*market share on firms with emp>2 & greater than 1 input/sale & and non-zero price
 gen ms5d_emp_input_price = catotal/catotal_tot_emp_input_price if sample_firm_emp2==1 & sample_firm_inputs ==1 & sample_firm_price  ==1

*housekeeping
drop catotal_tot_*


**************  Graphs and Stats on Final Sample ***********************************************************************

gen sample_estim = sample_sectors2*sample_years*sample_firm 
gen sample_markup = sample_sectors2* sample_firm_inputs



******some graphs on various varialbles






**************  Save the data ***********************************************************************

**** Housekeeping
*useless
drop  prodtot  effsalm subvexp redi_r301 redi_r201 resexploit_08 c_ape_lanc c_naf_rev2 //keep orbil
drop ape datcr nace  appgr caexpor cafranc saltrai charsoc vaht vabcf
drop value_firm_m2345 quant_firm_m2345 price_firm_m2345 value_firm_m234 quant_firm_m234 price_firm_m234 value_firm_m345 quant_firm_m345 price_firm_m345 value_firm_m34 quant_firm_m34 price_firm_m34




*** Save the full dataset


*dropping firms that appears once
keep if sample_sectors2==1 & sample_years ==1 & sample_firm ==1 & sample_va ==1

bysort siren: egen count=count(catotal)
drop if count==1
drop count

* match variables in original dataset
keep com typimpo siren cj year reg dep orbil inpdt inpcs rrdinx rrdexx rd innovexpense catotal immocor invcorp achamar achampr varstma varstmp autacha mfrg software firmsId emp sal va acha1 acha2 acha3 acha4 acha5 acha6 cogs naf naf_single code cogs_newdef naf2d naf2d_num manuf sample_years price_firm_m2345 price_firm_std_m2345_m_m price_firm_std_m2345_m_wm price_firm_std_m2345_wm_m price_firm_std_m2345_wm_wm price_firm_std8_m2345_m_m price_firm_std8_m2345_m_wm price_firm_std8_m2345_wm_m price_firm_std8_m2345_wm_wm l_price_firm__m2345_wm price_firm_m2345_wm price_firm_std_m234_m_m price_firm_std_m234_m_wm price_firm_std_m234_wm_m price_firm_std_m234_wm_wm price_firm_std8_m234_m_m price_firm_std8_m234_m_wm price_firm_std8_m234_wm_m price_firm_std8_m234_wm_wm l_price_firm__m234_wm price_firm_m234_wm price_firm_std_m345_m_m price_firm_std_m345_m_wm price_firm_std_m345_wm_m price_firm_std_m345_wm_wm price_firm_std8_m345_m_m price_firm_std8_m345_m_wm price_firm_std8_m345_wm_m price_firm_std8_m345_wm_wm l_price_firm__m345_wm price_firm_m345_wm price_firm_std_m34_m_m price_firm_std_m34_m_wm price_firm_std_m34_wm_m price_firm_std_m34_wm_wm price_firm_std8_m34_m_m price_firm_std8_m34_m_wm price_firm_std8_m34_wm_m price_firm_std8_m34_wm_wm l_price_firm__m34_wm price_firm_m34_wm matchEAP_FARE catotalR salR acha1R acha2R acha3R acha4R acha5R acha6R autachaR immocorR sample_firm_price sample_firm_inputs neg_catotalR l_catotalR neg_salR l_salR neg_immocorR l_immocorR neg_acha4R l_acha4R neg_autachaR l_autachaR sample_firm_inputs1 less1_catotalR less1_salR less1_immocorR less1_acha4R less1_autachaR sample_firm_emp2 sample_va sample_firm_cj5 product_number sample_firm_uprod sample_firm sample_firm_u sample_firm_inputs_emp2 sample_firm_1year sample_sectors sample_sectors2 sample_sectors3 sample_sector_5d age catotal_tot ms5d ms5d_emp ms5d_emp_input ms5d_emp_input_price sample_estim sample_markup
 

save  analysisdata\ficusfare_reduced_9419_sec2_year2_firm_va_sevyears_v0222, replace 





clear
