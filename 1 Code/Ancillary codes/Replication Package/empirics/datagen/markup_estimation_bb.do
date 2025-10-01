*** estimate XTABOND coefficients 


**************** Preamble **************** *************************************

* dependencies 
ssc install xtabond2, replace 

* housekeeping
clear all 
macro drop _all
set more off, permanently 

* set working directory 

** preamble 
local altprice p37_XTABOND
use analysisdata/ficusfare_reduced_9419_sec2_year2_firm_va_sevyears_v0222, clear 

	

*** Industry list
local indlist 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 
************** Generate and rename variables ***********************************

*sale
gen s = ln(catotalR)
gen sale = catotalR

*materials + merchandise
gen m = ln(acha4R)

*labor
gen v = ln(salR)

*capital
gen k = ln(immocorR)

*service
gen o = ln(autachaR)

* definitions 
gen p = ln(price_firm_std_m2345_wm_wm)
local indlist 08 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95
gen q = log(catotalR) - p


*code the winsorizing by sector
local winlevel 02
foreach var in  s m v k o p q {
	gen `var'_w = .
	foreach ind in  `indlist' {
	
		winsor `var' if naf2d_num==`ind', p(.`winlevel') gen(`var'_temp)
		replace `var'_w  = `var'_temp if naf2d_num==`ind'
		drop `var'_temp
	
	}
	drop `var'
	rename `var'_w `var'
	label var `var' "`var' win `winlevel'"
}
************* Graph on inputs **************************************************

* summary stats
su s m v k o p q ms5d

preserve
	
		keep firmsId year  naf2d_num sale  v k m o s p q ms5d 
		order firmsId year  naf2d_num sale  v k m o s p q ms5d
		
		xtset firmsId year
		sort firmsId year	
		sort naf2d_num firmsId year
		
		save "analysisdata\data_smallest_for_python_p37_XTABOND.dta", replace
	
	
	
restore




*** open data
use  "analysisdata\data_smallest_for_python_p37_XTABOND.dta", replace	
		
*** generate variables
gen v2 = v^2 
gen k2 = k^2 
gen o2 = o^2 
gen m2 = m^2 
gen mv = m*v
gen mo = m*o 
gen mk = m*k 
gen vo = v*o 
gen vk = v*k 
gen ok = o*k


	

*** run XTABOND - quantity 
xtset firmsId year 
xtabond2 q l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l(0/1).v2 l(0/1).k2 l(0/1).m2 l(0/1).o2  l(0/1).mv   l(0/1).mo   l(0/1).mk  l(0/1).vo  l(0/1).vk l(0/1).ok l.q if naf2d==08 , gmm(q v k m o v2 k2 m2 o2 mv mo mk vo vk ok, lag(3 3)  )   robust
md_ar1, nx(14) beta(e(b)) cov(e(V)) 
matrix theta08= theta   
xtabond2 q l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l.q if naf2d==08 , gmm(q v k m o , lag(3 3)) robust
md_ar1, nx(4) beta(e(b)) cov(e(V)) 
matrix theta08_cd= theta 
matrix collection =  [theta08_cd', theta08']  
foreach ind in  13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95 {
	xtabond2 q l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l(0/1).v2 l(0/1).k2 l(0/1).m2 l(0/1).o2  l(0/1).mv   l(0/1).mo   l(0/1).mk  l(0/1).vo  l(0/1).vk l(0/1).ok l.q if naf2d==`ind' , gmm(q v k m o v2 k2 m2 o2 mv mo mk vo vk ok, lag(3 3 ) ) robust
	md_ar1, nx(14) beta(e(b)) cov(e(V)) 
	matrix theta`ind'= theta   
	xtabond2 q l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l.q if naf2d==`ind' , gmm(q v k m o , lag(3 3)) robust
	md_ar1, nx(4) beta(e(b)) cov(e(V)) 
	matrix theta`ind'_cd= theta 
	matrix collection = [collection \ [theta`ind'_cd', theta`ind''] ] 
	
}


*** save data 

* turn matrix into data 
clear
svmat collection 

* clean variables
local index = 1  
foreach var in  beta_v_cd beta_k_cd beta_m_cd beta_o_cd drop1 beta_v_tl beta_k_tl beta_m_tl beta_o_tl beta_v2_tl beta_k2_tl beta_m2_tl beta_o2_tl beta_mv_tl beta_mo_tl beta_mk_tl beta_vo_tl beta_vk_tl beta_ok_tl  {
	rename collection`index'  `var'
	local index = `index' + 1 
}
drop drop* 

* add industry 
gen naf2d = . 
local index = 1  
foreach ind in  8 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95  {
	replace naf2d = `ind' if _n == `index'  
	local index = `index' + 1 
}
 
* save 
save "analysisdata\coefficients_byind_Python_BBQ_p37.dta" , replace 




 
*** open data
use  "analysisdata\data_smallest_for_python_p37_XTABOND.dta", replace	
		
*** generate variables
gen v2 = v^2 
gen k2 = k^2 
gen o2 = o^2 
gen m2 = m^2 
gen mv = m*v
gen mo = m*o 
gen mk = m*k 
gen vo = v*o 
gen vk = v*k 
gen ok = o*k

*** run XTABOND - revenue 
xtset firmsId year 
xtabond2 s l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l(0/1).v2 l(0/1).k2 l(0/1).m2 l(0/1).o2  l(0/1).mv   l(0/1).mo   l(0/1).mk  l(0/1).vo  l(0/1).vk l(0/1).ok l.s if naf2d==08 , gmm(s v k m o v2 k2 m2 o2 mv mo mk vo vk ok, lag(3 3)) robust 
md_ar1, nx(13) beta(e(b)) cov(e(V)) 
matrix theta08= theta   
xtabond2 s l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l.s if naf2d==08 , gmm(s v k m o , lag(3 3)) robust
md_ar1, nx(4) beta(e(b)) cov(e(V)) 
matrix theta08_cd= theta 
matrix collection =  [theta08_cd', theta08']  
foreach ind in  13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95 {
	xtabond2 s l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l(0/1).v2 l(0/1).k2 l(0/1).m2 l(0/1).o2  l(0/1).mv   l(0/1).mo   l(0/1).mk  l(0/1).vo  l(0/1).vk l(0/1).ok l.s if naf2d==`ind' , gmm(s v k m o v2 k2 m2 o2 mv mo mk vo vk ok, lag(3 3)) robust
	md_ar1, nx(13) beta(e(b)) cov(e(V)) 
	matrix theta`ind'= theta   
	xtabond2 s l(0/1).v l(0/1).k l(0/1).m l(0/1).o  l.s if naf2d==`ind' , gmm(s v k m o , lag(3 3)) robust 
	md_ar1, nx(4) beta(e(b)) cov(e(V)) 
	matrix theta`ind'_cd= theta 
	matrix collection = [collection \ [theta`ind'_cd', theta`ind''] ] 
	
}
*** save data 

* turn matrix into data 
clear
svmat collection 

* clean variables
local index = 1  
foreach var in  beta_v_cd beta_k_cd beta_m_cd beta_o_cd drop1 beta_v_tl beta_k_tl beta_m_tl beta_o_tl beta_v2_tl beta_k2_tl beta_m2_tl beta_o2_tl beta_mv_tl beta_mo_tl beta_mk_tl beta_vo_tl beta_vk_tl beta_ok_tl  {
	rename collection`index'  `var'
	local index = `index' + 1 
}
drop drop* 

* add industry 
gen naf2d = . 
local index = 1  
foreach ind in  8 13 14 15 16 17 18 20 22 23 24 25 26 27 28 29 30 31 32 33 43 46 70 95  {
	replace naf2d = `ind' if _n == `index'  
	local index = `index' + 1 
}
 
* save 
save "analysisdata\coefficients_byind_Python_BBR_p37.dta" , replace 




