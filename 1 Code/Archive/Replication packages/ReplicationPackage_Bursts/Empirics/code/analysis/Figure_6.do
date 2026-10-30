
* open dataset 
use "$path/data/processed/EAPprodfra10_20102019_OnlyEAP1_firmprod_clean.dta", clear 

* identify a firm's first year in the data 
bysort siren year: egen yearrev = total(revenue)
gen firmyear = year if yearrev != .  & yearrev  > 0 
bysort siren: egen minyear = min(firmyear)
bysort siren: egen maxyear = max(firmyear)

* attach correct labels to products 
xtset id year 
//products that firm will continue to produce 
gen 	jplus  	= ( revenue != . & f1.revenue != . & year != maxyear) 
replace jplus 	= . if year == maxyear | revenue == .
//products that firm continued to produce from last year 
gen 	jminus  = ( revenue != . & l1.revenue != . & year != minyear)  
replace jminus 	= . if year == minyear | revenue == .
//products that the firm is going to lose
gen 	lcurl   = ( revenue != . & f1.revenue == . & year != maxyear)
replace lcurl 	= . if year == maxyear | revenue == . 
//products that are new to the firm 
gen 	ncurl 	= ( revenue != . & l1.revenue == . & year != minyear)  
replace ncurl 	= . if year == minyear | revenue == .

* calculate revenue at time t per group
foreach var in jplus jminus lcurl ncurl { 
	gen temp  							= revenue*`var' 
	bysort year siren:	egen rev_`var' 	= total(temp) 
	drop temp 
}

* generate EAP revenue
bysort year siren: egen revenue_EAP = total(revenue)

* keep one obs per firm-year
duplicates drop siren year , force 
capture drop id 
egen id = group(siren)
xtset id year
gen lrevenue_EAP = l1.revenue_EAP

* make panel balanced
save $path/data/processed/temp_mdr , replace 
duplicates drop id , force 
keep id 
expand 10
bysort id: gen year = _n +2009 
merge 1:1 id year using $path/data/processed/temp_mdr
erase $path/data/processed/temp_mdr.dta 
bysort id: egen exityyear_used = mean(exityearecon)
bysort id: egen birthyear_used = mean(birthyearecon)
xtset id year 



** place firm-years in bins 

* entry and exit 
gen exited  = 1 if   exityyear_used  - year < 0 &  exityyear_used  - year  >= -1 
gen entered = 1 if   birthyear_used  - year <=0 &  birthyear_used  - year  >  -1 

* generate additional variables - haltiwanger decomposition 
gen halt_revgrowth  	= (revenue_EAP-l1.revenue_EAP) 	/(0.5*(revenue_EAP+l1.revenue_EAP))
replace halt_revgrowth = -2 if exited 	== 1
replace halt_revgrowth =  2 if entered 	== 1


* count bin contributions
gen bin = .
forval bins = -2.0(0.05)2.0 {
			qui: replace bin = `bins' if halt_revgrowth >= `bins' & halt_revgrowth < `bins' + .05
} 


** calculate revenue per bin 

* add up continuing revenue change per bin
gen delta_cont = (rev_jminus-l1.rev_jplus)

* add up change in revenue from lost products
xtset id year 
gen delta_loss = -(l1.rev_lcurl) 
gen delta_gain = (rev_ncurl) 

* add up total revenue growth 
xtset id year 
gen delta = (revenue_EAP-l1.revenue_EAP)
replace delta = -l1.revenue_EAP if exited ==1 
replace delta =                revenue_EAP if entered ==1 

* exit tratment 
replace delta_loss = -l1.revenue_EAP if exited == 1
replace delta_gain = 0 if exited == 1 
replace delta_cont = 0 if exited == 1 
replace delta      =  -l1.revenue_EAP if exited == 1

* entry treatment 
replace delta_loss = 0 if entered == 1
replace delta_gain = revenue_EAP if entered == 1 
replace delta_cont = 0 if entered == 1 
replace delta      = revenue_EAP if entered == 1

* bins 
drop if bin == . 
bysort bin year: egen delta_bin      = total(delta) 
bysort bin year: egen delta_gain_bin = total(delta_gain) 
bysort bin year: egen delta_loss_bin = total(delta_loss) 
bysort bin year: egen delta_cont_bin = total(delta_cont) 




** decompose overall creation and destruction 

* calculate overall revenue creation and destruction 
gen flag_pos =1 if bin >0
gen flag_neg =1 if bin <0
gen delta_pos = delta*flag_pos
gen delta_neg = delta*flag_neg
bysort year: egen creation = sum(delta_pos)
bysort year: egen destruct = sum(delta_neg)

* calculate bin contributions 
duplicates drop bin year, force 
keep delta*bin bin* creation destruct year
gen binshare_pos =   delta_bin / creation if bin > 0 
gen binshare_neg =   delta_bin / destruct if bin < 0 

* contributors - split 
gen binshare_cont_pos =   delta_cont_bin / creation if bin > 0 
gen binshare_cont_neg =   delta_cont_bin / destruct if bin < 0 
gen binshare_gain_pos =   delta_gain_bin / creation if bin > 0 
gen binshare_gain_neg =   delta_gain_bin / destruct if bin < 0 
gen binshare_loss_pos =   delta_loss_bin / creation if bin > 0 
gen binshare_loss_neg =   delta_loss_bin / destruct if bin < 0

* alternative: decomposition 
gen binshare_cont_pos_decomp =   binshare_cont_pos / binshare_pos if bin > 0 
gen binshare_cont_neg_decomp =   binshare_cont_neg / binshare_neg if bin < 0 
gen binshare_gain_pos_decomp =   binshare_gain_pos / binshare_pos if bin > 0 
gen binshare_gain_neg_decomp =   binshare_gain_neg / binshare_neg if bin < 0 
gen binshare_loss_pos_decomp =   binshare_loss_pos / binshare_pos if bin > 0 
gen binshare_loss_neg_decomp =   binshare_loss_neg / binshare_neg if bin < 0


** take averages over year
foreach var in binshare_pos binshare_neg binshare_cont_pos binshare_cont_neg  binshare_gain_pos binshare_gain_neg binshare_loss_pos binshare_loss_neg {
	bysort bin: egen `var'_m = mean(`var') 
	replace `var'_m = `var'_m * 100 
	if "`var'" != "binshare_pos" & "`var'" != "binshare_neg" { 
		bysort bin: egen `var'_decomp_m = mean(`var'_decomp) 
		replace `var'_decomp_m = `var'_decomp_m * 100 
	}
}
keep bin *_m 
duplicates drop bin, force 
sort bin 
gen bin_neg = -bin 


** prepare charts for twoway graph 
gen binshare_gain_pos_m_stack = binshare_gain_pos_m + binshare_cont_pos_m


** make plots

** plots with net effect of product innovation and destruction 
gen binshare_gainloss_pos_decomp_m = binshare_gain_pos_decomp_m + binshare_loss_pos_decomp_m 
gen binshare_gainloss_neg_decomp_m = binshare_gain_neg_decomp_m + binshare_loss_neg_decomp_m 
insobs 1 
replace bin = 0 if _n == _N 

graph bar (sum) binshare_pos_m binshare_neg_m, over(bin,   gap(*.25)  relabel(1 "-2" 2 " " 3 " " 4 " " 5 " " 6 " " 7 " " 8 " " 9 " " 10 "-1.5 " 11 " " 12 " " 13 " " 14 " " 15 " " 16 " " 17 " " 18 " " 19 " " 20 " -1"  ///
21 " " 22 " " 23 " " 24 " " 25 " " 26 " " 27 " " 28 " " 29 " " 30 " " 31 "-0.5" 32 " " 33 " " 34 " " 35 " " 36 " " 37 " " 38 " " 39 " " 40 " " 41 "0.0"  ///
42 " " 43 " " 44 " " 45 " " 46 " " 47 " " 48 " " 49 " " 50 " " /// 
51 "0.5" 52 " " 53 " " 54 " " 55 " " 56 " " 57 " " 58 " " 59 " " 60 " " 61 "1.0" 62 " " 63 " " 64 " " 65 " " 66 " " 67 " " 68 " " 69 " " 70 " " 71 "1.5"  /// 
72 " " 73 " " 74 " " 75 " " 76 " " 77 " " 78 " " 79 " " 80 " " 81 "2" )) ///
legend(off)  ytitle("% of Aggregate Revenue Growth")  bar(2,   bc(blue)) bar(1, bc(blue)) bar(3, bc(emidblue)) xsize(2.5)  ysize(1) plotregion(margin(zero)) saving($path/output/figure_6_top, replace)

graph bar (sum) binshare_cont_pos_decomp_m binshare_gainloss_pos_decomp_m    binshare_cont_neg_decomp_m binshare_gainloss_neg_decomp_m , over(bin,gap(*1.5)     relabel(1 "-2" 2 " " 3 " " 4 " " 5 " " 6 " " 7 " " 8 " " 9 " " 10 "-1.5 " 11 " " 12 " " 13 " " 14 " " 15 " " 16 " " 17 " " 18 " " 19 " " 20 " -1"  ///
21 " " 22 " " 23 " " 24 " " 25 " " 26 " " 27 " " 28 " " 29 " " 30 " " 31 "-0.5" 32 " " 33 " " 34 " " 35 " " 36 " " 37 " " 38 " " 39 " " 40 " " 41 "0.0"  ///
42 " " 43 " " 44 " " 45 " " 46 " " 47 " " 48 " " 49 " " 50 " " /// 
51 "0.5" 52 " " 53 " " 54 " " 55 " " 56 " " 57 " " 58 " " 59 " " 60 " " 61 "1.0" 62 " " 63 " " 64 " " 65 " " 66 " " 67 " " 68 " " 69 " " 70 " " 71 "1.5"  /// 
72 " " 73 " " 74 " " 75 " " 76 " " 77 " " 78 " " 79 " " 80 " " 81 "2" )) ///
legend(order(1 "Contribution of Continued Products" 2 "Contribution of Creative Destruction" ) position(5) cols(2))  ytitle("% Contribution to Bin's Growth")  bar(2,   bc(green) lwidth(dotdash)) bar(1, bc(emidblue)) bar(4, bc(green) lwidth(vthin))  bar(3,   bc(emidblue)) xsize(2.5)  ysize(1) plotregion(margin(zero)) stack saving($path/output/figure_6_bottom, replace)

graph combine  $path/output/figure_6_top.gph $path/output/figure_6_bottom.gph , rows(2) xcommon 
graph export $path/output/figure_6.pdf , replace 
 

erase "$path/output/figure_6_bottom.gph"
erase "$path/output/figure_6_top.gph"