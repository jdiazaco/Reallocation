
clear 
set more off
args repo_root


import delimited "$output/theory_firmsize_homogeneous_stata.csv", clear
sort series x

local ymin = -16.1180957
local xlabels 0 "1" .6931472 "2" 1.386294 "4" 2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.1588831 "64" 4.8520303 "128" 5.5451775 "256"
local ylabels 0 "1" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}" -13.8155106 "10{superscript:-6}" -16.1180957 "10{superscript:-7}"

twoway ///
    (line y x if series == "bursts_hom" & y >= `ymin', sort lcolor(blue) lwidth(vthick)) || ///
    (line y x if series == "poisson_hom" & y >= `ymin', sort lcolor(red) lpattern(dash) lwidth(vthick)) || ///
    (line y x if series == "fit" & y >= `ymin', sort lcolor(gs10) lpattern(dash) lwidth(thick)) || ///
    (scatter y x if series == "data" & y >= `ymin', msymbol(Dh) mcolor(midgreen) msize(2.5)), ///
    legend(order(1 2 4) label(1 "Innovation Bursts") label(2 "Poisson Innovation") label(4 "Data") position(1) ring(0) size(*1.4)) ///
    xtitle("Number of Products", size(*1.4)) ///
    ytitle("Rank", size(*1.4)) ///
    xlabel(`xlabels', labsize(*1.4)) ///
    ylabel(`ylabels', labsize(*1.4)) ///
    xscale(range(0 5.5451775)) ///
    yscale(range(`ymin' 0) noextend)

graph export "$output/Figure_10.pdf", replace
