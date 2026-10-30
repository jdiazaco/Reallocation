
clear 
set more off
args repo_root


import delimited "$output/theory_firmsize_stata.csv", clear
//import delimited "D:\Bursts_ReplicationPackage\Theory\KKfixed_betterCDtarget\plots\theory_firmsize_stata.csv"
sort panel variant series x

local xlabels 0 "1" .6931472 "2" 1.386294 "4" 2.0794415 "8" 2.7725887 "16" 3.4657359 "32" 4.1588831 "64" 4.8520303 "128" 5.5451775 "256"
local ylabels 0 "1" -2.3025851 "10{superscript:-1}" -4.6051702 "10{superscript:-2}" -6.9077553 "10{superscript:-3}" -9.2103404 "10{superscript:-4}" -11.512925 "10{superscript:-5}" -13.8155106 "10{superscript:-6}" -16.1180957 "10{superscript:-7}" -18.4206807 "10{superscript:-8}" -20.7232658 "10{superscript:-9}" 

foreach variant in baseline  {
    foreach panel in type1 type2 full {

        if "`panel'" == "full" {
			local outfile Figure_9a
		}
		else if "`panel'" == "type1" {
			local outfile Figure_9b
		}
		else if "`panel'" == "type2" {
			local outfile Figure_9c
		}
		local ymin = -20.72326584

        local plotcmd ///
            (line y x if panel == "`panel'" & variant == "`variant'" & series == "bursts" & y >= `ymin', sort lcolor(blue) lwidth(vthick)) || ///
            (line y x if panel == "`panel'" & variant == "`variant'" & series == "poisson" & y >= `ymin', sort lcolor(red) lpattern(dash) lwidth(vthick))

        if "`variant'" == "kkequal" {
            local plotcmd `"`plotcmd' || (line y x if panel == "`panel'" & variant == "`variant'" & series == "kkequal" & y >= `ymin', sort lcolor(gs6) lpattern(dash) lwidth(vthick))"'
        }

        if "`panel'" == "full" {
            local plotcmd `"`plotcmd' || (line y x if panel == "`panel'" & variant == "`variant'" & series == "fit" & y >= `ymin', sort lcolor(gs10) lpattern(dash) lwidth(thick)) || (scatter y x if panel == "`panel'" & variant == "`variant'" & series == "data" & y >= `ymin', msymbol(Dh) mcolor(midgreen) msize(2.5))"'
        }

        if "`panel'" == "full" & "`variant'" == "baseline" {
            local legend legend(order(1 2 4) label(1 "Innovation Bursts") label(2 "Poisson Innovation") label(4 "Data") position(7) ring(0) size(*1.4))
        }
        else if "`panel'" == "full" & "`variant'" == "kkequal" {
            local legend legend(order(1 2 3 5) label(1 "Innovation Bursts") label(2 "Poisson Innovation") label(3 "Klette and Kortum types") label(5 "Data") position(7) ring(0) size(*1.4))
        }
        else {
            local legend legend(off)
        }

        twoway `plotcmd', ///
            `legend' ///
            xtitle("Number of Products", size(*1.4)) ///
            ytitle("Rank", size(*1.4)) ///
            xlabel(`xlabels', labsize(*1.4)) ///
            ylabel(`ylabels', labsize(*1.4)) ///
            xscale(range(0 5.5451775)) ///
            yscale(range(`ymin' 0) noextend) ///
            xsize(5.33) ysize(6.0)

        graph export "$output/`outfile'.pdf", replace
    }
}
