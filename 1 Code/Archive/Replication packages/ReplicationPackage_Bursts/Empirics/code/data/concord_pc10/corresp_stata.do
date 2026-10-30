
*--- 1. read the edge list --------------------------------------------
import delimited using "$outpath\edge.txt", delimiter(tab) varnames(nonames) clear
rename v1 num0
rename v2 num1
* v3 is the constant weight, irrelevant for components
cap drop v3

keep num0 num1
drop if missing(num0) | missing(num1)
duplicates drop

*--- 2. build a symmetric adjacency list, with self loops --------------
tempfile dir1 selfl adj labs ids

preserve
    rename num0 node
    rename num1 nbr
    keep node nbr
    save `dir1'
restore

rename num1 node
rename num0 nbr
keep node nbr
append using `dir1'

* self loops so that every node carries at least one entry
preserve
    keep node
    duplicates drop
    gen long nbr = node
    save `selfl'
restore
append using `selfl'

duplicates drop
sort node nbr
save `adj'

*--- 3. initialise each node with its own label ------------------------
keep node
duplicates drop
gen long lab = node
sort node
save `labs'

qui count
local nnodes = r(N)
qui su node
local maxnode = r(max)
di as txt "nodes: `nnodes', highest index: `maxnode'"

if `nnodes' != `maxnode' {
    di as err "node indices are not consecutive 1..`maxnode'"
    di as err "numbering will not match the MATLAB output, investigate id_num"
    exit 459
}

*--- 4. propagate the minimum label until nothing changes --------------
local converged = 0
local iter      = 0
local maxiter   = 200

while `converged'==0 & `iter' < `maxiter' {

    local ++iter

    use `adj', clear
    rename node keepnode
    rename nbr  node
    * attach the neighbour's current label
    merge m:1 node using `labs', keep(match) nogen
    rename node nbr
    rename keepnode node

    collapse (min) newlab = lab, by(node)

    merge 1:1 node using `labs', nogen
    qui count if newlab != lab
    local nchange = r(N)

    drop lab
    rename newlab lab
    sort node
    save `labs', replace

    di as txt "iteration `iter': `nchange' labels changed"
    if `nchange'==0 local converged = 1
}

if `converged'==0 {
    di as err "label propagation did not converge in `maxiter' iterations"
    exit 498
}

*--- 5. number the components ---------------------------
* lab now holds, for every node, the lowest node index in its component
use `labs', clear
bysort lab (node): gen long csize = _N

preserve
    keep lab csize
    duplicates drop
    * size descending, ties by lowest node index ascending
    gsort -csize lab
    gen long id_conc = _n
    save `ids'
restore

merge m:1 lab using `ids', keep(match) nogen assert(match)

rename node num
keep id_conc num
order id_conc num
sort num

qui su id_conc
di as txt "components: " r(max)
