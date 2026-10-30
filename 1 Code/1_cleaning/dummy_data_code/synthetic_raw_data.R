# =============================================================================
# Synthetic raw inputs for the full Reallocation pipeline
# =============================================================================
# Builds CASD-format raw files so that 1_cleaning (scripts 1 -> 9, skipping 3)
# and 2_analysis run end to end outside the CASD:
#   Raw_data/br|sbs|bs/{br,sbs,bs}{yr}.csv          1994-2023 (FARE/FICUS)
#   Raw_data/prodcom/new_2025/eap{yr}_pp.csv         2009-2022 (EAP)
#   external/...                                     harmonised prodfra table,
#                                                    NUTS tables, LIFI.rds
#   2 Data/...                                       PC8 entry years, unit table,
#                                                    deflators & concordances,
#                                                    markups (.dta), LinkedIn,
#                                                    trademarks, patents
#
# Calibration targets (CASD, October 2026, firms with empl >= 1):
#   firms, entry and exit rates by year; firm-size shares; EAP firms and
#   firm-product lines by year; products per firm; share of EAP firms < 20
#   employees; patenting and trademark firms (ever, BR and EAP; EAP by year).
#   May 2026 outputs (desc_Patenting, desc_Products Added) give patenting and
#   product-creation rates by size x age.
#
# Patents are the real families of 3_patent_lvl_patent_dta.parquet. Applicant
# ids are remapped to synthetic firms matched on size, EAP status and
# lifespan; families of applicants that do not map to a BR firm keep a
# synthetic non-BR id.
#
# Data-generating process (main channels):
#   - Firm size: latent Gibrat process with age drift and mean reversion,
#     mapped each year onto the observed size distribution.
#   - Product creation: Poisson additions plus Pareto-sized bursts. Burst
#     probability rises with size and a firm "innovativeness" type (theta), and
#     with recent patents, more so for small firms (Argente et al.).
#   - Turnover: productivity FE + AR(1) + small effects of patents, new
#     products and trademarks (with mild anticipation, i.e. pre-trends).
#
# Usage: Rscript synthetic_raw_data.R  (env vars REALLOCATION_SYNTHETIC_DIR,
# REALLOCATION_SYNTHETIC_SCALE override the defaults below). Then run the
# pipeline with Sys.setenv(REALLOCATION_SYNTHETIC = "TRUE") (see Main.R) or
# with run_synthetic_pipeline.R.
# =============================================================================

suppressMessages({
  library(data.table); library(arrow); library(stringr); library(haven)
})

# EAP_ONLY: simulate only the sectors EAP samples from and write only firms ever
# in EAP. Fits CASD size on a laptop and is enough for the product/patent
# analyses (1 -> 1a -> 2a -> 4 -> 2_analysis/11); BR-wide outputs (industry
# ranks, entry/exit, concentration) are not meaningful in this mode.
EAP_ONLY <- isTRUE(as.logical(Sys.getenv("REALLOCATION_SYNTHETIC_EAP_ONLY", "FALSE")))
SYN_ROOT <- Sys.getenv("REALLOCATION_SYNTHETIC_DIR",
                       if (EAP_ONLY) "C:/Users/nb/Documents/Reallocation_synthetic_eap/" else "C:/Users/nb/Documents/Reallocation_synthetic/")
SRC_DIR  <- Sys.getenv("REALLOCATION_SOURCE_DIR", "G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/2 Data/")
SCALE    <- as.numeric(Sys.getenv("REALLOCATION_SYNTHETIC_SCALE", "1"))  # 1 = CASD size
SEED     <- 20261028
set.seed(SEED)

raw_dir  <- file.path(SYN_ROOT, "Raw_data")
ext_dir  <- file.path(SYN_ROOT, "external")
data_dir <- file.path(SYN_ROOT, "2 Data")
for (d in c(file.path(raw_dir, c("br", "sbs", "bs", "prodcom/new_2025")),
            file.path(ext_dir, c("2 Data/product_harmonization_output/harmonized codes", "1 Code/Ancillary datasets/NUTS")),
            file.path(data_dir, c("Ancillary datasets", "firm_lists", "PC8_years_entry", "tm_data", "linkedin_data", "temp")),
            file.path(SYN_ROOT, "3 Output/2026"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

sc  <- function(x) round(x * SCALE)
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

# Efraimidis-Spirakis weighted sampling without replacement: indices of k draws
wsample <- function(w, k) {
  if (k <= 0) return(integer(0))
  k <- min(k, sum(w > 0))
  order(log(runif(length(w))) / w, decreasing = TRUE)[seq_len(k)]
}
rgumbel <- function(n) -log(-log(runif(n)))

# Unique 9-character alphanumeric ids, like the pseudonymised CASD SIREN
make_ids <- function(n) {
  alphabet <- c(0:9, LETTERS)
  x <- sample.int(36^8, n) - 1
  out <- character(n)
  for (k in 1:8) {
    out <- paste0(alphabet[x %% 36 + 1], out)
    x <- x %/% 36
  }
  paste0(sample(c(0:9, LETTERS[1:6]), n, replace = TRUE), out)
}


# =============================================================================
# 0. Calibration targets
# =============================================================================
targets_br <- data.table(
  year  = 1995:2022,
  firms = c(922473, 1030495, 1032415, 1007987, 1006118, 1082953, 1057787, 1097730, 1117130, 1164497,
            1130066, 1217202, 1255664, 886738, 1022965, 1045096, 1048586, 1064067, 1045251, 1066539,
            1055700, 1180541, 1283692, 1290305, 1314946, 1171881, 1054282, 1104359),
  entry = c(.190, .135, .111, .105, .104, .119, .109, .100, .103, .106, .111, .108, .107, .140,
            .108, .089, .102, .102, .079, .096, .082, .124, .118, .096, .100, .068, .070, .090),
  exit  = c(.064, .092, .100, .094, .084, .109, .076, .083, .081, .117, .069, .093, .240, .078,
            .090, .104, .090, .092, .079, .074, .051, .086, .095, .088, .139, .105, .076, .087)
)
# 1994 and 2023 are not in the CASD tables: 1994 is the first year (all firms
# "enter") and 2023 the last (all firms "exit")
demog <- rbind(
  data.table(year = 1994, firms = 880000, entry = 1, exit = .070),
  targets_br,
  data.table(year = 2023, firms = 1080000, entry = .085, exit = 1)
)

size_survival <- data.table(x = c(1, 5, 10, 20, 50, 250), S = c(1, .347, .161, .080, .027, .005))
pareto_tail   <- 1.05

targets_eap <- data.table(
  year  = 2009:2022,
  firms = c(31451, 37682, 40764, 43453, 47693, 46493, 46561, 46169, 46429, 45423, 43950, 42353, 40485, 39287),
  lines = c(56882, 65654, 71566, 76096, 82271, 80915, 81277, 80655, 81000, 79768, 77351, 75152, 73204, 71607)
)
eap_products_dist <- data.table(N_lo = c(1, 2, 3, 5, 10), N_hi = c(1, 2, 4, 9, 40),
                                share = c(.639, .194, .123, .039, .006))
eap_share_small <- .683

targets_pat <- list(ever_br = 56128, ever_eap = 9238,
                    eap_by_year = data.table(year = 1995:2022, firms = c(
                      944, 943, 1062, 1084, 1147, 1140, 1119, 1182, 1241, 1315, 1361, 1357, 1340, 1369,
                      1403, 1567, 1520, 1528, 1512, 1432, 1431, 1348, 1299, 1245, 1172, 1014, 1000, 874)))
targets_tm  <- list(ever_br = 283614, ever_eap = 16244,
                    eap_by_year = data.table(year = 1995:2022, firms = c(
                      1371, 1468, 1567, 1625, 1809, 1746, 1730, 1680, 1745, 1753, 1875, 1870, 1977, 1949,
                      2025, 2151, 2125, 2087, 1939, 1900, 1902, 1862, 1742, 1735, 1631, 1633, 1575, 1373)))


# =============================================================================
# 1. Industries
# =============================================================================
# Shares of employer firms by NACE 2-digit (approximate French FARE structure)
nace2_w <- c(
  "01" = 1.2, "02" = .3, "03" = .3, "08" = .3,
  "10" = 2.6, "11" = .3, "13" = .25, "14" = .3, "15" = .08, "16" = .45, "17" = .1, "18" = .6,
  "19" = .005, "20" = .25, "21" = .03, "22" = .35, "23" = .5, "24" = .08, "25" = 1.6, "26" = .2,
  "27" = .15, "28" = .5, "29" = .15, "30" = .06, "31" = .4, "32" = .6, "33" = .8,
  "35" = .3, "36" = .1, "37" = .1, "38" = .2,
  "41" = 2.5, "42" = .4, "43" = 13, "45" = 4, "46" = 7, "47" = 14,
  "49" = 3, "50" = .1, "51" = .05, "52" = .6, "53" = .2, "55" = 1.5, "56" = 9,
  "58" = .3, "59" = .4, "60" = .05, "61" = .1, "62" = 1.5, "63" = .3, "64" = .4, "66" = .4,
  "68" = 3, "69" = 3, "70" = 2.5, "71" = 3, "72" = .3, "73" = .8, "74" = .8, "75" = .3,
  "77" = .6, "78" = .3, "79" = .3, "80" = .3, "81" = 2, "82" = 1,
  "85" = 1.5, "86" = 4, "87" = .5, "88" = 1.5, "90" = .5, "93" = 1, "94" = .3, "95" = .4, "96" = 4
)
manuf2 <- sprintf("%02d", 10:33)
size_shift2 <- c(setNames(rep(.55, length(manuf2)), manuf2),
                 "46" = .4, "35" = 1.2, "49" = .3, "62" = .2, "41" = .1,
                 "47" = -.2, "56" = -.1, "96" = -.4, "69" = -.3, "68" = -.5, "86" = -.6)

nace_defind <- fread(file.path(SRC_DIR, "Ancillary datasets/nace_DEFind.conc"), colClasses = "character")
nace4_tab <- unique(nace_defind[, .(nace4 = str_pad(nace, 4, pad = "0"))])[, nace2 := substr(nace4, 1, 2)]
nace4_tab <- nace4_tab[nace2 %in% names(nace2_w)]
nace4_tab[, w := nace2_w[nace2] * rgamma(.N, 1.2) / .N, by = nace2]
nace4_tab[, w := w / sum(w)]

prodcom_codes <- fread(file.path(SRC_DIR, "Ancillary datasets/prodcom codes.csv"), colClasses = "character")
prodcom_codes <- prodcom_codes[nchar(code) %in% 7:8, .(pcc8 = str_pad(code, 8, pad = "0"))]
prodcom_codes <- unique(prodcom_codes[substr(pcc8, 1, 2) %in% setdiff(manuf2, "19")])
prodcom_codes[, `:=`(n4 = substr(pcc8, 1, 4), n2 = substr(pcc8, 1, 2))]

# Non-manufacturing EAP firms produce goods of a related manufacturing division
eap_other_sectors <- c("42", "43", "45", "47", "58", "68", "70", "71", "72", "73", "77", "82", "95", "96")
related_manuf <- c("42" = "23", "43" = "25", "45" = "29", "47" = "10", "58" = "18", "68" = "16",
                   "70" = "28", "71" = "28", "72" = "21", "73" = "18", "77" = "28", "82" = "18",
                   "95" = "33", "96" = "14")

# EAP_ONLY: firm counts scaled by the weight of the EAP sectors, so each sector
# keeps its CASD size
if (EAP_ONLY) {
  eap_nace2 <- c(setdiff(manuf2, "19"), eap_other_sectors)
  demog[, firms := firms * nace4_tab[nace2 %in% eap_nace2, sum(w)]]
  nace4_tab <- nace4_tab[nace2 %in% eap_nace2][, w := w / sum(w)]
}


# =============================================================================
# 2. Firm demography and size (1994-2023)
# =============================================================================
msg("Firm demography")
demog[, N := sc(firms)]
demog[, `:=`(E = round(entry * N), X = round(exit * N))]
demog[year == 1994, E := N]
F_total <- sum(demog$E)

firm <- data.table(idx = seq_len(F_total))
firm[, `:=`(
  birth = rep(demog$year, demog$E),
  nace4 = sample(nace4_tab$nace4, F_total, replace = TRUE, prob = nace4_tab$w),
  theta = rnorm(F_total),   # innovativeness type
  phi   = rnorm(F_total, 0, .5)
)]
firm[, nace2 := substr(nace4, 1, 2)]
firm[, eta := rnorm(.N) + fcoalesce(size_shift2[nace2], 0) + .15 * theta]
# Legal birth: the 1994 stock is left-censored, later entrants mostly enter at creation
firm[, legal_age0 := fifelse(birth == 1994,
                             fifelse(runif(.N) < .25, rbinom(.N, 2, .5), round(rexp(.N, 1 / 15))),
                             fifelse(runif(.N) < .75, 0L, rgeom(.N, .25) + 1L))]
firm[, legal_birth := pmax(birth - legal_age0, 1901L)]
firm[, z := eta + fifelse(birth == 1994, .35 * log1p(legal_age0) - .5, -.9) + rnorm(.N, 0, .5)]
firm[, nu := rnorm(.N, 0, .15)]

z_state  <- firm$z
nu_state <- firm$nu
death_vec <- rep(NA_integer_, F_total)
panel_list <- vector("list", nrow(demog))

for (i in seq_len(nrow(demog))) {
  t <- demog$year[i]
  alive <- which(firm$birth <= t & is.na(death_vec))
  age <- t - firm$legal_birth[alive]
  born <- firm$birth[alive] == t

  # exits: smaller and younger firms are more likely to exit
  zs <- (z_state[alive] - mean(z_state[alive])) / sd(z_state[alive])
  w_exit <- exp(-.45 * zs + .5 * (age <= 2) + .2 * (age <= 5))
  exiting <- if (demog$exit[i] >= 1) seq_along(alive) else wsample(w_exit, demog$X[i])
  dying <- logical(length(alive)); dying[exiting] <- TRUE
  death_vec[alive[dying]] <- t

  # presence: first and last year always observed; interior years have gaps
  interior <- !born & !dying
  p_t <- (demog$N[i] - sum(born | dying)) / max(sum(interior), 1)
  p_t <- min(max(p_t, .3), 1)
  present <- !interior | runif(length(alive)) < p_t

  panel_list[[i]] <- data.table(idx = alive, year = t, present = present,
                                z = z_state[alive], nu = nu_state[alive])

  # latent size and productivity evolve for all alive firms
  z_state[alive]  <- z_state[alive] + .10 * exp(-age / 4) - .04 * (z_state[alive] - firm$eta[alive]) +
    rnorm(length(alive), 0, .20)
  nu_state[alive] <- .75 * nu_state[alive] + rnorm(length(alive), 0, .15)
}
panel <- rbindlist(panel_list); rm(panel_list); gc()
firm[, death := death_vec]
rm(death_vec, z_state, nu_state)

# economic birth/death = first/last year observed with employees
obs_span <- panel[present == TRUE, .(econ_birth = min(year), econ_death = max(year)), by = idx]
firm <- merge(firm, obs_span, by = "idx", all.x = TRUE)

# Employment: map latent size ranks onto the target distribution each year
inv_survival <- function(S) {
  out <- exp(approx(log(rev(size_survival$S)), log(rev(size_survival$x)), xout = log(S), rule = 2)$y)
  tail <- S < min(size_survival$S)
  out[tail] <- max(size_survival$x) * (S[tail] / min(size_survival$S))^(-1 / pareto_tail)
  pmin(out, 150000)
}
panel[present == TRUE, empl := floor(inv_survival(1 - (frank(z) - .5) / .N)), by = year]
# SBS employment is an annual average: some firms report half units
panel[present == TRUE & empl >= 2 & runif(.N) < .15, empl := empl + .5]
panel[, age := year - firm$legal_birth[idx]]
msg("Firm-years present:", panel[present == TRUE, .N], "| firms:", uniqueN(panel[present == TRUE]$idx))


# =============================================================================
# 3. EAP membership (2009-2022)
# =============================================================================
msg("EAP membership")
firm[, eap_u := qlogis(runif(.N))]
firm[, eap_sector := fcase(nace2 %in% setdiff(manuf2, "19"), 0,
                           nace2 %in% eap_other_sectors, 3.5,
                           default = NA_real_)]
elig <- panel[present == TRUE & year %between% c(2009, 2022)]
elig[, `:=`(eap_sector = firm$eap_sector[idx], eap_u = firm$eap_u[idx])]
elig <- elig[!is.na(eap_sector)]
elig[, noise := rnorm(.N, 0, .35)]

select_eap <- function(dt, b) {
  dt[, score := eap_u + eap_sector + noise - b * log(empl)]
  dt[, rk := frank(score, ties.method = "first"), by = year]
  dt[targets_eap, on = "year", in_eap := rk <= round(sc(i.firms) * 1.015)]
}
# size gradient b chosen to match the share of EAP firms with < 20 employees
b_grid <- seq(0, 4, by = .1)
share_small <- sapply(b_grid, function(b) {
  select_eap(elig, b)
  elig[in_eap == TRUE, mean(empl < 20)]
})
b_eap <- b_grid[which.min(abs(share_small - eap_share_small))]
select_eap(elig, b_eap)
msg("EAP size gradient:", b_eap, "| share < 20:", round(elig[in_eap == TRUE, mean(empl < 20)], 3))
eap_panel <- elig[in_eap == TRUE, .(idx, year)]
rm(elig); gc()
firm[, ever_eap := idx %in% eap_panel$idx]


# =============================================================================
# 4. Patents: remap real applicants to synthetic firms
# =============================================================================
msg("Patents")
pat_raw <- as.data.table(read_parquet(file.path(SRC_DIR, "3_patent_lvl_patent_dta.parquet"),
                                      col_select = c("patent_family", "filing_year", "firmid", "ipc_subclasses_symbol")))
applicants <- pat_raw[!is.na(firmid), .(fam = uniqueN(patent_family),
                                        f0 = min(filing_year, na.rm = TRUE),
                                        f1 = max(filing_year, na.rm = TRUE)), by = firmid]
applicants <- applicants[sample.int(.N, round(.N * min(SCALE, 1)))]

# Applicant field: modal NACE 2-digit of its IPC subclasses
ipc_nace <- unique(fread(file.path(SRC_DIR, "Ancillary datasets/IPC V8_NACE Rev 2.txt")))
ipc_nace[, nace2 := substr(as.character(NACE2), 1, 2)]
ipc_nace <- ipc_nace[nace2 %in% setdiff(manuf2, "19"), .(nace2 = nace2[1]), by = IPCV2015]
ipc_raw <- pat_raw[firmid %in% applicants$firmid & !is.na(ipc_subclasses_symbol), .(firmid, ipc_subclasses_symbol)]
ipc_split <- strsplit(gsub(" ", "", ipc_raw$ipc_subclasses_symbol), ";")
ipc_long <- data.table(firmid = rep(ipc_raw$firmid, lengths(ipc_split)),
                       ipc = substr(toupper(unlist(ipc_split)), 1, 4))
rm(ipc_raw, ipc_split)
ipc_long <- ipc_nace[ipc_long, on = c(IPCV2015 = "ipc"), nomatch = NULL]
applicant_field <- ipc_long[, .N, by = .(firmid, nace2)][order(-N), .(pat_nace2 = nace2[1]), by = firmid]
applicants <- applicant_field[applicants, on = "firmid"]
rm(ipc_long); gc()

# Which applicants are BR employers: larger portfolios are more likely to be
K_br <- min(sc(targets_pat$ever_br), applicants[f1 >= 1994, .N])
applicants[, in_br := FALSE]
cand_app <- applicants[f1 >= 1994, which = TRUE]
applicants[cand_app[wsample(applicants$fam[cand_app]^.5, K_br)], in_br := TRUE]

# Synthetic patentees: big, manufacturing, EAP and innovative firms; the EAP
# bonus is set so that the number of EAP patentees matches the target
firm_size <- panel[present == TRUE, .(max_empl = max(empl)), by = idx]
firm[firm_size, on = "idx", max_empl := i.max_empl]
pat_noise <- rgumbel(nrow(firm))
pat_base <- with(firm, 1.1 * log(max_empl) + .6 * (nace2 %in% manuf2) + .9 * theta) + pat_noise
pick_patentees <- function(bonus) {
  s <- pat_base + bonus * firm$ever_eap
  order(s, decreasing = TRUE)[seq_len(K_br)]
}
bonus_grid <- seq(-3, 6, by = .1)
n_eap_pat <- sapply(bonus_grid, function(b) sum(firm$ever_eap[pick_patentees(b)]))
eap_bonus <- bonus_grid[which.min(abs(n_eap_pat - sc(targets_pat$ever_eap)))]
patentee_idx <- pick_patentees(eap_bonus)
msg("EAP bonus:", eap_bonus, "| EAP patentees:", sum(firm$ever_eap[patentee_idx]))

# Match: portfolio size <-> firm size in rank blocks; within blocks, first
# filing year <-> economic birth year
app_br <- applicants[in_br == TRUE][order(-fam)]
syn <- firm[patentee_idx, .(idx, econ_birth, s = max_empl * exp(.3 * theta))][order(-s)]
n_blocks <- max(1, round(K_br / 200))
app_br[, block := ceiling(seq_len(.N) / .N * n_blocks)]
syn[, block := ceiling(seq_len(.N) / .N * n_blocks)]
setorder(app_br, block, f0); setorder(syn, block, econ_birth)
app_br[, idx := syn$idx]
firm_map <- rbind(
  app_br[, .(firmid_real = firmid, firmid_syn = NA_character_, idx)],
  applicants[in_br == FALSE, .(firmid_real = firmid, firmid_syn = NA_character_, idx = NA_integer_)]
)
firm[, pat_nace2 := NA_character_]
firm[app_br, on = "idx", pat_nace2 := i.pat_nace2]

# Synthetic ids for every firm, patent-only applicants and trademark-only filers
n_tm_only <- sc(120000)
all_ids <- make_ids(F_total + nrow(firm_map) + n_tm_only)
firm[, firmid := all_ids[idx]]
firm_map[!is.na(idx), firmid_syn := firm$firmid[idx]]
firm_map[is.na(idx), firmid_syn := all_ids[F_total + seq_len(.N)]]
tm_only_ids <- all_ids[F_total + nrow(firm_map) + seq_len(n_tm_only)]
rm(all_ids)

# Firm-year patent families (filing year) for the DGP below
pat_fy <- pat_raw[firmid %in% app_br$firmid & !is.na(filing_year),
                  .(npf = uniqueN(patent_family)), by = .(firmid, year = as.integer(filing_year))]
pat_fy[app_br, on = "firmid", idx := i.idx]
pat_fy <- pat_fy[, .(idx, year, npf)]
rm(pat_raw); gc()


# =============================================================================
# 5. Products (EAP firms, 2009-2022)
# =============================================================================
msg("Products")
# prodfra (10-digit) universe: each PRODCOM code has 1-3 national sub-codes
prodfra_univ <- prodcom_codes[rep(seq_len(.N), 1 + rbinom(.N, 2, .2))]
prodfra_univ[, prodfra := paste0(pcc8, c("00", "10", "20")[seq_len(.N)]), by = pcc8]
units <- c("KG", "P/ST", "M2", "L", "M3", "T", "M", "PA")
code_attr <- prodcom_codes[, .(pcc8, unit = sample(units, .N, TRUE, c(.42, .25, .06, .08, .04, .09, .04, .02)),
                               price = exp(rnorm(.N, -3, 1.6)),
                               entry_year = fifelse(runif(.N) < .93, sample(1995:2008, .N, TRUE), sample(2010:2021, .N, TRUE)))]
prodfra_univ <- code_attr[prodfra_univ, on = "pcc8"]
pool_n4 <- split(prodfra_univ$prodfra, prodfra_univ$n4)
pool_n2 <- split(prodfra_univ$prodfra, prodfra_univ$n2)
pool_all <- prodfra_univ$prodfra
code_entry <- setNames(prodfra_univ$entry_year, prodfra_univ$prodfra)

eap_firms <- eap_panel[, .(fy = min(year), ly = max(year)), by = idx]
eap_firms[, `:=`(nace4 = firm$nace4[idx], nace2 = firm$nace2[idx], theta = firm$theta[idx],
                 pat_nace2 = firm$pat_nace2[idx])]
eap_firms[!nace2 %in% manuf2, `:=`(nace2 = related_manuf[nace2], nace4 = NA_character_)]
eap_firms[!is.na(nace4) & !nace4 %in% names(pool_n4), nace4 := NA_character_]
eap_firms[!nace2 %in% names(pool_n2), nace2 := sample(names(pool_n2), .N, TRUE)]

empl_lookup <- panel[present == TRUE & year >= 2008, .(idx, year, empl)]
pat_lookup <- CJ(idx = eap_firms$idx, year = 2006:2022)
pat_lookup[pat_fy, on = c("idx", "year"), npf := i.npf]
pat_lookup[is.na(npf), npf := 0]
pat_lookup[, pat_recent := shift(npf, 1) + shift(npf, 2), by = idx]

draw_codes <- function(fids, n4, n2, pn2, recent_pat) {
  u <- runif(length(fids))
  key <- fifelse(!is.na(pn2) & recent_pat > 0 & u < .45, paste0("n2_", pn2),
         fifelse(!is.na(n4) & u < .70, paste0("n4_", n4),
         fifelse(u < .92, paste0("n2_", n2), "all")))
  key[key == "n4_NA"] <- paste0("n2_", n2[key == "n4_NA"])
  code <- character(length(fids))
  for (k in unique(key)) {
    pool <- if (k == "all") pool_all else if (startsWith(k, "n4_")) pool_n4[[sub("n4_", "", k)]] else pool_n2[[sub("n2_", "", k)]]
    if (is.null(pool)) pool <- pool_all
    j <- which(key == k)
    code[j] <- pool[sample.int(length(pool), length(j), replace = TRUE)]
  }
  data.table(idx = fids, prodfra = code)
}

port <- data.table(idx = integer(), prodfra = character(), active = logical(),
                   appeal = numeric(), intro = integer())
creation_log <- list()
eap_rows <- list()
for (t in 2009:2022) {
  st <- eap_firms[fy <= t & ly >= t]
  st[empl_lookup[year == t], on = "idx", empl := i.empl]
  st[empl_lookup[year == t - 1], on = "idx", empl_l := i.empl]
  st[is.na(empl), empl := fcoalesce(empl_l, 5)]
  st[pat_lookup[year == t], on = "idx", pat_recent := i.pat_recent]
  st[is.na(pat_recent), pat_recent := 0]

  # a) initial portfolios: product counts by size rank onto the target distribution
  new_f <- st[fy == t]
  if (nrow(new_f)) {
    u <- (frank(log(new_f$empl) + rnorm(nrow(new_f), 0, 1.2) + .4 * new_f$theta) - .5) / nrow(new_f)
    cum <- cumsum(eap_products_dist$share)
    band <- findInterval(u, cum / max(cum)) + 1
    n0 <- with(eap_products_dist, round(exp(runif(length(band), log(N_lo[band]), log(N_hi[band] + .99)) - 1e-9)))
    n0 <- pmax(pmin(n0, eap_products_dist$N_hi[band]), eap_products_dist$N_lo[band])
    init <- draw_codes(rep(new_f$idx, n0), rep(new_f$nace4, n0), rep(new_f$nace2, n0),
                       rep(NA_character_, sum(n0)), 0)
    init <- unique(init)
    init[, `:=`(active = TRUE, appeal = exp(rnorm(.N, 0, 1.1)), intro = t - sample(1:8, .N, TRUE))]
    port <- rbind(port, init, use.names = TRUE)
  }

  # b) incumbents: drops, reintroductions, additions
  old_f <- st[fy < t]
  if (nrow(old_f)) {
    cur <- port[idx %in% old_f$idx]
    lagN <- cur[active == TRUE, .(lagN = .N), by = idx]
    old_f[lagN, on = "idx", lagN := i.lagN]
    old_f[is.na(lagN), lagN := 1L]
    cur[lagN, on = "idx", lagN := i.lagN]

    cur[active == TRUE, drop := runif(.N) < (.06 + .05 * log(lagN))]
    cur[active == FALSE, back := runif(.N) < .10]
    cur[drop == TRUE, active := FALSE]
    cur[back == TRUE, active := TRUE]
    keep_one <- cur[, .(n_act = sum(active)), by = idx][n_act == 0]$idx
    cur[idx %in% keep_one, active := appeal == max(appeal), by = idx]
    cur[, c("drop", "back", "lagN") := NULL]
    port <- rbind(port[!idx %in% old_f$idx], cur, use.names = TRUE)

    # additions: Poisson trickle + Pareto-sized bursts (Berlingieri et al.)
    small <- old_f$empl < 20
    gamma_pat <- fifelse(small, .35, fifelse(old_f$empl < 250, .20, .05))
    lambda <- .016 * old_f$empl^.25 * exp(.3 * old_f$theta)
    p_burst <- plogis(-5.4 + .33 * log(old_f$empl) + .5 * old_f$theta +
                        gamma_pat * asinh(old_f$pat_recent) - .25 * (firm$legal_birth[old_f$idx] < t - 20))
    burst <- runif(nrow(old_f)) < p_burst
    burst_size <- 1 + floor((old_f$lagN + 1)^.2 * runif(nrow(old_f))^(-1 / 1.6))
    n_add <- rpois(nrow(old_f), lambda) + burst * pmin(burst_size, 60)
    if (sum(n_add)) {
      adds <- draw_codes(rep(old_f$idx, n_add), rep(old_f$nace4, n_add), rep(old_f$nace2, n_add),
                         rep(old_f$pat_nace2, n_add), rep(old_f$pat_recent, n_add))
      adds <- unique(adds)[!port, on = c("idx", "prodfra")]
      adds[, `:=`(active = TRUE, appeal = exp(rnorm(.N, -.3, 1.1)), intro = t)]
      port <- rbind(port, adds, use.names = TRUE)
    }
    creation_log[[as.character(t)]] <- data.table(idx = old_f$idx, year = t, burst = burst, n_add = n_add)
  }

  # c) survey snapshot for firms sampled in t
  in_t <- eap_panel[year == t]$idx
  eap_rows[[as.character(t)]] <- port[active == TRUE & idx %in% in_t, .(idx, year = t, prodfra, appeal, intro)]
  msg("  EAP", t, "| firms:", length(in_t), "| lines:", nrow(eap_rows[[as.character(t)]]))
}
eap_long <- rbindlist(eap_rows); rm(eap_rows, port); gc()
creation <- rbindlist(creation_log)

# BR-wide statistics from the full simulated panel, before EAP_ONLY trims it
pres <- panel[present == TRUE, .(idx, year, empl)]
pres[, `:=`(eb = firm$econ_birth[idx], ed = firm$econ_death[idx])]
rep_br <- pres[, .(N = .N, entry = round(mean(year == eb), 3), exit = round(mean(year == ed), 3)), keyby = year]
rep_br <- merge(rep_br, demog[, .(year, N_target = N, entry_target = entry, exit_target = exit)], by = "year")
size_rep <- round(prop.table(table(cut(pres$empl, c(0, 4, 9, 19, 49, 249, Inf)))), 3)
rm(pres)
# Counts in CASD-size units so the in_br threshold in Main.R means the same at any SCALE
prodcom_sectors <- panel[present == TRUE & year >= 2009, .(in_br = .N / uniqueN(year) / SCALE), by = .(NACE_2d_BR = firm$nace2[idx])]
eap_sec <- eap_panel[, .(in_prodcom = .N / uniqueN(year) / SCALE), by = .(NACE_2d_BR = firm$nace2[idx])]
prodcom_sectors <- eap_sec[prodcom_sectors, on = "NACE_2d_BR"][is.na(in_prodcom), in_prodcom := 0]
prodcom_sectors[, share := in_prodcom * 100 / in_br]

if (EAP_ONLY) {
  panel <- panel[idx %in% eap_panel$idx]
  msg("EAP_ONLY: keeping", uniqueN(panel$idx), "firms,", panel[present == TRUE, .N], "firm-years")
}
gc()


# =============================================================================
# 6. Turnover, balance sheet and trademarks
# =============================================================================
msg("Financials")
fin <- panel[present == TRUE, .(idx, year, empl, nu, age)]
fin[, `:=`(nace2 = firm$nace2[idx], phi = firm$phi[idx], theta = firm$theta[idx])]
fin[pat_fy, on = c("idx", "year"), npf := i.npf]
fin[creation, on = c("idx", "year"), n_add := i.n_add]
fin[is.na(npf), npf := 0]; fin[is.na(n_add), n_add := 0]
setorder(fin, idx, year)
fin[, `:=`(npf_l1 = shift(npf, 1, 0), npf_l2 = shift(npf, 2, 0), npf_l3 = shift(npf, 3, 0),
           npf_f1 = shift(npf, 1, 0, type = "lead"), n_add_l1 = shift(n_add, 1, 0)), by = idx]

# Trademarks: firm propensity calibrated to the number of BR filers
tm_consumer <- c("10", "11", "13", "14", "15", "20", "32", "47", "56", "96")
firm[, tm_fe := rnorm(.N, 0, 1.4)]
fin[, tm_base := .55 * log(empl) + .6 * theta + firm$tm_fe[idx] + .7 * (nace2 %in% tm_consumer) + 1.2 * (age <= 1)]
c_grid <- seq(-12, -4, by = .1)
tm_intensity <- fin[, .(s = sum(exp(tm_base))), by = idx]$s
ever_tm <- sapply(c_grid, function(c0) sum(1 - exp(-exp(c0) * tm_intensity)))
c_tm <- c_grid[which.min(abs(ever_tm - sc(if (EAP_ONLY) targets_tm$ever_eap else targets_tm$ever_br)))]
fin[, num_tm := rpois(.N, exp(c_tm + tm_base))]
msg("TM constant:", c_tm, "| BR TM filers:", fin[num_tm > 0, uniqueN(idx)])

innov <- with(fin, {
  size_f <- fifelse(empl < 20, 1.5, fifelse(empl < 250, 1, .5))
  size_f * (.015 * (asinh(npf) + asinh(npf_l1) + asinh(npf_l2) + asinh(npf_l3)) + .01 * asinh(npf_f1)) +
    .04 * asinh(n_add) + .03 * asinh(n_add_l1) + .01 * asinh(num_tm)
})
sector_lp <- c(setNames(rep(log(170), length(manuf2)), manuf2), "46" = log(320), "47" = log(160),
               "43" = log(105), "41" = log(180), "56" = log(70), "35" = log(400))
fin[, nq := round(empl * exp(fcoalesce(sector_lp[nace2], log(110)) + phi + .15 * theta + nu + innov), 1)]
m_share <- c(setNames(rep(.45, length(manuf2)), manuf2), "46" = .65, "47" = .6, "43" = .35, "41" = .4, "56" = .3)
k_ratio <- c(setNames(rep(.5, length(manuf2)), manuf2), "68" = 3, "35" = 2, "49" = .8, "47" = .25)
fin[, `:=`(
  turnover      = round(nq * exp(rnorm(.N, .02, .05)), 1),
  raw_materials = round(nq * fcoalesce(m_share[nace2], .15) * exp(rnorm(.N, 0, .25)), 1),
  labor_cost    = round(empl * 42 * exp(.3 * phi + rnorm(.N, 0, .15)), 1),
  capital       = round(nq * fcoalesce(k_ratio[nace2], .3) * exp(rnorm(.N, 0, .7)), 1)
)]
fin[, c("tm_base", "nu", "age", "npf", "npf_l1", "npf_l2", "npf_l3", "npf_f1", "n_add", "n_add_l1") := NULL]
panel[, c("z", "nu") := NULL]
gc()


# =============================================================================
# 7. Write raw FARE/FICUS files (BR, SBS, BS)
# =============================================================================
msg("Writing BR/SBS/BS")
legal_forms <- c(5499, 5710, 5720, 1000, 5498, 5202, 5306)
firm[, LEGAL := sample(legal_forms, .N, TRUE, c(.38, .25, .12, .15, .06, .02, .02))]
firm[, Start_Ent := fifelse(runif(.N) < .01, "00000000",
                            sprintf("%02d%02d%04d", sample(1:28, .N, TRUE), sample(1:12, .N, TRUE), legal_birth))]
# a few firms switch activity within their division
nace_switch <- firm[runif(.N) < .03, .(idx, switch_year = birth + rgeom(.N, .15) + 1,
                                        nace4_new = sapply(nace2, function(n) sample(nace4_tab[nace2 == n]$nace4, 1)))]

for (t in 1994:2023) {
  br <- panel[year == t, .(idx, present)]
  br[, `:=`(ENT_ID = firm$firmid[idx], year = t, NACE_M = as.integer(firm$nace4[idx]),
            Start_Ent = firm$Start_Ent[idx], LEGAL = firm$LEGAL[idx])]
  sw <- nace_switch[switch_year <= t]
  br[sw, on = "idx", NACE_M := as.integer(i.nace4_new)]
  fwrite(br[, .(ENT_ID, year, NACE_M, Start_Ent, LEGAL)], file.path(raw_dir, "br", paste0("br", t, ".csv")))

  f_t <- fin[year == t]
  # absent-but-alive firms are in SBS with no employees (dropped by script 1)
  sbs <- rbind(f_t[, .(idx, SBS_12110 = nq, SBS_16110 = empl)],
               br[present == FALSE, .(idx, SBS_12110 = 0, SBS_16110 = 0)])
  sbs[runif(.N) < .01, SBS_12110 := NA]
  sbs[, `:=`(ENT_ID = firm$firmid[idx], year = t, SBS_13110 = round(SBS_12110 * runif(.N, .2, .5), 1))]
  fwrite(sbs[, .(ENT_ID, year, SBS_12110, SBS_13110, SBS_16110)], file.path(raw_dir, "sbs", paste0("sbs", t, ".csv")))

  bs <- f_t[runif(.N) > .03, .(ENT_ID = firm$firmid[idx], year = t, capital, turnover, raw_materials, labor_cost)]
  fwrite(bs, file.path(raw_dir, "bs", paste0("bs", t, ".csv")))
}


# =============================================================================
# 8. Write EAP files and product tables
# =============================================================================
msg("Writing EAP and product tables")
eap_long[prodfra_univ, on = "prodfra", `:=`(pcc8 = i.pcc8, unit = i.unit, price = i.price)]
eap_long[fin, on = c("idx", "year"), nq := i.nq]
firm[, eap_cov := fifelse(nace2 %in% manuf2, rbeta(.N, 8, 3), rbeta(.N, 2, 6))]
eap_long[, ramp := pmin(1, .3 + .35 * (year - intro))]
eap_long[, rev := round(fcoalesce(nq, 500) * firm$eap_cov[idx] * appeal * ramp / sum(appeal * ramp), 1), by = .(idx, year)]
eap_long[, prod_q := round(rev / price * exp(rnorm(.N, 0, .1)))]
eap_long[, sold_q := round(prod_q * runif(.N, .8, 1.02))]
eap_long[runif(.N) < .003, rev := NA]
eap_long[runif(.N) < .01, rev := 0]

# Harmonisation: ~4% of prodfra codes were renumbered in a break year; a few
# codes are missing from one year's nomenclature (rows dropped by script 2a)
harm <- prodfra_univ[, .(prodfra_plus = prodfra)]
harm[, `:=`(renamed = runif(.N) < .04, break_year = sample(2011:2018, .N, TRUE),
            gap_year = fifelse(runif(.N) < .005, sample(2009:2022, .N, TRUE), NA_integer_))]
harm[, old_code := paste0(substr(prodfra_plus, 1, 8), 90 + seq_len(.N)), by = .(substr(prodfra_plus, 1, 8))]
for (y in 2009:2023) {
  col <- paste0("prodfra_", y)
  harm[, (col) := fifelse(renamed & y < break_year, old_code, prodfra_plus)]
  harm[!is.na(gap_year) & gap_year == y, (col) := NA_character_]
}
fwrite(harm[, c(paste0("prodfra_", 2009:2023), "prodfra_plus"), with = FALSE],
       file.path(ext_dir, "2 Data/product_harmonization_output/harmonized codes/prodfra_harmonized_2009to2023_prodfra.csv"))

eap_long[harm, on = c(prodfra = "prodfra_plus"), `:=`(renamed = i.renamed, break_year = i.break_year, old_code = i.old_code)]
eap_long[renamed == TRUE & year < break_year, prodfra := old_code]
for (t in 2009:2022) {
  e_t <- eap_long[year == t, .(firmid = firm$firmid[idx], pcc8, prodfra, rev, prod_q, sold_q, unit, year)]
  fwrite(e_t, file.path(raw_dir, "prodcom/new_2025", paste0("eap", t, "_pp.csv")))
}

# PC8 entry years (two letter-suffixed codes keep `codes` as character, as in
# the CASD file, so that the merge with pcc8 works)
pc8 <- rbind(code_attr[, .(codes = pcc8, code_entry_year = entry_year)],
             data.table(codes = c("1013A100", "2059Z900"), code_entry_year = 2008L))
fwrite(pc8, file.path(data_dir, "PC8_years_entry/PC8_prodfra_years_entry.csv"))

unit_tab <- rbind(data.table(unit_collapsed = units, unit_obv = units, factor = 1),
                  data.table(unit_collapsed = c("KG,T", "KG,T", "L,M3", "L,M3"), unit_obv = c("KG", "T", "L", "M3"),
                             factor = c(1, 1000, 1, 1000)),
                  data.table(unit_collapsed = "", unit_obv = "M2", factor = 1))
fwrite(unit_tab, file.path(data_dir, "Ancillary datasets/unit_collapsed.csv"))


# =============================================================================
# 9. External tables: NUTS, LIFI, markups, LinkedIn, trademarks
# =============================================================================
msg("Writing NUTS, LIFI, markups, LinkedIn, trademarks")
# --- NUTS 3: 2016 codes, with the pre-2016 code for the merged regions
nuts16 <- data.table(prefix = c("FR10", "FRB0", "FRC1", "FRC2", "FRD1", "FRD2", "FRE1", "FRE2", "FRF1", "FRF2",
                                "FRF3", "FRG0", "FRH0", "FRI1", "FRI2", "FRI3", "FRJ1", "FRJ2", "FRK1", "FRK2",
                                "FRL0", "FRM0"),
                     old    = c("FR10", "FR24", "FR26", "FR43", "FR25", "FR23", "FR30", "FR22", "FR42", "FR21",
                                "FR41", "FR51", "FR52", "FR61", "FR63", "FR53", "FR81", "FR62", "FR72", "FR71",
                                "FR82", "FR83"),
                     n      = c(8, 6, 4, 4, 3, 2, 2, 3, 2, 4, 4, 5, 4, 5, 3, 4, 5, 8, 4, 8, 6, 2))
nuts16 <- nuts16[rep(seq_len(.N), n)][, k := seq_len(.N), by = prefix]
nuts16[, `:=`(nuts2016 = paste0(prefix, k), nuts2013 = paste0(old, k))]
fwrite(nuts16[nuts2013 != nuts2016, .(nuts2013, nuts2016)], file.path(ext_dir, "1 Code/Ancillary datasets/NUTS/nuts_conc.csv"))
firm[, nuts_k := sample.int(nrow(nuts16), .N, TRUE, prob = c(8, rep(1, nrow(nuts16) - 1)))]
nuts <- panel[present == TRUE, .(siren = firm$firmid[idx], year, k = firm$nuts_k[idx])]
nuts[, nuts3 := fifelse(year < 2016, nuts16$nuts2013[k], nuts16$nuts2016[k])]
nuts[runif(.N) < .01, nuts3 := ""]
fwrite(nuts[, .(siren, year, nuts3)], file.path(ext_dir, "1 Code/Ancillary datasets/NUTS/nuts_soe_addition.csv"))
rm(nuts); gc()

# --- LIFI: group membership rises with size; groups headed by their largest member
p_group <- function(e) fcase(e < 5, .03, e < 10, .08, e < 20, .15, e < 50, .30, e < 250, .60, default = .85)
grp <- firm[!is.na(max_empl) & (!EAP_ONLY | ever_eap) & runif(.N) < p_group(max_empl), .(idx, max_empl, econ_birth, econ_death)]
grp[, join := fifelse(runif(.N) < .6, econ_birth, econ_birth + floor(runif(.N) * (econ_death - econ_birth + 1)))]
# group sizes: discrete Pareto, capped at 400 French subsidiaries
grp <- grp[sample(.N)]
group_sizes <- pmin(1 + floor(runif(nrow(grp))^(-1 / 1.3)), 400)
grp[, group := rep(seq_along(group_sizes), group_sizes)[seq_len(.N)]]
n_groups <- max(grp$group)
grp[, head := firm$firmid[idx[which.max(max_empl)]], by = group]
grp_type <- data.table(group = seq_len(n_groups),
                       ent_type = sample(c("GFR-MNE", "GET-MNE", "GFR-FRA"), n_groups, TRUE, c(.25, .2, .55)))
grp_type[, ent_nationality := fifelse(ent_type == "GET-MNE",
                                      sample(c("DE", "US", "UK", "IT", "NL", "CH", "BE", "ES", "JP"), .N, TRUE), "FR")]
grp <- grp_type[grp, on = "group"]
lifi <- panel[present == TRUE & year <= 2022][grp, on = "idx", nomatch = NULL][year >= join]
lifi <- lifi[, .(firmid = firm$firmid[idx], year = as.integer(year), firmid_hg = head,
                 entid = paste0("ENT", sprintf("%07d", group)), ent_type, ent_nationality,
                 firm_nationality = fifelse(runif(.N) < .97, "FR", ent_nationality))]
setkey(lifi, firmid, year)
saveRDS(lifi, file.path(ext_dir, "LIFI.rds"))
rm(lifi, grp); gc()

# --- Markups (12 variants: TL/CD x BFSR/NoFSR x t1/t8/t10)
mk <- fin[empl >= 10 | (nace2 %in% manuf2 & empl >= 3), .(siren = firm$firmid[idx], year, phi, theta)]
mk[, base := log(1.15) + .25 * phi + .1 * theta + rnorm(.N, 0, .08)]
for (pf in c("TL", "CD")) for (fs in c("BFSR", "NoFSR")) for (th in c(1, 8, 10)) {
  mk[, (sprintf("mu_sepcal_%s_s%s_t%d", pf, fs, th)) :=
       round(exp(base + (pf == "CD") * .03 + (fs == "NoFSR") * .02 + rnorm(.N, 0, .03)), 4)]
}
mk[, c("phi", "theta", "base") := NULL]
write_dta(mk, file.path(data_dir, "dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p_XTABOND_andSTATA_DGM.dta"))
rm(mk); gc()

# --- LinkedIn roles: a third of firms with 20+ employees, 2008 onwards
li <- fin[year >= 2008 & idx %in% firm[max_empl >= 20 & runif(.N) < .35]$idx, .(idx, year, empl, theta, nace2)]
li[, emp_total := pmax(1, round(empl * runif(.N, .05, .4)))]
li[, `:=`(emp_french = round(emp_total * runif(.N, .85, 1)),
          emp_engineer = round(emp_total * plogis(-1.5 + .3 * theta + .5 * (nace2 %in% manuf2))),
          emp_data = round(emp_total * runif(.N, 0, .03)),
          emp_rnd = round(emp_total * plogis(-2.6 + .5 * theta + .4 * (nace2 %in% manuf2))),
          emp_stem = round(emp_total * plogis(-2 + .4 * theta)))]
li[, (sub("emp_", "comp_", c("emp_french", "emp_total", "emp_engineer", "emp_data", "emp_rnd", "emp_stem"))) :=
     lapply(.SD, function(x) round(x * 75 * exp(rnorm(.N, 0, .1)))),
   .SDcols = c("emp_french", "emp_total", "emp_engineer", "emp_data", "emp_rnd", "emp_stem")]
li[, `:=`(firmid = firm$firmid[idx], rcid = as.character(sample.int(3e7, .N, TRUE)), has_lei = runif(.N) < .3,
          public = runif(.N) < .02, likely_french = TRUE, subsidiary = runif(.N) < .3, has_siren = TRUE,
          rcid_count = 1L, needs_collapse = FALSE, `__index_level_0__` = seq_len(.N) - 1L)]
write_parquet(li[, .(firmid, rcid, has_lei, public, likely_french, subsidiary, has_siren, rcid_count, needs_collapse,
                     year = as.integer(year), emp_french, emp_total, emp_engineer, emp_data, emp_rnd, emp_stem,
                     comp_french, comp_total, comp_engineer, comp_data, comp_rnd, comp_stem, `__index_level_0__`)],
              file.path(data_dir, "linkedin_data/french_affiliated_firm_roles_collapsed_clean.parquet"))
rm(li); gc()

# --- Trademarks (siren-year): BR filers, pre-1994 filings of old firms, and
# trademark-only filers outside the BR
tm <- fin[num_tm > 0, .(siren = firm$firmid[idx], application_year = year, num_tm)]
old_tm <- firm[birth == 1994 & legal_birth < 1994 & idx %in% fin[num_tm > 0]$idx, .(idx, legal_birth)]
old_tm <- old_tm[, .(year = max(legal_birth, 1985):1993), by = idx][runif(.N) < .15]
tm <- rbind(tm, old_tm[, .(siren = firm$firmid[idx], application_year = year, num_tm = 1 + rpois(.N, .3))])
tm_only <- data.table(siren = rep(tm_only_ids, 1 + rpois(length(tm_only_ids), .4)))
tm_only[, `:=`(application_year = sample(1985:2022, .N, TRUE), num_tm = 1 + rpois(.N, .2))]
tm <- rbind(tm, tm_only)[, .(num_tm = sum(num_tm)), by = .(siren, application_year)]
pat_sy <- pat_fy[, .(siren = firm$firmid[idx], application_year = year, num_patent = npf)]
tm <- merge(tm, pat_sy, by = c("siren", "application_year"), all = TRUE)
tm[, `:=`(application_year = as.numeric(application_year), num_patent = as.numeric(num_patent),
          num_tm = as.numeric(num_tm))]
setorder(tm, siren, application_year)
tm[, `__index_level_0__` := seq_len(.N) - 1L]
write_parquet(tm[, .(siren, application_year, num_patent, num_tm, `__index_level_0__`)],
              file.path(data_dir, "tm_data/siren_level_patent_and_tm_final.parquet"))


# =============================================================================
# 10. Patent files with remapped applicant ids
# =============================================================================
msg("Writing patent files")
pat_full <- as.data.table(read_parquet(file.path(SRC_DIR, "3_patent_lvl_patent_dta.parquet")))
kept_fam <- unique(pat_full[firmid %in% firm_map$firmid_real]$patent_family)
na_fam <- unique(pat_full[is.na(firmid)]$patent_family)
kept_fam <- union(kept_fam, sample(na_fam, round(length(na_fam) * min(SCALE, 1))))
pat_full <- pat_full[patent_family %in% kept_fam & (is.na(firmid) | firmid %in% firm_map$firmid_real)]
pat_full[firm_map, on = c(firmid = "firmid_real"), firmid := i.firmid_syn]
pat_full[, siren := firmid]
write_parquet(pat_full, file.path(data_dir, "3_patent_lvl_patent_dta.parquet"))
rm(pat_full); gc()

cit <- readRDS(file.path(SRC_DIR, "3b_patent_citation_summary.RDS"))
cit <- cit[patent_family %in% kept_fam]
lens <- lengths(cit$firmid_prior_list)
flat <- unlist(cit$firmid_prior_list, use.names = FALSE)
flat_syn <- firm_map$firmid_syn[match(flat, firm_map$firmid_real)]
flat_syn[is.na(flat_syn)] <- "unknown"
new_list <- rep(list(character(0)), length(lens))
nz <- which(lens > 0)
new_list[nz] <- split(flat_syn, rep.int(nz, lens[nz]))
set(cit, j = "firmid_prior_list", value = list(new_list))
saveRDS(cit, file.path(data_dir, "3b_patent_citation_summary.RDS"))
rm(cit, flat, flat_syn); gc()


# =============================================================================
# 11. Ancillary datasets copied from the local data folder
# =============================================================================
for (f in c("Ancillary datasets/deflators.csv", "Ancillary datasets/nace_DEFind.conc",
            "Ancillary datasets/IPC V8_NACE Rev 2.txt", "Ancillary datasets/division_to_tech_level.xlsx",
            "Ancillary datasets/nace_labels.csv", "Ancillary datasets/prodcom codes.csv")) {
  file.copy(file.path(SRC_DIR, f), file.path(data_dir, f), overwrite = TRUE)
}
fwrite(prodcom_sectors[order(NACE_2d_BR), .(NACE_2d_BR, share, in_br, in_prodcom)],
       file.path(data_dir, "firm_lists/prodcom_sectors.csv"))
# Hand-made list read by Tools/regression_innovation_growth.r: patenting EAP
# firms in the BR, without the top 1% of patent portfolios
pat_eap <- pat_fy[idx %in% eap_panel$idx, .(fam = sum(npf)), by = idx]
fwrite(pat_eap[fam <= quantile(fam, .99), .(patenting_prod_firmids = firm$firmid[idx])],
       file.path(data_dir, "firm_lists/patenting_firms_in_BR_no_outliers.csv"))


# =============================================================================
# 12. Calibration report (raw-data analogue of the CASD check script)
# =============================================================================
msg("Calibration report")
rm(panel, empl_lookup, pat_lookup); gc()

eap_n <- eap_long[, .(firms = uniqueN(idx), lines = uniqueN(paste(idx, pcc8))), keyby = year]
eap_n <- merge(eap_n, targets_eap[, .(year, firms_target = sc(firms), lines_target = sc(lines))], by = "year")
np <- eap_long[, .(np = uniqueN(pcc8)), by = .(idx, year)]
np_rep <- round(prop.table(table(cut(np$np, c(0, 1, 2, 4, 9, Inf)))), 3)
np[fin, on = c("idx", "year"), empl := i.empl]

ever_eap_ids <- unique(eap_long$idx)
pat_rep <- c(ever_br = uniqueN(pat_fy$idx), ever_eap = uniqueN(pat_fy[idx %in% ever_eap_ids]$idx))
pat_year <- pat_fy[idx %in% ever_eap_ids & year %between% c(1995, 2022), .(firms = uniqueN(idx)), keyby = year]
tm_rep <- c(ever_br = fin[num_tm > 0, uniqueN(idx)], ever_eap = fin[num_tm > 0 & idx %in% ever_eap_ids, uniqueN(idx)])
tm_year <- fin[num_tm > 0 & idx %in% ever_eap_ids, .(firms = uniqueN(idx)), keyby = year]

creation[fin, on = c("idx", "year"), empl := i.empl]
top_share <- creation[order(-n_add), .(top5_share = sum(head(n_add, ceiling(.05 * .N))) / sum(n_add))]
eap_sectors <- eap_panel[, .(manuf = firm$nace2[idx] %in% manuf2)][, round(prop.table(table(manuf)), 3)]
add_by_size <- creation[, .(mean_add = round(mean(n_add), 3), burst_rate = round(mean(burst), 4)),
                        keyby = .(size = cut(empl, c(0, 4, 9, 19, 49, 99, 249, 499, 999, Inf)))]

sink(file.path(SYN_ROOT, "calibration_report.txt"))
cat("SCALE =", SCALE, "| seed =", SEED, "| EAP_ONLY =", EAP_ONLY, "\n")
if (EAP_ONLY) cat("EAP_ONLY: BR targets cover EAP sectors only; files hold firms ever in EAP; BR TM filers = EAP firms\n")
cat("\n== BR firms, entry, exit\n"); print(rep_br)
cat("\n== Size shares (target .654 .186 .081 .053 .022 .005)\n"); print(size_rep)
cat("\n== EAP firms and lines\n"); print(eap_n)
cat("\n== Products per firm (target .639 .194 .123 .039 .006)\n"); print(np_rep)
cat("\n== EAP share < 20 employees (target", eap_share_small, "):", round(np[, mean(empl < 20, na.rm = TRUE)], 3), "\n")
cat("\n== EAP firm-years in manufacturing NACE\n"); print(eap_sectors)
cat("\n== Patenting firms (target BR", sc(targets_pat$ever_br), "EAP", sc(targets_pat$ever_eap), ")\n"); print(pat_rep)
print(merge(pat_year, targets_pat$eap_by_year[, .(year, target = sc(firms))], by = "year"))
cat("\n== TM firms (target BR", sc(targets_tm$ever_br), "EAP", sc(targets_tm$ever_eap), ")\n"); print(tm_rep)
print(merge(tm_year, targets_tm$eap_by_year[, .(year, target = sc(firms))], by = "year"))
cat("\n== Product creation: share of new products made by the top 5% of firm-years (Berlingieri et al.: > .76)\n")
print(top_share)
cat("\n== Products added by size (May desc: 1-4 ~.025, 10-19 ~.09, 100-249 ~.2, 1000+ ~.5)\n"); print(add_by_size)
sink()
msg("Done. Report in", file.path(SYN_ROOT, "calibration_report.txt"))
