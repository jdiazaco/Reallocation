# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ------------------
product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet"))

# 1) Create size measures: rank within product categories, size buckets (quartile, decile, percentile, 1000tile), leader and top_4_leaders
product_data[, `:=`(
  rank_within_pc = frank(rev_bar, ties.method = "average", na.last = "keep"),
  n_firms_in_pc  = .N
), by = .(year, get(cpa_or_pf))] %>%
  .[, `:=`(
    size_quartile = as.integer(ifelse(n_firms_in_pc > 4, pmin(4L, ceiling(4 * rank_within_pc / n_firms_in_pc)), NA)),
    size_decile = as.integer(ifelse(n_firms_in_pc > 10, pmin(10L, ceiling(10 * rank_within_pc / n_firms_in_pc)), NA)),
    size_percentile = as.integer(ifelse(n_firms_in_pc > 100, pmin(100L, ceiling(100 * rank_within_pc / n_firms_in_pc)), NA)),
    size_1000tile = as.integer(ifelse(n_firms_in_pc > 1000, pmin(1000L, ceiling(1000 * rank_within_pc / n_firms_in_pc)), NA)),
    leader = ifelse(n_firms_in_pc > 1, ifelse( rank_within_pc == n_firms_in_pc, 1L, 0L), NA),
    top_4_leaders = ifelse(n_firms_in_pc > 8, ifelse(rank_within_pc > (n_firms_in_pc - 4), 1L, 0L), NA), 
    top_10_leaders = ifelse(n_firms_in_pc > 20, ifelse(rank_within_pc > (n_firms_in_pc - 10), 1L, 0L), NA)
  )] %>%
  # Add a measure of how far away leaders are from the rest of the distribution, share of leaders (top1, top4, top10) in total industry revenue
  .[, `:=`(
    # leader_rev_share = sum(within_industry_rev_share[leader == 1], na.rm = TRUE),
    # top_4_leaders_rev_share = sum(within_industry_rev_share[top_4_leaders == 1], na.rm = TRUE),
    # top_10_leaders_rev_share = sum(within_industry_rev_share[top_10_leaders == 1], na.rm = TRUE),
    diff_leader_vs_2nd = 
      {
        # Find the second highest rev_bar within each (year, NACE_BR) group
        rev_bars <- rev_bar[order(-rev_bar)]
        if (length(rev_bars) > 1 && !is.na(rev_bars[2]) && rev_bars[1] != 0) {
          (rev_bars[1] - rev_bars[2]) / rev_bars[1]
        } else {
          NA_real_
        }
      }
  ), by = .(year, get(cpa_or_pf))] 
setDT(product_data)

# Check that all is well
View(product_data[prodcom=="24511340"] %>% select(firmid, year, cpa_or_pf, rev, rev_bar, 
                                                  rank_within_pc, n_firms_in_pc, size_quartile, 
                                                  size_decile, size_percentile, size_1000tile, 
                                                  leader, top_4_leaders, top_10_leaders, 
                                                  diff_leader_vs_2nd)%>% 
       setorder(n_firms_in_pc, prodcom, year, rank_within_pc)
) 

# Make product maket level leaders and players dataset
product_mkt_data <- product_data[, .(leader=list(unique(firmid[leader==1])),
                                     top_4_leaders=list(unique(firmid[top_4_leaders==1])),
                                     top_10_leaders=list(unique(firmid[top_10_leaders==1])),
                                     firms_in_pc=list(unique(firmid)),
                                     n_firms_in_pc = n_distinct(firmid),
                                     diff_leader_vs_2nd = 
                                       {
                                         # Find the second highest rev_bar within each (year, NACE_BR) group
                                         rev_bars <- rev_bar[order(-rev_bar)]
                                         if (length(rev_bars) > 1 && !is.na(rev_bars[2]) && rev_bars[1] != 0) {
                                           (rev_bars[1] - rev_bars[2]) / rev_bars[1]
                                         } else {
                                           NA_real_
                                         }
                                       }), by=.(get(cpa_or_pf), year)] %>%
  .[, (cpa_or_pf):=get] %>% .[, get:=NULL]

# Bring product market level information to firm-product level data
product_data <- merge(product_data %>% select(-c(leader, top_4_leaders, top_10_leaders, n_firms_in_pc, diff_leader_vs_2nd)), 
                      product_mkt_data, by=c(cpa_or_pf, "year"), all.x = T)
product_firm_data <- product_data[, .(leaders=list(unique(unlist(leader))),
                                      top_4_leaders=list(unique(unlist(top_4_leaders))),
                                      top_10_leaders=list(unique(unlist(top_10_leaders))),
                                      firms_in_pc=list(unique(unlist(firms_in_pc)))), by=.(firmid, year)]




