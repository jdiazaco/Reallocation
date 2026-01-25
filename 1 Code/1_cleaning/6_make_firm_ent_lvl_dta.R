# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ----------------------------------------------------------------

end <- 2022
# lifi_data <- read_parquet("C:/Users/Public/1. Microprod/0. Raw data processing/Codes/Preparation_LIFI/LIFI.parquet")
lifi_data <- as.data.table(read_rds("C:/Users/Public/1. Microprod/0. Raw data processing/Codes/Preparation_LIFI/LIFI.rds")) 
# lifi_data <- as.data.table(read_parquet("C:/Users/Public/1. Microprod/0. Raw data processing/Codes/Preparation_LIFI/LIFI.parquet")) 
firm_nace_data <- read_parquet("firm_lists/firmid_NACE_BR_year.parquet")

# Create firm groups
firmhg <- lifi_data[!is.na(firmid_hg) & firmid_hg!="" , .(list_firmid=list(unique(firmid))), by=.(firmid_hg, year)]


# Merge firm, lifi and group information
firm_data <- merge(firm_nace_data,
                   lifi_data,
                   by=c("firmid", "year"),
                   all.x=T)
firm_data <- merge(firm_data,
                   firmhg,
                   by=c("firmid_hg", "year"),
                   all.x = T)
rm(lifi_data, firmhg); gc()

# firm_data <- dummy_dataset(firm_data, "!is.na(firmid_hg)", 10)

# Create lags of key variables
firm_data <- create_lags(firm_data, c("list_firmid", "entid", "ent_type", "firmid_hg"), id_vars="firmid", time_var="year")

# Create flags for change in 
firm_data <- firm_data[, `:=`(change_ent=!(entid==lag_entid),
                              change_ent_type=!(ent_type==lag_ent_type),
                              change_firmid_hg=!(firmid_hg==lag_firmid_hg))]

# Compute similarities of groups by year
firm_data[, jaccard_group:=mapply(jaccard, lag_list_firmid, list_firmid, USE.NAMES = FALSE)]
firm_data[, jaccard_group:=round(jaccard_group, 2)]

# test <- firm_data[, .(n_change_ent=sum(change_ent, na.rm=T),
#                       n_change_firmid_hg=sum(change_firmid_hg, na.rm=T),
#                       n=.N), by=jaccard_group] %>%
#   .[, `:=`(share_change_ent=(n_change_ent/n),
#            share_change_firmid_hg=(n_change_firmid_hg/n))]
# 
# ggplot(firm_data, aes(x=jaccard_group, fill=change_firmid_hg)) + 
#   geom_histogram(position="stack", color="black")

#Define change in business group
firm_data[, change_in_bg:=fifelse((change_firmid_hg==T & jaccard_group<=threshold_jaccard_change_business_group) | change_ent, TRUE, FALSE)]

# Get group nationalities
group_nationalities <- firm_data[, .(firmid, year, group_nationality=as.character(firm_nationality))] %>% 
  .[, group_nationality:=fifelse(is.na(group_nationality), "FR", group_nationality)] %>%
  .[, group_french_or_foreign:=fifelse(group_nationality=="FR", "french", "foreign")]

# Bring group information back into general data
firm_data[, group_french_or_foreign:=NULL]
firm_data[, group_nationality:=NULL]
firm_data <- merge(firm_data,
                   group_nationalities, 
                   by.x=c("firmid_hg", "year"),
                   by.y=c("firmid", "year"),
                   all.x = T)
table(firm_data$group_french_or_foreign)

# Considering that the ent information is super accurate starting in some time, readjust group nationality information controlling for that 
firm_data[, group_french_or_foreign:=fifelse(is.na(ent_type), 
                                             group_french_or_foreign,
                                             fifelse(ent_type=="GET-MNE" | ent_type=="IND-ETR" | ent_type=="4",
                                                     "foreign",
                                                     fifelse(ent_type=="GFR-FRA" | ent_type=="GFR-MNE" | ent_type=="IND-FRA" | ent_type=="1" | ent_type=="2",
                                                             "french", 
                                                             group_french_or_foreign)))]
table(firm_data$group_french_or_foreign)



firm_data <- firm_data[, .(firmid, year, firmid_hg, firm_nationality,
                           entid, ent_nationality,  ent_type, list_firmid,
                           change_ent, change_ent_type, change_firmid_hg, jaccard_group, change_in_bg, group_nationality,      
                           group_french_or_foreign)]

test <- firm_data[, .(share_french=sum(group_french_or_foreign=="french", na.rm=T)/.N,
                      share_foreigh=sum(group_french_or_foreign=="foreign", na.rm=T)/.N), by=.(year)]
ggplot(test, aes(x=year, y=share_foreigh)) + geom_line()

write_rds(firm_data, "6_firm_ent_lvl_dta.RDS")

