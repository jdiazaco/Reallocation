#' ------------------------------------------------------------------------------
#' Script: Patent Data Cleaning and Cumulative IPCR/NACE Generation
#' Author: Julián Díaz-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' ------------------------------------------------------------------------------

# 0) Setup ----------------------------------------------------------------------
source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name <- ""
output_dir <- paste0(output_dir, "2025/Export 08.05/DRS tables/")
output_dir_creator(output_dir)

# 1) Load and clean patent data ------------------------------------------------
patent_data_upd <- read_parquet("tm_patent/patent_record_level_final.parquet")
setDT(patent_data_upd)
patent_data_upd <- patent_data_upd[!is.na(siren)]

# 2) Expand multiple firm IDs per patent ---------------------------------------
patent_data_upd[, siren_list := strsplit(siren, ",")]
patent_data_upd <- patent_data_upd[
  , .(firmid = unlist(siren_list)), by = setdiff(names(patent_data_upd), "siren_list")
]

# 3) Process IPCR codes --------------------------------------------------------
patent_data_upd[, `:=`(
  ipcr_upd = gsub(" ", "", ipcr),
  patent_id = .I
)]

patent_data_upd[, ipcr_list := strsplit(ipcr_upd, ",")]
test <- patent_data_upd[, .(ipcr = unlist(ipcr_list)), by = .(firmid, application_year, patent_id)]

# Normalize IPCR to 4 digits + handle special codes
special_codes <- c("A61K8", "B65F1", "B65F3", "B65F5", "B65F7", "B65F9")
test[, ipcr4 := substr(toupper(ipcr), 1, 5)]
test[, ipcr4 := ifelse(ipcr4 %in% special_codes, ipcr4, substr(ipcr4, 1, 4))]

test <- unique(test[, .(firmid, application_year, patent_id, ipcr4)])

# 4) Add NACE codes via concordance --------------------------------------------
nace_ipc_concord <- unique(fread("IPC V8_NACE Rev 2.txt"))
test <- merge(test, nace_ipc_concord[, .(IPCV2015, NACE2)], 
              by.x = "ipcr4", by.y = "IPCV2015", all.x = TRUE)

# 5) Create full firm-year panel ------------------------------------------------
start<-1990; end<-2024
years_per_firm <- test[, .(min_year = start, max_year = end), by = firmid]
all_years <- years_per_firm[, .(application_year = min_year:max_year), by = firmid]

test_expanded <- merge(all_years, test, by = c("firmid", "application_year"), all = TRUE)
setorder(test_expanded, firmid, application_year)

# 6) Efficient cumulative IPCR4 and NACE2 sets ----------------------------------
cum_data <- test_expanded[
  order(firmid, application_year),
  .(ipcr4 = list(ipcr4), NACE2 = list(NACE2)), by = .(firmid, application_year)
][
  , `:=`(
    ipcr_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), ipcr4, accumulate = TRUE),
    NACE_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), NACE2, accumulate = TRUE)
  ), by = firmid
][
  , .(firmid, application_year, ipcr_cum, NACE_cum)
][
  , `:=`(
    ipcr_cum = lapply(ipcr_cum, unique),
    NACE_cum = lapply(NACE_cum, unique),
    year=application_year
  )
][, `:=`(
  n_ipcr = sapply(ipcr_cum, function(x) sum(!is.na(x))),
  n_NACE = sapply(NACE_cum, function(x) sum(!is.na(x)))
)]
growth<-growth_creator(cum_data, c("n_NACE", "n_ipcr"), 1)[, c("firmid", "year", "n_NACE_bar", "n_ipcr_bar", "n_NACE_growth", "n_ipcr_growth")]
cum_data_og<-copy(cum_data)
cum_data<-merge(cum_data, growth, by=c("firmid", "year"), all.x = T)

# Shift forward for lag comparison
cum_data_fwd <- copy(cum_data)[, application_year := application_year + 1][, c("firmid", "application_year", "ipcr_cum", "NACE_cum")]
setnames(cum_data_fwd, c("ipcr_cum", "NACE_cum"), c("ipcr_cum_l", "NACE_cum_l"))

# Merge current and lagged cumulative data
ipcr_cumulative <- merge(cum_data, cum_data_fwd, by = c("firmid", "application_year"), all.x = TRUE)

# 7) Identify new IPCR/NACE codes per year -------------------------------------
ipcr_cumulative[, `:=`(
  new_ipcr = Map(setdiff, ipcr_cum, ipcr_cum_l),
  new_NACE = Map(setdiff, NACE_cum, NACE_cum_l)
)]
ipcr_cumulative[, ipcr_creat:=sapply(new_ipcr, function(x) ifelse(length(x)>0, 1, 0))]
ipcr_cumulative[, ipcr_creat:=ifelse(ipcr_creat==1 & is.na(n_NACE_growth), 0, ipcr_creat)]

saveRDS(ipcr_cumulative, "ipcr_cumulative.RDS")

# 8) IPC analysis -----------------

product_data<-readRDS(paste0("product_level_growth_", filter_indicator, "_.RDS"))
ipcr_cumulative<-readRDS("ipcr_cumulative.RDS")
product_summary<-readRDS("product_firm_data_pre_high_growth.RDS")

ipcr_firmids<-unique(ipcr_cumulative$firmid) # length(ipcr_firmids) 118129
prod_firmids<-unique(product_summary$firmid) # length(prod_firmids) 70354
patenting_prod_firmids<-intersect(ipcr_firmids, prod_firmids) # length(patenting_prod_firmids) 9109

#patenting_products<- product_summary[firmid %in% patenting_prod_firmids] %>% 
product_summary <- product_summary[abs(empl_growth)!=2 & abs(nq_growth)!=2 & abs(rev_growth)!=2]

patenting_products<- product_summary %>%   select(firmid, year, NACE_BR, nuts3, young,
                                                  patent, patent_window, 
                                                  size, young, firm_age, number_of_products, 
                                                  superstar, superstar_cr4, superstar_tfp_99, superstar_tfp_90,
                                                  prod_creat, prod_destr, 
                                                  tm, tm_window,
                                                  new_products, first_introduction, 
                                                  net_product_creat, net_product_creat_window,
                                                  net_product_destr, net_product_destr_window,
                                                  empl_bar, empl_growth, nq_growth, nq_bar, tfp_growth, tfp_bar, rev_growth, rev_bar)

patenting_products<-merge(patenting_products, ipcr_cumulative, by=c("firmid", "year"), all.x=T)

# Create important variables and clean data
patenting_products[, `:=`(ipcr_creat=fifelse(is.na(ipcr_creat), 0, ipcr_creat))]
patenting_products<-window_var_cretor(patenting_products, "firmid", "year", "ipcr_creat", 2,0, "ipcr_creat_window", na_rm=F)
patenting_products[, `:=`(log_firm_age=log(firm_age),
                          log_empl_bar=log(empl_bar))]

# 8a) -----------------


ys<-c("net_product_creat_window", "empl_growth", "nq_growth")#, "rev_growth")
patent_var_og<-"patent"
ipcr_var_og<-"ipcr_creat"
threshold_young<-5

# make_summary_stats(patenting_products, 
#                    c("firm_age", "young", "superstar", "empl_bar", "nq_bar", "empl_growth", "nq_growth", "number_of_products", "prod_creat", "prod_destr", "patent", "tm"),
#                    "ipcr_creat",
#                    paste0("summary_stats_ipcr"))

output_dir_og<-output_dir
patenting_products_og<-patenting_products



for(type in c("patenting_firms")){
  
  output_dir<-paste0(output_dir_og, type, "/")
  if(!dir.exists(output_dir)){
    output_dir_creator(output_dir)
  }
  
  for(subset in c("all", "young", "mature")){

    patenting_products<-age_data_filter(patenting_products_og, threshold_young, subset)
    
    if(type=="patenting_firms"){
      patenting_products<- patenting_products[firmid %in% patenting_prod_firmids] 
    }
    
    output_dir<-paste0(output_dir_og, type, "/", subset, "/")
    if(!dir.exists(output_dir)){
      output_dir_creator(output_dir)
    }
    
    for(y in ys){
      for (x in c("", "_window")){
        
        if(y=="net_product_creat" & x=="_window"){
          y<-paste0(y, x)
        }
        patent_var<-paste0(patent_var_og, x)
        ipcr_var<-paste0(ipcr_var_og, x)
        print(paste0(y, " ~ ", patent_var, " + ", ipcr_var))
         
        models<- list(feols(.[y]  ~ .[patent_var] + .[ipcr_var] , patenting_products),
                      feols(.[y]  ~ .[patent_var] + .[ipcr_var]   | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ .[patent_var] + .[ipcr_var]   | NACE_BR^year + firmid, patenting_products),
                      feols(.[y]  ~ .[patent_var] + .[ipcr_var]  + log(firm_age) + log(empl_bar), patenting_products),
                      feols(.[y]  ~ .[patent_var] + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ .[patent_var] + .[ipcr_var]  + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ .[patent_var] + .[ipcr_var]  + log(firm_age) + log(empl_bar) | NACE_BR + year, patenting_products),
                      # feols(.[y]  ~ .[patent_var]*superstar  + .[ipcr_var]*superstar  + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      # feols(.[y]  ~ .[patent_var]*young + .[ipcr_var]*young + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ .[patent_var] + .[ipcr_var]  + log(firm_age) + log(empl_bar) + log(tfp_bar) | NACE_BR^year, patenting_products))
        
        modelsummary(
          models,
          output = paste0(output_dir, "regressions_ipcr_addition_", y, x, ".tex"), 
          label = paste0("regressions_ipcr_addition_", y, x),
          stars = TRUE, 
          title = tools::toTitleCase(paste0(gsub("_", " ", y), " on patenting - Sample: ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
        )
        
        results<-dynamic_reg_reallocation(patenting_products,
                                          y=y,
                                          x=c(patent_var, ipcr_var) ,
                                          fix_eff="NACE_BR^year",
                                          weight_var = NULL,
                                          disag_var="size",
                                          n_lags_bw = 4,
                                          n_lags_fw = 4)
        # test<-results[variable=="all"]
        dynamic_reg_graphs(results, 
                           fix_eff = "NACE_BR^year", 
                           output_dir, 
                           paste0("regressions_ipcr_addition_", y, x), 
                           n_lags_bw=4, n_lags_fw=4)
        # results; fix_eff = ""; output_dir; paste0("regressions_ipcr_addition_", y, x); n_lags_bw=4; n_lags_fw=4
        
        print(paste0(output_dir, "regressions_ipcr_addition_", y, x, ".tex"))
      }
    }
    
  }
  
}

inno_vars<-c("tm_window", "tm_window*net_product_creat_window", "net_product_destr_window", "patent_window", "patent_window*net_product_creat_window",  "patent_window + ipcr_creat_window", "patent_window + ipcr_creat_window + tm_window + net_product_destr_window")
graphs<-T

for(type in c("patenting_firms", "all_firms")){
  
  output_dir<-paste0(output_dir_og, type, "/")
  if(!dir.exists(output_dir)){
    output_dir_creator(output_dir)
  }
  
  for(subset in c("all", "young", "mature")){
    
    patenting_products<-age_data_filter(patenting_products_og, threshold_young, subset)
    
    if(type=="patenting_firms"){
      patenting_products<- patenting_products[firmid %in% patenting_prod_firmids] 
    }
    
    output_dir<-paste0(output_dir_og, type, "/", subset, "/")
    if(!dir.exists(output_dir)){
      output_dir_creator(output_dir)
    }
    
    
    
    for(y in ys){
      
      
      for (inno_var in inno_vars){
        
        print(paste0(y, " ~ ", inno_var))
        
        models<- list(feols(.[y]  ~ .[inno_var]   | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ .[inno_var]   | NACE_BR^year + firmid, patenting_products),
                      feols(.[y]  ~ .[inno_var]  + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ (.[inno_var])*superstar  + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ (.[inno_var])*superstar_cr4  + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ (.[inno_var])*superstar_tfp_99  + log(firm_age) + log(empl_bar) | NACE_BR^year, patenting_products),
                      feols(.[y]  ~ (.[inno_var])*young*size  + log(empl_bar) | NACE_BR^year, patenting_products))
        
        modelsummary(
          models,
          output = paste0(output_dir, "regressions_ipcr_addition_", y, "_", gsub("\\*", "_", inno_var) , ".tex"), 
          label = paste0("regressions_ipcr_addition_", y, "_", gsub("\\*", "_", inno_var)),
          stars = TRUE, 
          title = tools::toTitleCase(paste0(gsub("_", " ", y), " on ", gsub("_", " ", gsub("\\*", "_", inno_var)), " - Sample: ", subset, " within ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
        )
        
        if(graphs){
          results<-dynamic_reg_reallocation(patenting_products,
                                            y=y,
                                            x=c(patent_var, ipcr_var) ,
                                            fix_eff="NACE_BR^year",
                                            weight_var = NULL,
                                            disag_var="size",
                                            n_lags_bw = 4,
                                            n_lags_fw = 4)
          # test<-results[variable=="all"]
          dynamic_reg_graphs(results, 
                             fix_eff = "NACE_BR^year", 
                             output_dir, 
                             paste0("regressions_ipcr_addition_", y, "_", inno_var), 
                             n_lags_bw=4, n_lags_fw=4)
          # results; fix_eff = ""; output_dir; paste0("regressions_ipcr_addition_", y, x); n_lags_bw=4; n_lags_fw=4
          
        }
        print(paste0(output_dir, "regressions_ipcr_addition_", y, "_", inno_var, ".tex"))
        
      }
    }
  }
}

patenting_products<-patenting_products_og[, `:=`(young_small=fifelse(young==1 & size=="small", 1, 0))]
patenting_products<-patenting_products_og[, `:=`(young_large=fifelse(young==1 & size!="small", 1, 0))]
patenting_products<-patenting_products_og[, `:=`(mature_small=fifelse(young==0 & size=="small", 1, 0))]
patenting_products<-patenting_products_og[, `:=`(mature_medium=fifelse(young==0 & size=="medium", 1, 0))]
patenting_products<-patenting_products_og[, `:=`(mature_large=fifelse(young==0 & size=="large", 1, 0))]



for(type in c("all_firms")){
  
  output_dir<-paste0(output_dir_og, type, "/")
  if(!dir.exists(output_dir)){
    output_dir_creator(output_dir)
  }
  
  for(subset in c("all")){
    
    patenting_products<-age_data_filter(patenting_products_og, threshold_young, subset)
    
    if(type=="patenting_firms"){
      patenting_products<- patenting_products[firmid %in% patenting_prod_firmids] 
    }
    
    output_dir<-paste0(output_dir_og, type, "/", subset, "/")
    if(!dir.exists(output_dir)){
      output_dir_creator(output_dir)
    }
    
    for(y in ys){
      
      for (inno_var in inno_vars){
        
        print(paste0(y, " ~ ", inno_var))
        
        models<- list("Y. Small" = feols(.[y]  ~ (.[inno_var])*young_small  | NACE_BR^year, patenting_products),
                      "Y. NonSmall" = feols(.[y]  ~ (.[inno_var])*young_large  | NACE_BR^year, patenting_products),
                      "M. Small" = feols(.[y]  ~ (.[inno_var])*mature_small  | NACE_BR^year, patenting_products),
                      "M. Medium" = feols(.[y]  ~ (.[inno_var])*mature_medium  | NACE_BR^year, patenting_products),
                      "M. Large" = feols(.[y]  ~ (.[inno_var])*mature_large  | NACE_BR^year, patenting_products),
                      "SS CR4" = feols(.[y]  ~ (.[inno_var])*superstar_cr4  | NACE_BR^year, patenting_products),
                      "SS Rev." = feols(.[y]  ~ (.[inno_var])*superstar  | NACE_BR^year, patenting_products),
                      "SS TFP 90" = feols(.[y]  ~ (.[inno_var])*superstar_tfp_90  | NACE_BR^year, patenting_products),
                      "SS TFP 99" =feols(.[y]  ~ (.[inno_var])*superstar_tfp_99  | NACE_BR^year, patenting_products))
        
        vars_interactions<-c("young_small", "young_large", "mature_small", "mature_medium", "mature_large", "superstar_cr4TRUE", "superstarTRUE", "superstar_tfp_90TRUE", "superstar_tfp_99TRUE")
        cum_coef_maps<-c()
        
        split_inno_var<-strsplit(inno_var, "\\*")[[1]]
        split_inno_var<-trimws(split_inno_var)
        
        if(length(split_inno_var)!=1){
          keys<-paste0(paste(split_inno_var, collapse = ":"), ":", vars_interactions)
          values<-rep(paste0(paste(split_inno_var, collapse =":"), ":category"), length(keys))
          coef_map<-setNames(values, keys)
          cum_coef_maps<-c(cum_coef_maps, coef_map)
        }else{
          split_inno_var<-strsplit(split_inno_var, "\\+")[[1]]
          split_inno_var<-trimws(split_inno_var)
        }
        
        
        for(var in split_inno_var){
          keys<-paste0(var, ":", vars_interactions)
          values<-rep(paste0(var, ":category"), length(keys))
          coef_map<-setNames(values, keys)
          cum_coef_maps<-c(cum_coef_maps, coef_map)
        }
        for(var in vars_interactions){
          keys<-paste0(var)
          values<-rep(paste0("category"), length(keys))
          coef_map<-setNames(values, keys)
          cum_coef_maps<-c(cum_coef_maps, coef_map)
        }

        modelsummary(
          models,
          coef_map = cum_coef_maps,
          output = paste0(output_dir, "regressions_ipcr_addition_", y, "_", gsub("\\*", "_", inno_var) , ".tex"), 
          label = paste0("regressions_ipcr_addition_", y, "_", gsub("\\*", "_", inno_var)),
          stars = TRUE, 
          title = tools::toTitleCase(paste0(gsub("_", " ", y), " on ", gsub("_", " ", gsub("\\*", "_", inno_var)), " - Sample: ", subset, " within ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
        )
        
        if(graphs){

          tidy_models<-imap(models, function(model, model_name){
            tidy(model, conf.int=T) %>% filter(str_detect(term, ".*:.*")) %>% mutate(model_label=model_name)
          })

          results_df<-bind_rows(tidy_models)
          setDT(results_df)
          results_df[, group:=fifelse(grepl("Y.", model_label), "Young", 
                                      fifelse(grepl("SS", model_label), "Superstar", 
                                              fifelse(grepl("M.", model_label), "Mature", NA_character_)))]
          
          setDT(results_df)
          results_df[, c("matched_var", "term_clean"):={
            matched<-NA_character_
            for(v in vars_interactions){
              if(str_ends(term, v)){
                matched<-v
                break
              }
            }
            cleaned<-if(!is.na(matched)) str_remove(term, paste0(matched)) else "term"
            list(matched, cleaned)
          }, by=seq_len(nrow(results_df))]
          
          pattern<-paste(paste0(":", vars_interactions), collapse="|")
          results_df[, term_clean:=str_remove_all(term, pattern)]
          results_df<-results_df[!is.na(matched_var)]
      
          results_df$model_label <- factor(results_df$model_label, levels=names(models))
          
          for(coefficient in unique(results_df$term_clean)){
            
            results_df_temp<-results_df[term_clean==coefficient]
            
            ggplot(results_df_temp, aes(x=model_label, y=estimate, ymin=conf.low, ymax=conf.high, color=group))+
              geom_pointrange()+
              geom_hline(yintercept = 0, linetype="dashed") + 
              labs(x=tools::toTitleCase(paste0("Interaction with ", gsub("_", " ", gsub(":", "× ", gsub("_window", " W", coefficient))))),
                   y=paste("Estimate (with 95% CI)"),
                   title=tools::toTitleCase(paste0("Differential correlations of ", 
                                                   gsub("_", " ", gsub("window", "W", y)), 
                                                   " = ", 
                                                   gsub("_", " ", gsub("_window", " W",  gsub("\\*", "× ", inno_var))))),
                   subtitle = tools::toTitleCase(paste0("Variable: ", gsub(" window", "  W",  gsub(":", " × ", gsub("_", " ", coefficient) )))))
            ggsave(paste0(output_dir, "", y, "_", gsub("\\*", "_", inno_var) , "_param_",  gsub(":", " x ", coefficient), ".png"), height=4, width = 8)
          }
        }
        print(paste0(output_dir, "regressions_ipcr_addition_", y, "_", inno_var, ".tex"))
      }
    }
  }
}


# 8b) -----------------

setDT(patenting_products)

start_year<-1991
end_year<-2022

#' Create firm typology, distinguishing firms that never patent, 
#' that have patented at some point in the same ipc cluster, 
#' and that have patented at some point in new ipc codes
firm_typology <- patenting_products[, .(sum_patenting=sum(patent, na.rm = T), 
                                        sum_ipcr_creat=sum(ipcr_creat, na.rm = T)),
                                        by=.(firmid)][
                                          , pat_firm_type:=fifelse(sum_patenting==0, "never patent",
                                                                   fifelse(sum_ipcr_creat>0, "new ipcr", "same ipcr"))
                                        ][
                                          , c("firmid", "pat_firm_type")
                                        ]
patenting_products<-merge(patenting_products, firm_typology, by="firmid", all.x=T)

#' Create product introduction rates by 
patenting_year<-patenting_products[, .(net_product_creat_wt=weighted.mean(net_product_creat, nq_bar, na.rm=T),
                                       net_product_creat=mean(net_product_creat, na.rm=T)), by=.(year, pat_firm_type)]

unweighted<-ggplot(patenting_year, aes(x=year, y=net_product_creat, color=pat_firm_type)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(min(patenting_products$year), max(patenting_products$year), by=1))+
  labs(title="Net Product Creation Over Time by Firm Type",
       subtitle="Unweigthed Average",
       x="Year",
       y="Net Product Creation",
       color="Firm Type")+
  theme_minimal() +
  scale_y_continuous(limits=c(0,0.25))

weighted<-ggplot(patenting_year, aes(x=year, y=net_product_creat_wt, color=pat_firm_type)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(min(patenting_products$year), max(patenting_products$year), by=1))+
  labs(subtitle="Weighted by Average Revenue",
       x="Year",
       y="Net Product Creation",
       color="Firm Type")+
  theme_minimal() +
  scale_y_continuous(limits=c(0,0.25))

plot<-unweighted +  theme(legend.position = "none") + weighted 
print(plot)
ggsave(paste0(output_dir, "net_product_creat_time_pat_firm_type.png"), height = 5, width = 10)


ipcr_year<-ipcr_cumulative[, .(ipcr_creat=mean(ipcr_creat, na.rm=T)), by=.(year)][year %in% start_year:end_year]

ggplot(ipcr_year, aes(x=year, y=ipcr_creat)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(min(ipcr_year$year), max(ipcr_year$year), by=1))+
  labs(title="Share of Patenting Firms Expanding into New IPC Codes",
       x="Year",
       y="Share of Patenting Firms",
       color="Firm Type")+
  theme_minimal() +
  expand_limits(y=0)
ggsave(paste0(output_dir, "share_pat_firms_new_ipc.png"), height = 5, width = 9)



