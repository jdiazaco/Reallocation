source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name<-""
output_dir<-paste0(output_dir, "2025/Export 22.05/")
output_dir_creator(output_dir)

cpa_or_pf<-"prodfra_plus"
if(cpa_or_pf=="cpa"){
  ext<-"_cpa"
  digits<-c(0, 1, 2, 4, 6)
  exit_digit<-"exit_6"
}else{
  if(cpa_or_pf=="prodfra_plus")
    ext<-""
  digits<-c(0, 1, 2, 4, 6, 8, 10)
  exit_digit<-"exit_10"
}

# 0) Clean data --------------------------------------------------------------

#Bring in necessary firm and product information
firm_data_select<-readRDS("sbs_br_combined.RDS") %>% filter(year>2009) # Coming from code a. 
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
product_data<-readRDS(paste0("product_data_10_digit_all_prodfra_.RDS")) # Coming from code a. 


setDT(product_data)
setkey(product_data, firmid, prodfra_plus, year)

product_data[, price:=rev/sold_q]
product_data[, ln_price:=fifelse(price!=0, log(1e-6 +price), NA_real_)]
product_data[, ln_growth_p:=ln_price-shift(ln_price), by=.(firmid, prodfra_plus)]

product_data[, s_unr:=rev/sum(rev, na.rm = T), by=.(firmid, year)]
product_data[, s_unr_lag:=shift(s_unr), by=.(firmid, prodfra_plus)]
product_data[, s_weight_unr:=(s_unr+s_unr_lag)/2]
product_data[is.na(s_weight_unr)|is.infinite(s_weight_unr), s_weight_unr:=0]

firm_index <- product_data[!is.na(ln_growth_p), .(
  delta_p_unr=sum(s_weight_unr*ln_growth_p, na.rm=T),
  total_weight=sum(s_unr, na.rm=T),
  valid=sum(s_unr, na.rm=T)>=0.95
  ), by=.(firmid, year)]

setorder(firm_index, firmid, year)
setDT(firm_index)
firm_index[valid==F, delta_p_unr:=0]
firm_index[, P_index_ln:= cumsum(delta_p_unr), by=firmid]
firm_index[, P_index_unr:=exp(P_index_ln)]
firm_price_index<-firm_index[, .(firmid, year, pf=log(1e-6 +P_index_unr))]
firm_price_index[, pf:=ifelse(is.infinite(pf), NA_real_, pf)]

df<-merge(firm_data_select, firm_price_index, by=c("firmid", "year"), all.x = T)


df<-df[order(firmid, year)]
df[, `:=`(
  ln_Y = log(1e-6 +nq),
  ln_K = log(1e-6 +capital),
  ln_L = log(1e-6 +empl),
  ln_M = log(1e-6 +raw_materials),
  wage_nominal = labor_cost /empl,
  Rshare=log(1e-6 + raw_materials/nq)
)]

df[, `:=`(
  ln_L2=ln_L^2,
  ln_K2=ln_K^2,
  ln_M2=ln_M^2,
  ln_L_K=ln_L*ln_K,
  ln_L_M=ln_L*ln_M,
  ln_K_M=ln_K*ln_M,
  ln_L_K_M=ln_L*ln_K*ln_M)]

df[, `:=`(
  priceL=pf*ln_L,
  priceK=pf*ln_K,
  priceM=pf*ln_M,
  priceL2=pf*ln_L2,
  priceK2=pf*ln_K2,
  priceM2=pf*ln_M2,
  priceLM=pf*ln_L *ln_M,
  priceLK=pf*ln_L *ln_K,
  priceMK=pf*ln_M *ln_K,
  priceLMK=pf*ln_L*ln_M*ln_K
)]

vars<-c("ln_Y", "ln_L", "ln_L2", "ln_K", "ln_K2", "ln_L_K", 
        "ln_M", "ln_M2", "ln_K_M", "ln_L_M", "ln_L_K_M", 
        "pf", "Rshare", 
        "priceL", "priceK", "priceM", 
        "priceL2", "priceK2", "priceM2", 
        "priceLM", "priceLK", "priceMK", "priceLMK")

lag_cols = paste0("l_", vars)
data_l = df %>% mutate(year = year + 1)  %>% select(firmid, year,vars)
colnames(data_l)[names(data_l) %in% vars] = lag_cols
df = merge(df, data_l,by=c('firmid','year'), all = T) 


iv_formula<-ln_Y ~ ln_L + ln_L2 + ln_K + ln_K2 + ln_L_K + 
  ln_M + ln_M2 + ln_K_M + ln_L_M + ln_L_K_M + 
  pf + Rshare + 
  priceL + priceK + priceM + 
  priceL2 + priceK2 + priceM2 + 
  priceLM + priceLK + priceMK + priceLMK | 
  ln_L + ln_L2 + ln_K + ln_K2 + ln_L_K + 
  pf + Rshare + 
  priceL + priceK + priceM + 
  priceL2 + priceK2 + priceM2 + 
  priceLM + priceLK + priceMK + priceLMK +
  l_ln_M + l_ln_M2 + l_ln_K + l_ln_L + 
  l_priceL + l_priceK + l_priceM +
  l_priceL2 + l_priceK2 + l_priceM2 +
  l_priceLM + l_priceLK + l_priceMK + l_priceLMK +
  l_pf + l_Rshare 

iv_vars<-c("ln_Y", "ln_L", "ln_L2", "ln_K", "ln_K2", "ln_L_K", 
           "ln_M", "ln_M2", "ln_K_M", "ln_L_M", "ln_L_K_M", 
           "pf", "Rshare", 
           "priceL", "priceK", "priceM", 
           "priceL2", "priceK2", "priceM2", 
           "priceLM", "priceLK", "priceMK", "priceLMK",
           "l_ln_M", "l_ln_M2", "l_ln_K", "l_ln_L", 
           "l_priceL", "l_priceK", "l_priceM",
           "l_priceL2", "l_priceK2", "l_priceM2",
           "l_priceLM", "l_priceLK", "l_priceMK", "l_priceLMK",
           "l_pf", "l_Rshare")

df_clean <- df[complete.cases(df[, ..iv_vars])]
df_clean <- df_clean[!apply(df_clean[, ..iv_vars], 1, function(row) any(is.infinite(row)))]
iv_model<-ivreg(formula=iv_formula, data=df_clean)

