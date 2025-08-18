"
With this script I want to capture the product switching, patenting and trademarking
differences between high-growth young firms and other young firms. 
In other words, I attempt to answer: Are there differences in the innovation and
product portfolio decisions of high growth young firms that make them succeed?

Author: Juli?n D?az-Acosta
Last update: 28.02.25
"
# 0) setup -------------------------------------------------------------------

source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name<-""
  output_dir<-paste0(output_dir, "2025/Export 27.03/High growth young firms/")
  output_dir_creator(output_dir)

cpa_or_pf<-"cpa"
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

#Bring in necessary firm and product information
firm_data_select<-readRDS("sbs_br_data_prodcom_firms.RDS") %>% filter(year>2009)
firm_age<-readRDS("BR_earliest_year_firm_birth.RDS")

nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
product_data<-readRDS(paste0("product_level_growth_", filter_indicator, ext, "_.RDS"))

#' Bring in product, patent and trademark data.
product_summary<-readRDS(paste0("product_creation_destruction", ext, ".RDS"))
patent_tm_data<-readRDS("patent_tm_clean.RDS")
setDT(patent_tm_data)

# Check only prodcom firms are present in the firm_data_select sample
if(length(setdiff(unique(firm_data_select$firmid), unique(product_data$firmid)))!=0){
  stop("Firm data does not contain only prodcom firms. Check modules a and c and come back")
}

window_length<-2

# Create growth measures for patent and tm variables
patent_growth<-growth_creator(patent_tm_data, "total_patent", window_length) %>% select(firmid, year, total_patent_l, total_patent_bar, total_patent_growth)
tm_growth<-growth_creator(patent_tm_data, "total_tm", window_length) %>% select(firmid, year, total_tm_l, total_tm_bar, total_tm_growth)
patent_tm_data<-merge(patent_tm_data, patent_growth, by=c("firmid", "year"), all.x=T)
patent_tm_data<-merge(patent_tm_data, tm_growth, by=c("firmid", "year"), all.x=T)


#' Create a two year time window for patenting and trademark after transforming the p and tm info into dummies.
patent_tm_data[, patent:=ifelse(num_patent<=0 | is.na(num_patent), 0, 1)]
patent_tm_data<-window_var_cretor(patent_tm_data, "firmid", "year", "patent", window_length, "patent_window_temp", na_rm=T)
patent_tm_data[, tm:=ifelse(num_tm<=0 | is.na(num_tm), 0, 1)]
patent_tm_data<-window_var_cretor(patent_tm_data, "firmid", "year", "tm", window_length, "tm_window_temp", na_rm=T)

#' Bring this information into the product data. Create an independent dummy and two year time window for p and tm.
product_summary<-merge(product_summary, patent_tm_data, by=c("firmid", "year"), all.x = T)
product_summary[, patent:=ifelse(num_patent<=0 | is.na(num_patent), 0, 1)]
product_summary<-window_var_cretor(product_summary, "firmid", "year", "patent", window_length, "patent_window", na_rm=T)
product_summary[, tm:=ifelse(num_tm<=0 | is.na(num_tm), 0, 1)]
product_summary<-window_var_cretor(product_summary, "firmid", "year", "tm", window_length, "tm_window", na_rm=T)


#' Because some p and tm is left out of the product data when merging (all.x=T), the new time window may
#' be overlooking p and tms granted before the start of the product panel. Adjust patent_window for this.
table(product_summary$patent_window, useNA = "always") # This should be only 1 or 0, no NAs
table(product_summary$tm_window, useNA = "always") # This should be only 1 or 0, no NAs
product_summary[, patent_window:=ifelse(patent_window==0 & patent_window_temp==1 & !is.na(patent_window_temp), 1, patent_window)]
product_summary[, tm_window:=ifelse(tm_window==0 & tm_window_temp==1 & !is.na(tm_window_temp), 1, tm_window)]
product_summary[, log_n_products:=log(number_of_products)]

# Adjust product creation and destruction measures as net variables (new-old)
product_summary[, `:=`(net_product_change=new_products-destroyed_products)]
product_summary[, `:=`(net_product_creat=ifelse(net_product_change>0, 1, 0),
                       net_product_destr=ifelse(net_product_change<0, 1, 0))]

#' Create windows for product creation and destruction variables
#' emember that our product data is left censored, so time windows that go to years before our first data point should be NAs (na_rm=F, )
product_summary<-window_var_cretor(product_summary, "firmid", "year", "net_product_creat", window_length, "net_product_creat_window", na_rm=F) 
product_summary<-window_var_cretor(product_summary, "firmid", "year", "net_product_destr", window_length, "net_product_destr_window", na_rm=F)

# Adjust firm age and merge firm with firmdata select
# firm_data<-readRDS('sbs_br_combined_cleaned.rds') #Coming from "a. Data preparation.R" part 2
# firm_age<-firm_data[, .(birth_year_adj = min(year)), by = .(firmid)]
# saveRDS(firm_age, "BR_earliest_year_firm_birth.RDS")
# firm_data_select<-merge(firm_data_select, firm_age, by="firmid", all.x = T)
# firm_data_select<-firm_data_select[, birth_year_adj:=ifelse(birth_year_adj==)]
# firm_data_select<-firm_data_select[, firm_age:=(year-birth_year_adj)]
product_summary<-merge(product_summary, firm_data_select, by=c("firmid", "year"), all.x = T)

# 1) Distributions of patenting and patenting intensity per age -------------------------------------------------------------------

# Bring in BR data and merge PC firm data with patenting data
firm_data<-readRDS('sbs_br_combined_cleaned.rds') #Coming from "a. Data preparation.R" part 2
firm_data[, `:=`(birth_year_adj = min(year)), by = firmid]
firm_data <- firm_data[, firm_age:=(year-birth_year_adj)]
firm_data <- merge(firm_data, patent_tm_data, by=c("firmid", "year"), all.x = T)
firm_data_select <- merge(firm_data_select, patent_tm_data, by=c("firmid", "year"), all.x = T)

inv_data<-fread("dataset, MD_in
firm_data, BR
                firm_data_select, PC")

for (i in 1:nrow(inv_data)){
  
  i<-2
  og_elements<-ls()
  
  # Bring in data and the MD indicator for exporting later
  dt<-inv_data[i]$dataset
  MD_in<-inv_data[i]$MD_in
  data<-copy(get(dt))
  
  # Create patent and trademarks per worker at the firm level
  data<-data[, `:=`(pat_per_worker=ifelse(empl>0, num_patent/empl, NA),
                    tm_per_worker=ifelse(empl>0, num_tm/empl, NA))]
  
  # Create number of patent, trademarks and firms to then calculate patent and trademark shares
  patent_intensity<-data[, .(num_patents=sum(num_patent, na.rm = T),
                             num_tm=sum(num_tm, na.rm=T),
                             num_firms=.N), by=.(firm_age)]
  patent_intensity<-patent_intensity[, `:=`(pat_share=num_patents/num_firms,
                                            tm_share=num_tm/num_firms)]
  
  # Create patent trademark intensity measures (number of pats and tm per worker)
  pat_per_worker<-data[, .(pat_per_worker_uw=mean(pat_per_worker, na.rm=T), # NA rm here conditions on patenting (firms not patenting, which are NAs, are excluded)
                           tm_per_worker_uw=mean(tm_per_worker, na.rm=T)) ,by=.(firm_age)]
  
  plots_info<-fread("dataset, y_var, title, y_label, plot_name,
  patent_intensity, pat_share, Distribution of Patenting by Firm Age, Av. Patents per firm, pat_share
  patent_intensity, tm_share, Distribution of Trademarking by Firm Age, Av. Trademarks per Firm, tm_share
  pat_per_worker, pat_per_worker_uw, Patent Intensity, Patents per Worker, pat_per_worker_uw
  pat_per_worker, tm_per_worker_uw, Trademark Intensity, Trademarks per Worker, tm_per_worker_uw")
  
  plots<-list()
  
  for(i in 1:nrow(plots_info)){
    
    # Duplicate dataset
    data_plot<-copy(get(plots_info[i]$dataset))

    # Create plot
    plot_temp<-ggplot(data_plot, aes(x=firm_age, y=.data[[plots_info[i]$y_var]])) +
      geom_bar(stat="identity") + 
      scale_x_continuous(breaks=seq(0,27, by=1),
                         labels=c(as.character(0:26),"27+"))+
      theme_minimal() +
      theme(plot.title = element_text(hjust=0.5))+
      labs(title=plots_info[i]$title,
           x="Firm Age",
           y=paste0(plots_info[i]$y_label))
    
    # Store plot in list
    if(i==1){
      plots<-plot_temp
    }else{
      plots<-plots+plot_temp
    }
  }
  
  # Save the plot
  ggsave(paste0(output_dir, "patent_instensity_", MD_in, ".png"), plots,  height=8, width = 12)
  
  # Remove created elements
  objects_to_remove<-setdiff(ls(), og_elements)

}




# 2) Graphs product decisions of high-growth young  firms -------------------------------------------------------------------

# Set high growth and young firms thresholds
threshold_young<-5
threshold_growth_vector_og<-c(0.3, 0.2, 0.1, 0.05, 0.01)

# Define set of dependent vars, weights fixed effects and lag/lead structure for regressions
growth_vars<-c("empl", "nq", "rev", "tfp")
fix_eff<-c("NACE_BR + year")
n_lags_bw<-0
n_lags_fw<-0
subset<-"young"


# patent_var<-"patent"
# tm_var<-"tm"

# Initialize results dataframe
results<-data.frame(
  k=numeric(),
  factor=character(),
  n_obs=numeric()
)
growth_thresholds<-data.table(year=c(min(product_summary$year, na.rm = T):max(product_summary$year, na.rm = T), "mean"))

# Anchor the original output path
output_dir_og<-output_dir

# Set window flag: Should we run the regressions with time windows for the variables or just the one time observation?
window_flag<-T

# LaTex table parameters
options(modelsummary_factory_default = "kableExtra")  # forces classic LaTeX output
options(modelsummary_format_latex = "latex")          # ensures no tabularray

for(subset in c("young", "mature")){

  # Define possible specifications of explanatory variables
  if(subset=="young"){
    contrast_var<-"size"
    product_summary$size<-factor(product_summary$size, levels=c("small", "medium", "large"))
    xs<-list(c("net_product_creat_window*patent_window","net_product_creat_window*tm_window", "net_product_destr_window"),
             c("net_product_creat_window*total_patent_growth","net_product_creat_window*total_tm_growth", "net_product_destr_window"))
  }else{
    if(subset=="mature"){
      contrast_var<-"superstar"
      product_summary$superstar<-factor(product_summary$superstar, levels=c(F, T))
      xs<-list(c("net_product_creat_window*patent_window*superstar","net_product_creat_window*tm_window*superstar", "net_product_destr_window"),
               c("net_product_creat_window*total_patent_growth*superstar","net_product_creat_window*total_tm_growth*superstar", "net_product_destr_window"))
    }
  }
  
  for(growth_var in growth_vars){
    
    # Set paths for output
    output_dir_growth_var<-paste0(output_dir_og, growth_var, "/")
    if(!dir.exists(output_dir_growth_var)){
      output_dir_creator(output_dir_growth_var)
    }
    
    output_dir_growth_var<-paste0(output_dir_og, growth_var, "/", subset, "/")
    if(!dir.exists(output_dir_growth_var)){
      output_dir_creator(output_dir_growth_var)
    }
    
    # Set loop thresholds based on stage
    threshold_growth_vector <- if (subset == "young") threshold_growth_vector_og else threshold_growth_vector_og[1]
    
    for(threshold_growth in threshold_growth_vector){
      output_dir_growth_var<-paste0(output_dir_og, growth_var, "/", subset, "/", threshold_growth, "/")
      if(!dir.exists(output_dir_growth_var)){
        output_dir_creator(output_dir_growth_var)
      }
      
      # Set dependent variables
      ys<-c("top_growth", paste0(growth_var, "_growth"))
      
      for(y in ys){
        
        # Set paths for output
        output_dir<-paste0(output_dir_growth_var, y, "/")
        if(!dir.exists(output_dir)){
          output_dir_creator(output_dir)
        }
        print(output_dir)
        
        # # Set patenting and trademark variables
        # patent_var_temp<-patent_var
        # tm_var_temp<-tm_var
        
        # Keep only young firms for growth regressions
        # Top growth regressions do not need this because top_growth is NA for mature firms
        # Top growth regressions need mature firm observations to create lead and lag observations
        if(y==paste0(growth_var, "_growth")){
          if(subset=="young"){
            product_summary_temp<-product_summary[firm_age<=threshold_young]
          }else{
            if(subset=="mature"){
              product_summary_temp<-product_summary[firm_age>threshold_young]
            }else{
              if(subset=="all"){
                product_summary_temp<-product_summary
              }else{
                stop("subset not well-defined")
              }
            }
          }
        }else{
          product_summary_temp<-product_summary
        }
        
        for (digit in c("all")) {
          
          # growth_var<-"nq"
          # digit<-"all"
          # Initialize results dataframe
          results<-data.frame(
            k=numeric(),
            factor=character(),
            n_obs=numeric()
          )
          
          # Drop top_growth from product_summary_temp in case it already exists
          # if("top_growth" %in% names(product_summary_temp)){
          #   product_summary$top_growth<-NULL
          # }
          
          # Keep only incumbent (drop entry and exit) and young firms
          setDT(product_summary_temp)
          continuing_firms <- product_summary_temp[abs(empl_growth)!=2 & abs(nq_growth)!=2]
          
          if(subset=="young"){
            continuing_firms<-continuing_firms[firm_age<=threshold_young]
          }else{
            if(subset=="mature"){
              continuing_firms<-continuing_firms[firm_age>threshold_young]
            }else{
              if(subset=="all"){
                continuing_firms<-continuing_firms
              }else{
                stop("subset not well-defined")
              }
            }
          }
          
          # Identify high growth firms
          continuing_firms[, top_growth:=fifelse(get(paste0(growth_var, "_growth"))>=quantile(get(paste0(growth_var, "_growth")), probs=1-threshold_growth, na.rm=T),1,0), by=.(year)]
          
          # Create a dataset with the top_growth_thresholds
          growth_thresholds_temp <- continuing_firms[, (paste0(growth_var, "_thresh_", threshold_growth)):=quantile(get(paste0(growth_var, "_growth")), probs=1-threshold_growth, na.rm=T), by=.(year)]
          thresh_name <- paste0(growth_var, "_thresh_", threshold_growth)
          growth_thresholds_temp <- continuing_firms[, 
                                                     setNames(
                                                       list(quantile(get(paste0(growth_var, "_growth")), probs = 1 - threshold_growth, na.rm = TRUE)),
                                                       thresh_name
                                                     ),
                                                     by = .(year)
          ]
          growth_thresholds_temp <- rbind(growth_thresholds_temp, as.list(c("mean", mean(growth_thresholds_temp[[2]], na.rm = TRUE))), fill = TRUE)
          growth_thresholds<-merge(growth_thresholds, growth_thresholds_temp, by="year", all.x = T)
          
          
          # continuing_firms[, top_growth:=fifelse(frank(get(paste0(growth_var, "_growth")), ties.method="min")/.N>(1-threshold_growth) ,1,0), by=.(year)]
          # summary(continuing_firms[top_growth==1, get(growth_var)])
          # Merge indicator of high growth into dataset with only firmids that are at some point young/mature in the dataset
          continuing_firms<-continuing_firms[, c("firmid", "year", "top_growth")]
          continuing_firmids<-unique(continuing_firms$firmid)
          product_summary_temp<-product_summary_temp[firmid %in% continuing_firmids]
          product_summary_temp<-merge(product_summary_temp, continuing_firms, by=c("firmid", "year"), all.x = T)
          
          
          make_summary_stats(product_summary_temp, 
                             c("firm_age", "empl_bar", "nq_bar", "empl_growth", "nq_growth", "rev_growth", "number_of_products", "prod_creat", "prod_destr", "patent", "tm"),
                             if (y=="top_growth") "top_growth" else contrast_var,
                             paste0("summary_stats_", if (y=="top_growth") "top_growth" else contrast_var, "_", paste0(growth_var, "_growth"), "_", threshold_growth))
          
          # # Set product creation and destruction variables
          # if(digit=="all"){
          #   creat_var_temp<-paste0("net_product_creat")
          #   destr_var_temp<-paste0("net_product_destr")
          # }else{
          #   creat_var_temp<-paste0("new_", digit)
          #   destr_var_temp<-paste0("exit_", digit)
          # }
          
          ##'CAUTION! THIS APPROACH IS FLAWED: WHILE na_rm=T removes nas, we have already removed firm entry and exit. Therefore, windows for 
          ##'product creation and destruction that should be NAs will not be classified as such for years close to firm entry. For instance,
          ##'2010 year observations will be 0 or 1, but they should be NAs, since we don't know firm's product churning decision in 2009.
          
          # if(window_flag & !grepl("window", creat_var_temp)){
          #   # Create time windows for the product creation and destruction vars (patent and tm windows have previously been created in section 1)
          #   # Remember that our product data is left censored, so time windows that go to years before our first data point should be NAs (na_rm=F, )
          #   window_var_cretor(product_summary_temp, "firmid", "year", creat_var_temp, window_length, paste0(creat_var_temp, "_window"), na_rm=F)
          #   window_var_cretor(product_summary_temp, "firmid", "year", destr_var_temp, window_length, paste0(destr_var_temp, "_window"), na_rm=F)
          # 
          #   # Update relevant vars
          #   vars<-c("creat_var_temp", "destr_var_temp", "patent_var_temp", "tm_var_temp")
          #   for(var in vars){
          #     print(var)
          #     assign(var, paste0(get(var), "_window"))
          #   }
          # }
          
          # # Define possible specifications of explanatory variables
          # xs<-list(c(creat_var_temp, destr_var_temp, patent_var_temp, tm_var_temp),
          #          # c(creat_var_temp, destr_var_temp, tm_var_temp),
          #          # c(patent_var_temp, destr_var_temp, tm_var_temp),
          #          c(paste0(creat_var_temp, "*", patent_var_temp), paste0(creat_var_temp, "*", tm_var_temp), destr_var_temp))
          
          # Initialize list to store results of static regressions
          static_results<-data.table()
          
          for(i in 1:length(xs)){
            # i<-2
            x<-xs[[i]]
            
            # Add control
            x<-c(x, "log_n_products", "log_emp_rnd")
            
            # test2<-na.omit(product_summary_temp, cols=c("net_product_creat_window", "net_product_destr_window", "patent_window", "tm_window", "log_n_products"))
            # test<-test[, c("firmid", "year", "net_product_creat_window", "net_product_destr_window", "patent_window", "tm_window", "log_n_products")]
            # test2<-test2[, c("firmid", "year", "net_product_creat_window", "net_product_destr_window", "patent_window", "tm_window", "log_n_products")]
            
            # COMPUTE DYNAMIC RESULTS
            
            # # Data for dynamic graphs
            # results_temp<-dynamic_reg_reallocation(data= product_summary_temp,
            #                                        y=y,
            #                                        x=x,
            #                                        fix_eff = fix_eff,
            #                                        weight_var = paste0(growth_var, "_bar"),
            #                                        disag_var = "size",
            #                                        # digits=digits,
            #                                        n_lags_bw=n_lags_bw,
            #                                        n_lags_fw=n_lags_fw)
            # 
            # # Add digit variable to dynamic results
            # results_temp[,`:=`(digit=digit,
            #                    x=paste0(y, " = ", paste(x, collapse ="+")))]
            # 
            # # Fill out dynamic results data table
            # results<-rbind(results, results_temp, fill=T)
            
            # COMPUTE STATIC RESULTS
            
            # Results of regressions for table format
            static_results_temp<-static_reg_reallocation(data= product_summary_temp, 
                                                         y=y,
                                                         x=x, 
                                                         fix_eff = fix_eff, 
                                                         weight_var = paste0(growth_var, "_bar"),
                                                         disag_var = "size",
                                                         k=0,
                                                         reg_name=i, 
                                                         mfx_cond=T)
            
            # Append static results
            static_results<-rbind(static_results, static_results_temp)
            
          }
          
          setDT(static_results)
          
          variables<-unique(static_results$variable)
          filter<-unique(static_results$filter)
          reg_name<-unique(static_results$reg_name)
          
          # Initialize list elements that will be populated in the following loop
          static_results_list<-list()
          regressions_per_filter_fe<-list()
          
          
          # EXPORTS FOR STATIC REGRESSIONS (TEX FILES)
          
          # Export tex file with the static regression results
          for(var in variables){
            for(f in fix_eff){
              for(i in c("variables")){
                for(fil in get(i)){
                  
                  # Keep only obs of a given category and fixed effect
                  static_results_temp<-static_results[variable==var & fe==f]
                  
                  if(i!="variables"){
                    static_results_temp<-static_results_temp[get(i)==fil]
                  }
                  
                  # Keep the list of the feols elements
                  regressions_per_filter_fe<-static_results_temp$regression
                  
                  # Define table titles
                  title<-toTitleCase(paste0("Regressions of ", 
                                            gsub("_", " ", y),
                                            if(y=="top_growth") paste0(" (top ", threshold_growth, ")"),
                                            " on product and innovation variables - ",
                                            subset, " sample"))
                  
                  # Final adjustments to formatting
                  if(i !="filter"){
                    # Adjust the names of the columns that will appear on top of the tables
                    names(regressions_per_filter_fe)<-static_results_temp$filter
                  }else{
                    title<-toTitleCase(paste0(title, " for ", var, "=", fil ))
                  }
                  

                  # Export results
                  modelsummary(
                    regressions_per_filter_fe,
                    output = paste0(output_dir, "regressions_", y, "_", fil, "_", f, ".tex"), 
                    label = paste0(growth_var, "_", subset, "_", threshold_growth, "_", "regressions_", y, "_", fil, "_", f),
                    stars = TRUE, 
                    gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                    title = title,
                    table = "\\begin{table}[H]\n\\centering\n%s\n\\end{table}"
                    )
                  print((paste0("Exported regressions for ",  var, " ", fil, " ", f)))
                  
                }
              }
            }
          }
          
          # EXPORTS OF MFX TABLES
          if(subset=="mature"){
            mfx_regressions<-static_results[filter=="all"]
            
            for(i in 1:nrow(mfx_regressions)){
              # Get necessary information for export name and content
              # i<-1
              table_temp<-mfx_regressions[i]$mfx
              fe<-mfx_regressions[i]$fe
              reg_name<-mfx_regressions[i]$reg_name
              table_temp<-table_temp[[1]]
              
              # Export mfx table
              sink(paste0(output_dir, "reg_", reg_name, "_", fe, ".tex"))
              
              
              print(xtable(table_temp, 
                           caption=toTitleCase(paste0("Regressions of ", gsub("_", " ", y), " on product and innovation variables by ", contrast_var)), 
                           label=paste0("reg_", reg_name, "_", fe),
                           type = "latex"),
                    include.rownames=F, caption.placement="top")
              sink()
            }
          }
          
          
          ## EXPORTS FOR DYNAMIC REGRESSIONS (GRAPHS)
          
          # # Remove n products variable
          # results<-results %>% select(-log_n_products, -chigh_log_n_products, -clow_log_n_products)
          # 
          # # Create graphs using results table
          # dynamic_reg_graphs(results=results,
          #                    fix_eff = fix_eff,
          #                    output_dir = output_dir,
          #                    graph_name =paste0(y, "_young_firms_fe") )
          
        }
        
      }
      
    }
    
    
  }
  
}








