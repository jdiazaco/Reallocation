# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

#1) compare PRODCOM and BR samples  ---------------------------------------------
## import data and neaten var names 
  comparison_dataset = readRDS('combined_sbs_br_prodcom_data.rds')
  comparison_dataset= comparison_dataset %>%
    rename(revenue = nq, employees = empl, 'high tech' = high_tech,
           'full sample' = full_sample, 'prodcom only' = in_prodcom, 
           'other sectors' = other_sector, 'low tech' = low_tech)

  ## generate summary stats
  interest_vars =  c('employees', 'revenue','age', 'young','high tech',
                   'manufacturing', 'services')

# import function to generate summary stat csv and latex files 
source('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/3.1 summary stats helper.R')

# compare full sample and prodcom sample
sub_groups = c('full sample', 'prodcom only')
make_summary_stats(comparison_dataset, interest_vars, "prodcom only", 'prodcom_comparison')

# compare manufacturing vs. services vs. other 
sub_groups_manufac = c('manufacturing', 'services', 'other sectors')
interest_vars_manufac = interest_vars[! interest_vars %in% c('manufacturing', 'services')]
make_summary_stats(comparison_dataset, interest_vars_manufac, sub_groups_manufac,
                   'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/summary stats/manufac_vs_services')

# compare high tech vs. low tech 
sub_groups_manufac = c('low tech', 'high tech')
interest_vars_manufac = interest_vars[! interest_vars %in% 'high tech']
make_summary_stats(comparison_dataset, interest_vars_manufac, sub_groups_manufac,
                   'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/summary stats/high_vs_low_tech')

## generate industry coverage rate
comparison_dataset[, `:=`(matched_rev = revenue*`prodcom only`, industry_rev = sum(revenue)), by = industry]
industry_comps = comparison_dataset[,.(obs_matched = round(sum(`prodcom only`,na.rm = T)/.N, 3),
                                       rev_matched =round(sum(revenue*`prodcom only`, na.rm = T)/sum(revenue, na.rm = T),3)), by = industry]
industry_comps= industry_comps[!is.na(industry)]
setorder(industry_comps, -rev_matched)
saveRDS(industry_comps, 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/summary stats/prodcom_industry_coverage.rds')

## generate change in sectoral composition over time 
  comparison_dataset = readRDS('combined_sbs_br_prodcom_data.rds')
  comparison_dataset[, `:=`(sector_name = ifelse(is.na(NACE_BR), NA,
                                                 ifelse(manufacturing, 'manufacturing',
                                                        ifelse(services, 'services', 'other sectors'))))]
  sectoral_decomp = comparison_dataset[!is.na(NACE_BR)]
  sectoral_decomp =  sectoral_decomp[, .(firms = .N,
                                           rev_share = sum(nq, na.rm =T),
                                           empl_share = sum(empl, na.rm =T)), by = .(year,sector_name)]
  
  sectoral_decomp =  sectoral_decomp[, `:=`(firm_share = firms/sum(firms),
                                         rev_share = rev_share/sum(rev_share),
                                         empl_share = empl_share/ sum(empl_share)), by = .(year)] %>%select(-firms)
  sectoral_decomp[,3:5] = round(sectoral_decomp[,3:5],2)
  ylim = c(0, max(sectoral_decomp[,3:5]))
  
  variables = c('firm_share', 'rev_share', 'empl_share')
  titles = c('Firm Share', 'Revenue Share', 'Labor Share')
  sector_graphs = lapply(seq_along(variables),function(i){
  ouput = ggplot(sectoral_decomp, aes(x = year, y = get(variables[i]), color = sector_name)) + geom_line() +
    scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) +
    labs(color = element_blank(), title = titles[i]) +theme(axis.title = element_blank())
  })
  output = ((sector_graphs[[1]] +theme(legend.position = 'none')) +
    (sector_graphs[[2]] + theme(legend.position = 'none', axis.text.y = element_blank(), axis.ticks.y = element_blank())) +
      (sector_graphs[[3]] + theme(legend.position = 'right',axis.text.y = element_blank(), axis.ticks.y = element_blank()))) +
    plot_annotation(title = 'Trends in French Sectoral Composition') 
  ggsave(output = 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/trends in french sector composition.png')
  
  
#2)firm level decompositions -------------------------------------------------------------
  # 2.1 calculate decompositions  -----------------------------------------------
  ## import data 
  
  # Export firm-level product data
  agg_product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet"))
  firm_yr_lvl_dta <- read_parquet('1_firm_yr_lvl_br_dta.parquet')
  # firm_yr_lvl_dta <- fread('1_temp.csv')
  

  ## carry out the decompositions  
    vars = c('rev', 'entrance', 'exit', 'empl') 
    data_frames = c(rep('agg_product_data',3), 'firm_yr_lvl_dta')
    dynamics <- c("growth", "reallocation")
    end = 2022
    for (i in 1:4){
      for(dyn in dynamics){
        ## define vars / import data 
        g = paste0(vars[i], '_', dyn); gweighted = paste0(g, '_weighted'); share= paste0(vars[i], '_share')
        g_l = paste0(g, '_l'); gweighted_l = paste0(gweighted, '_l');share_l =  paste0(share, '_l')
        shift_vars = c(g, share, gweighted)
        lag_vars = paste0(shift_vars, '_l')
        
        firm_data = get(data_frames[i])
        setorder(firm_data,firmid, year)
        firm_data = firm_data %>% select(firmid, year, consolidated_birth_year, economic_death_year, all_of(shift_vars))
        
        #define aggregates /net entry portion of decomp 
        aggregates = as.data.table(firm_data[, .(agg = sum(get(gweighted), na.rm = T)), by = year] %>% arrange(year))
        aggregates[, `:=`(agg_l = shift(agg), agg_delta = agg- shift(agg))]
        
        ## generate lagged vars
        firm_data_l = firm_data[year<end] %>% mutate(year = year + 1) %>% select(shift_vars, everything())
        colnames(firm_data_l)[names(firm_data_l) %in% shift_vars] = lag_vars
        firm_data = merge(firm_data, firm_data_l, by=c("firmid", "year", "consolidated_birth_year", "economic_death_year"), all = T)
        
        ## redefine birth/ death/ survivor 
        firm_data[, `:=`(born = !is.na(consolidated_birth_year) & consolidated_birth_year == year,
                         died = !is.na(economic_death_year) & economic_death_year < year)]
        firm_data[, survivor := !born & ! died]
        
        ## set missing data to zero 
        for (j in 1:length(shift_vars)){
          firm_data[is.na(get(shift_vars[j])), shift_vars[j] := 0]
          firm_data[is.na(get(lag_vars[j])), lag_vars[j] := 0]
        } 
        
        decompositions = firm_data[,.(
          survivor = sum(survivor*(get(gweighted)- get(gweighted_l))),
          within = sum(survivor*get(share_l)*(get(g)-get(g_l))),
          between = sum(survivor*(get(share)- get(share_l))*get(g_l)),
          cross = sum(survivor*(get(share) - get(share_l))*(get(g)-get(g_l))),
          entrance = sum(born*(get(gweighted)- get(gweighted_l))),
          exit = sum(died*(get(gweighted)- get(gweighted_l))),
          'entry/exit' = sum(born*(get(gweighted)- get(gweighted_l))) +
            sum(died*(get(gweighted)- get(gweighted_l)))),
          by =year]
        decompositions= merge(aggregates,decompositions)
        
        ## check that the decompositions were carried out correctly 
        decompositions[,`:=`(check_1 = survivor-within-between-cross,
                             check_2 = agg_delta -(survivor + entrance + exit))]
        if (max(abs(decompositions$check_2), na.rm = T) > 1E-6|
            max(abs(decompositions$check_1), na.rm = T) > 1E-6){
          
          check <- merge(firm_data_og, firm_data, by=c("firmid", "year", "consolidated_birth_year", "economic_death_year"), all.x=T)
          check[, check:=empl_growth_weighted.x==empl_growth_weighted.y]
          table(check$check)
          View(check[check==F])
          
          
          stop(paste0(vars[i], " not correct"))
        }
        print(paste0(vars[i], " correct"))
        decompositions[,c('check_1', 'check_2','agg_l') := NULL]
        
        ## for extensive margin add together entrance and exit; reshape and export  
        sub_groups = colnames(decompositions)[2:ncol(decompositions)]
        if (vars[i]=='entrance'){
          entrance = decompositions 
        }else if(vars[i]== 'exit'){
          exit = decompositions
          products = merge(entrance, exit, by = 'year')
          for (j in seq_along(sub_groups)){
            x = paste0(sub_groups[j], '.x')
            y = paste0(sub_groups[j], '.y')
            products[, sub_groups[j] := get(x)+ get(y)]
            products[,c(x,y):=NULL]
          }
          products_long = melt(products, id.vars = 'year', measure.vars = sub_groups) %>%
            rename(sub_group = variable)
          saveRDS(products_long,paste0(output_dir, '/firm_level_prod_', dyn, '.rds'))
        } else{
          decompositions_long = melt(decompositions, id.vars = 'year', measure.vars = sub_groups) %>%
            rename(sub_group = variable)
          saveRDS(decompositions_long, paste0(output_dir, 'firm_level_',vars[i],'_', dyn, '.rds'))
        }
      }
    }
  
  # 2.2 graph decompositions ----------------------------------------------------
    #combine output and additional cleaning 
    for(dyn in dynamics){
      vars = c('empl', 'rev','prod')
      realloc_results = rbindlist(lapply(seq_along(vars),function(i){
        as.data.table(readRDS(paste0(output_dir, 'firm_level_', vars[i],'_', dyn, '.rds'))) %>%
          mutate(var= vars[i])
      }))
      realloc_results[var != 'empl' & (year == 2010 & sub_group !='agg' | year == 2009), value:= NA]
      realloc_results[var == 'empl' & (year == 1995 & sub_group !='agg' | year == 1994), value:= NA]
      
      titles = c(paste0('Decomposing $\\Delta$ Labor ', tools::toTitleCase(dyn), ' Rate') ,
                 paste0('Decomposing $\\Delta$ Product ', tools::toTitleCase(dyn), ' Rate'), 
                 paste0('Decomposing $\\Delta$ Product ', tools::toTitleCase(dyn), ' Rate'))
      subtitles= c('', '(intensive margin)', '(extensive margin)')
      
      #make each graph 
      for (i in seq_along(vars)){
        data = realloc_results[var == vars[i]]
        ylim = c(min(data$value,na.rm = T)-.01, max(data$value, na.rm = T)+.01)*100
        ybreaks = seq(floor(ylim[1]), ceiling(ylim[2]), length.out = 4)
        ybreaks = sort(c(ybreaks[abs(ybreaks)>5],0))
        
        assign(paste0(vars[i],'_delta_decomp'), ggplot(data[sub_group %in% c('agg', 'within', 'between', 'cross','entry/exit')],
                                                       aes(x =year, y = value*100, color = sub_group, size = sub_group)) +
                 geom_line() +
                 scale_size_manual(values = c(1,rep(.5,4)))+
                 scale_x_continuous(limits = c(min(data$year)+1,max(data$year)), breaks = scales::breaks_pretty(5))+
                 scale_y_continuous(labels = scales::label_percent(scale = 1), limits = ylim, breaks= ybreaks) +
                 theme_classic() +theme(legend.position = 'bottom', axis.title.x = element_blank(),
                                        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
                                        axis.line.x = element_blank(), legend.title = element_blank()) + 
                 labs(title =  TeX(titles[i]), subtitle = subtitles[i])+ guides(size = 'none') +
                 geom_hline(yintercept = 0, linetype = 'dashed') +
                 scale_color_hue(labels = c('agg' = 'Reallocation Rate          Decompostion:',
                                            'within'= 'within', 'between' = 'between', 
                                            'cross' = 'cross', 'entry/exit' = 'entry/exit'))
        )
        ggsave(paste0(output_dir, dyn, '_', vars[i],'_delta_decomp.png'), get(paste0(vars[i],'_delta_decomp')), height=5, width=9)
      }
    }
    
#3) decomposition of growth by product status (new, existing, etc) -------------------------
## import product data 
product_data = readRDS('product_level_reallocation.rds')

## generate weighted growth metric 
product_data[, rev_growth_weighted := rev_growth * within_economy_rev_share]
product_data[, agg := sum(rev_growth_weighted), by = year]

## carry out decomposition 
sub_groups = c('first_introduction', 'reintroduced', 'discontinued', 'incumbent')
sub_groups_g = paste0(sub_groups, '_growth')
for (i in seq_along(sub_groups)){
  product_data[, sub_groups_g[i]:= sum(rev_growth_weighted * get(sub_groups[i])), by = year]
}
growth_decomposition = unique(product_data %>% select(year,agg,sub_groups_g))

## check decomposition results 
growth_decomposition[, check := agg- rowSums(.SD, na.rm = T), .SDcols = sub_groups_g]
if(max(abs(growth_decomposition$check))>1e-12){
  stop('decomp not correct')
}
growth_decomposition[, check:=NULL]

## output decomposition results 
colnames(growth_decomposition)[3:6] = sub_groups
growth_decomp_long = melt(growth_decomposition, id.vars = 'year', measure.vars = c('agg',sub_groups))
colnames(growth_decomp_long) = c('year', 'subset', 'growth_rate')
saveRDS(growth_decomp_long, 'decompositions/growth_rate_decomp.rds')


#4) product-HHI analysis ----------------------------------------------------------
product_data = readRDS('product_level_reallocation.rds')
HHI_graph = product_data[, .(HHI = mean(HHI, na.rm = T), rev = sum(rev, na.rm = T)), by =.(firmid, year)]
HHI_graph = HHI_graph[, .('unweighted' = mean(HHI, na.rm = T), 'rev. weighted' = sum(HHI*rev/sum(rev,na.rm = T),na.rm= T)), by = year]
HHI_graph = melt(HHI_graph, id.vars = 'year', measure.vars = c('unweighted',  'rev. weighted')) %>% rename(type = variable) 

ggplot(HHI_graph, aes(x = year, y = value*100, color = type)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
  labs(title = "Within Firm Product HHI", x = element_blank(),  y = "HHI value", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/HHI_over_time.png', height = 5.33, width = 9)

#5) emergence of superstar firms  -------------------------------------------
end = 2020
age_cutoff = 3

## identify number and share of firms hat reach supserstar cutoff within age cutoff 
sbs_data = readRDS('sbs_br_combined_cleaned.rds', select = c('firmid', 'year', 'birth_year', 'nq', 'NACE_BR'))
sbs_data[, market_share := nq/sum(nq, na.rm = T), by = .(year, NACE_BR)]

superstar_cutoffs = c(.001, .005, .01)
temp = sbs_data[year - birth_year <= age_cutoff & birth_year <= end-age_cutoff]
superstar_analysis=rbindlist(lapply(seq_along(superstar_cutoffs), function(i){
  temp = temp[, .(superstar = max(market_share)>superstar_cutoffs[i]), by = .(firmid, birth_year)]
  temp = temp[, .(count = sum(superstar, na.rm = T), share = sum(superstar, na.rm = T)/.N), by = birth_year]
  temp = temp %>% mutate('Market Share Req' = paste0(superstar_cutoffs[i]*100,'%')) %>% rename(cohort = birth_year)
}))
superstar_analysis = superstar_analysis[cohort>1995]

## graph results 
number= ggplot(superstar_analysis, aes(x =cohort, y= count, color = `Market Share Req`)) + geom_line() +
  scale_y_continuous(limits = c(0, max(superstar_analysis$count))) +
  theme(legend.position = 'bottom',axis.title.y = element_blank()) + labs(title = 'Number of young superstar firms')

share = ggplot(superstar_analysis, aes(x =cohort, y= share, color = `Market Share Req`)) + geom_line() +
  scale_y_continuous(limits = c(0, max(superstar_analysis$share))) +
  theme(legend.position = 'none',axis.title.y = element_blank()) + labs(title = 'Share of Cohort')

superstar_combined = number + share
superstar_combined+ plot_annotation(title = 'Declines in Young Superstar Firms')
ggsave('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/superstar_growth_overtime.png', height = 5.33, width = 9)



#6) plot reallocation rate comparisons across types of firms  ------------
## import the data 
combined_data =  readRDS('combined_sbs_br_prodcom_data.rds')
## prepare data for graphing -----------------------------------------------
    division_vars = c('full_sample','in_prodcom', 'sector_labor_bucket','high_tech', 'superstar','young')
    #division_vars = c('sector_labor_bucket')
    interest_vars = c('empl','rev', 'exit', 'entrance')
    
    # allow for faster testing 
      test = F
      if(test){
        set.seed(123)
        combined_data_test= combined_data[sample(nrow(combined_data), ceiling(.01*nrow(combined_data))),]
      }


    ## prepare data for graphing 
    setorder(combined_data,firmid, year)
    reallocation_output = rbindlist(lapply(seq_along(division_vars), function(i){
      division = division_vars[i]
      if (test){temp_og = combined_data_test[!is.na(get(division))]
      }else{ temp_og = combined_data[!is.na(get(division))]}
      output= rbindlist(lapply(seq_along(interest_vars), function(j){
        ##define vars for this iteration
        var= interest_vars[j]; growth = paste0(var,'_growth'); share = paste0(var,'_share')
        temp = temp_og[!is.na(get(growth)) & !is.na(get(share))]
        print(paste0('division: ', division, '; var: ', var))
        ## carry out the decomposition --> what portion of reallocation comes from each category
        temp[, share := get(share)/sum(get(share), na.rm = T), by = year]
        temp[, growth_weighted := share*get(growth)]
        decomposition = temp[, .(sum(growth_weighted, na.rm = T),
                                 version = 'decomp'), by = .(get(division), year)]
        
        ## carry out comparison --> how do the reallocation rates compare between categories 
        temp[, share := NULL]
        temp[, share := get(share)/sum(get(share), na.rm = T), by = .(get(division), year)]
        temp[, growth_weighted := share*get(growth)]
        
        comparison  = temp[, .(sum(growth_weighted, na.rm = T),
                               version = 'comparison'), by = .(get(division), year)]
        #reset share variable and combine two results 
        temp[,share := NULL]
        output = rbind(comparison, decomposition) %>% mutate(division = division, var = var) %>%
          rename(reallocation_rate = V1, division_cat = get)
      }))
    })) 
    
    ### cleanup output 
    reallocation_output = reallocation_output[!(division == 'full_sample' & version =='decomp')]
    ### only include analysis of in prodcom for employment variable, and when we have prodcom data 
    reallocation_output = reallocation_output[division != 'in_prodcom' | (var == 'empl' & year >2008)]
    
    ### restrict prodcom reallocation metrics to 2010 +, sbs metrics to '95 + 
    reallocation_output = reallocation_output[(var=='empl' & year>=1995) | (var!='empl' & year >= 2010)]
    
    ### restrict young analysis to after 1998
    reallocation_output = reallocation_output[division != 'young' | year > 1998]
    
    ### handle extensive margin 
    reallocation_output= rbind(reallocation_output[var!='entrance' & var!='exit'],
                               dcast(reallocation_output[var=='entrance' | var == 'exit'],
                                     division_cat + year + version + division ~ var,
                                     value.var = 'reallocation_rate') %>%
                                 mutate(reallocation_rate = entrance + exit, var = 'product') %>% 
                                 select(-c(entrance, exit)))


  ## plot graphs  -------------------------------------------------
    ### baseline ----------------------------------------------------------------
     labor = reallocation_output[var == 'empl' & (division =='full_sample'|
                                (division=='in_prodcom' & division_cat ==T & version =='comparison'))]
     product= reallocation_output[(var =='rev' | var =='product') & division == 'full_sample']
    
     lim = c(0, max(labor$reallocation_rate, product$reallocation_rate, na.rm= T))
    labor_graph =ggplot(labor, aes(x =year , y = reallocation_rate, color = division)) + geom_line() + 
        theme(legend.position = 'bottom', axis.title.x = element_blank()) + labs(color = "Sample", y= 'reallocation rate', title ='Employee-Level') + 
        scale_color_hue(labels = c('full_sample'='FARE/FICUS','in_prodcom' = 'Prodcom')) +  
        scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) 
        
    product_graph = ggplot(product, aes(x =year , y = reallocation_rate, color = var)) + geom_line() + 
        theme(legend.position = 'bottom', axis.title.x = element_blank()) + labs(color = "Margin", y= element_blank(), title ='Product-Level') + 
        scale_color_hue(labels = c('product'='extensive','rev' = 'intensive')) +  
        scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) 
      
      graph=labor_graph + product_graph +plot_annotation(title = 'French Reallocation Rates (1995-2020)')
      ggsave('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/baseline_reallocation.png', get(graph),height = 5.33, width = 9)
      rm(labor_graph, product_graph)
    
    ## plot sector x labor_bucket -------------------------------------------------
    manufacturing_graph = reallocation_output[division =='sector_labor_bucket'] %>%
      separate(division_cat, into= c('sector', 'employees'), sep =':') %>% filter(employees!="NA") %>%
        mutate(employees = factor(employees, levels = c("<10","10-50","50-250","250+" )))
    setorder(manufacturing_graph,version,sector, employees,var, year)
   
        sectors = c('manufacturing', 'services', 'other sector')
        graph_vars = c('empl', 'rev', 'product')
        versions = c('comparison', 'decomp'); k = 2
        plot_titles = c('Labor Reallocation Rate', 'Product Reallocation Rate (Intensive Margin)',
                        'Product Reallocation Rate (Extensive Margin)')
        subtitle = 'Decomposing By Sector and Firm Size'
        breaks= c(4,3,3)
  
    for(i in seq_along(graph_vars)){
      y_lim = c(0, max(manufacturing_graph[version == versions[k] & var == graph_vars[i]] %>%pull(reallocation_rate) ))
    for (j in seq_along(sectors)){
    
   assign(paste0(sectors[j],'_g'),ggplot(manufacturing_graph[version == versions[k] & sector == sectors[j]  & var == graph_vars[i]],
                     aes(x = year, y = reallocation_rate, color = employees)) + geom_line() +
      scale_color_brewer(palette = 'RdYlGn') +labs(title = sectors[j], y = 'reallocation rate') +
        theme(title = element_text(size =  9)) + scale_x_continuous(breaks = scales::pretty_breaks(n =breaks[i]))+
        scale_y_continuous(labels = scales::label_percent(scale = 100), limits = y_lim)
   )
    }
      assign(paste0('graph_', graph_vars[i]),
             ((manufacturing_g +theme(axis.title.x  = element_blank(), legend.position = 'none')) |
                (services_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
                (`other sector_g` + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'right'))) +
               plot_annotation(title = plot_titles[i], subtitle = subtitle))
    ggsave(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/manufac_decomp_',graph_vars[i],'.png'),
           get(paste0('graph_', graph_vars[i])), width=10, height = 5)
      }


    ## make young, high tech, superstar graphs --------------------------------------------------
    division_titles = c('Changes in Reallocation by Technology Level',
                        'Changes in Reallocation by Superstar status',
                        'Changes in Reallocation by Firm Age')
      legend_title = c('', 'market share', '')
    division_vars = c('high_tech', 'superstar','young')
    
    graph_vars = c('empl', 'rev', 'product')
    titles = c('Employment', 'Product (intensive)', "Product (extensive)")
    
    versions = c('comparison', 'decomp')
    axis_titles = c('reallocation rate', 'weighted reallocation rate')
    x_lim_list = list(ceiling(seq(1995,2020, length.out = 4))
                      ,ceiling(seq(2010,2020, length.out = 4)), ceiling(seq(2010,2020, length.out = 4)))
    
    reallocation_output[division== 'high_tech', division_cat_new :=ifelse(division_cat== "TRUE", "high tech", 'low tech')]
    reallocation_output[division== 'superstar', division_cat_new :=ifelse(division_cat== "TRUE", "> 1%", '< 1 %')]
    reallocation_output[division== 'young', division_cat_new :=ifelse(division_cat== "TRUE", "young (< 5 years)", 'old')]
    
    for( h in seq_along(division_vars)){
      for(j in seq_along(versions)){
        y_lim = c(0, max(reallocation_output[version == versions[j] & division == division_vars[h]] %>%pull(reallocation_rate) ))
      
        for( i in seq_along(graph_vars)){
          assign(paste0(graph_vars[i], '_g'),
                 ggplot(reallocation_output[division == division_vars[h] & var == graph_vars[i] & version == versions[j]],
                        aes(x =year, y = reallocation_rate, color = division_cat_new))
                 + geom_line() +  scale_y_continuous(labels = scales::label_percent(scale = 100), limits = y_lim) +
                   labs(title = titles[i], color = legend_title[h], y = axis_titles[j]) + scale_x_continuous(breaks = x_lim_list[[i]]))
          }
            ggsave(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/',division_vars[h],'_', versions[j],'.png'),
               ((empl_g +theme(axis.title.x  = element_blank(), legend.position = 'bottom')) |
                  (rev_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
                  (product_g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none'))) +
                plot_annotation(title = division_titles[h]), height = 5.3, width = 9) 
               
      }
    }
    
  
 
    
  

  





    
