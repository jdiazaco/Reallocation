#' NACE classification harmonization
#'
#' @description
#' This function takes the results table from dynamic_reg_reallocation and the set
#' of fixed effects and creates a plot with the results. Creates a plot for each
#' fixed effect. Each plot has a row for each OLS formula and one row per
#' explanatory variable within those fomulae.
#' 
#' @param results: table with results table from dynamic_reg_reallocation;
#' @param fix_eff: set of fixed effects used as part of dynamic_reg_reallocation;
#' @param output_dir: path where graphs will be stored;
#'
#' @author Julián Díaz-Acosta, 20/02/2025.
#' @export

dynamic_reg_graphs<-function(results, fix_eff, output_dir, graph_name){
  
  results<-results %>% filter(factor!=TRUE)
  
  for (z in fix_eff) {
    
    # z<-"firmid"
    results_pre<- results[fix_eff==z]
    
    # Set the axis boundaries for graphs
    min_value<-min(unlist(results_pre[, lapply(.SD, min, na.rm=T), .SDcols = patterns("^clow")]))
    max_value<-max(unlist(results_pre[, lapply(.SD, max, na.rm=T), .SDcols = patterns("^chigh")]))
    y_lim=c(min(0, min_value), max_value)
    
    formulas<-unique(results_pre$x)
    
    for(i in 1:length(formulas)){
      
      formula<-formulas[i]
      
      # Filter only observations 
      setDT(results_temp)
      results_temp<-results_pre[x==formula]
      
      # Keep only the names of the dataset that represent coefficient point estimates
      inform_vars<-c("k", "factor", "variable", "filter", "fix_eff", "digit", "x", "nobs")
      x_names<-names(results_temp)[!grepl("^(chigh|clow)", names(results_temp))]
      x_names<-x_names[!(x_names %in% inform_vars)]
      
      plot_list<-list()
      
      for(j in 1:length(x_names)){
        # name<-x_names[1]
        name<-x_names[j]
        print(name)
        
        # Create the names of the variable names for confidence intervals
        clow_name<-paste0("clow_", name)
        chigh_name<-paste0("chigh_", name)
        
        # Create significance flag
        results_temp$significant<-(sign(results_temp[[clow_name]])==sign(results_temp[[chigh_name]])) & results_temp[[name]]!=0
        
        # Plot for product creation 
        temp<-ggplot(results_temp, aes(x=k, y=.data[[name]], color=filter, fill=filter)) +
          geom_point(aes(size=.data[["significant"]])) +
          geom_ribbon(aes(ymin=.data[[clow_name]], ymax=.data[[chigh_name]]), colour=NA, alpha=0.2) +
          geom_line()+
          geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
          geom_vline(xintercept = 0, color="black", linetype="dashed") + 
          scale_x_continuous(breaks=seq(-n_lags_bw, n_lags_fw, by=1))+
          coord_cartesian(ylim=y_lim)+
          theme_minimal() +
          theme(plot.title = element_text(hjust=0.5))+
          labs(title=paste0(name),
               fill="Category",
               color="Category",
               size="5% Significance",
               x="k",
               y=paste0("Coefficient"))
        
        plot_list[[paste0(z, "_", name)]]<-temp
        
        # Create row of plots. Keep the label of the y axis only for the most left plot. Kepp the legend box only for the most right plot
        if(j==1){
          plot_row<-(plot_list[[j]]  +  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none"))
        }else{
          if(j!=length(x_names)){
            plot_row<-plot_row+(plot_list[[j]] + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position ="none" ))
          }else{
            plot_row<-plot_row+(plot_list[[j]] + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())) + plot_layout(ncol=length(x_names))
          }
        }
      }
      
      
      # Stack rows of plots to create final plot. Group legend boxes and keep only top row names
      if(i==1){
        final_plot<-plot_row
      }else{
        if(j!=length(x_names)){
          final_plot<-final_plot/plot_row
        }else{
          #Group legend box to the left and delete all plot titles
          final_plot<-final_plot/plot_row + plot_layout(guides="collect") & theme(plot.title = element_blank())
          
          # Add plot titles only for the first row
          for(f in 1:length(final_plot[[1]])){
            final_plot[[1]][[f]] <- final_plot[[1]][[f]] + theme(plot.title = element_text())
          }
        }
      }
      
    }
    
    ggsave( paste0(output_dir, graph_name, "_", gsub("\\*|\\+|\\^", "_", z), ".png"), final_plot, height = length(formulas)*2.5, width = length(x_names)*2.5)
    
  }
  
}

